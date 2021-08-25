#' Find the best spar value for spline smoothing using dive statistics
#'
#' Function recovers a record 21 separate times [0-1] with increments of 0.05
#' and assess how each change in spar value influences the dive statistics
#' gathered by the record. It also finds the ideal spar value for that record
#' by assessing changes in bottom distance.
#'
#' @param filepath folder containing the trace, time dots, and argument files.
#' @return folder in /results that contains a csv for all spar scenarios, a
#' dive_stats data frame containing all dive stats for each spar scenario to the
#' global environment, and the best spar value to use for this record.
#' @export
#' @examples
#' \dontrun{
#' filepath <- system.file("extdata", "WS_25_1981", package = "recoverKBTDR")
#' find_best_spar(filepath)
#' }

###############################################################################
# Function: find_best_spar(filepath = "../sample_data/WS_25_1981)
# Author:   EmmaLi Tsai
# Date:     3/30/21
#
# One of the biggest challenges is finding the best smoothing penalty (spar
# value, in this case) that would effectively capture all of the dives without
# overfitting (producing ghost wiggles) or underfitting (not capturing
# inflections in the record at depth). As expected, this function takes awhile
# to run but output of these functions (i.e., dive_stats csv file,
# sample_data/WS_25_1981/WS_25_1981_dive_stats.csv) can be found in the sample
# data folder in this repository.
#
# Function takes the raw time dots, trace, psi calibration, and args files in a
# record folder and completes two main steps to find the best spar value for
# that record:
#
#   (1) - recovers the same record 21 sparate times using a different spar
#         value. The spar values range from [0-1] with increments of 0.05. This
#         section writes all files into a folder in the results folder.
#
#   (2) - reads each file into the "diveMove" package to assess how changing
#         spar value affects the bottom distance of each dive. Bottom distance
#         was chosen because it is often used as a proxy for "wiggles" and would
#         therefore be the most sensitive to changing spar values.
#
#         When plotted (bottom distance vs. spar value), the trend across all
#         records is a unimodal distribution with a negative skew. The threshold
#         spar value before bottom distance variance increases is also the spar
#         value that minimizes overall bottom distance. At this spar value, all
#         dives would be captured but the inflections in the dive at depth would
#         not be captured. As you decrease the spar value closer to 0 and
#         smoothing resolution increases, bottom distance increases as the
#         wiggles within dives are being documented. However, lower values also
#         produce "ghost" wiggles not present in the original record. Therefore,
#         I think the best approach is to take the threshold spar value and
#         divide by 2 as a compromise between overfitting and underfitting the
#         data.
#
# Input:
#
#   - filepath    : folder that contains all files for the record, including the
#                   argument file that is used for fast recovery.
#
# Output:
#
#   - dive_stats  : tidy dive_stats data frame with all of the dive stats
#                   for the different spar scenarios, complete with dive numbers
#                   and spar value. Also returns best spar value.
###############################################################################
find_best_spar <- function(filepath = "../sample_data/WS_25_1981"){
  # read in the record
  read_trace(filepath = filepath)

  # center the scan
  trace_raw <- center_scan(trace_raw, time_dots_raw,
                           center_along_y = args_raw$dist_timedot)

  # getting the centered psi calibration curve, if the record has one
  if (is.na(args_raw$max_depth)){
    psi_calibration <- .centered_psi_calibration(trace_raw)
  }


  # zoc, if needed
  if (!is.na(args_raw$k_h)){
    zoc(trace_raw,
        k_h = args_raw$k_h,
        depth_bounds = c(args_raw$depth_bounds_l, args_raw$depth_bounds_h))
  }

  # transforming x-axis to time (minutes from the start)
  trace_raw <- transform_x_vals(trace_raw, time_dots_raw,
                                center_y = args_raw$center_y,
                                time_period_min = args_raw$time_period_min)

  # dates and times with interpolated points
  trace_raw <- add_dates_times(trace_raw,
                               start_time = args_raw$date_start,
                               on_seal = args_raw$on_seal,
                               off_seal = args_raw$off_seal)

  if(!is.na(args_raw$max_depth)){
    trace_raw <- transform_y_vals(trace_raw, args_raw$max_depth)
  } else {
    trace_raw <- transform_y_vals(trace_raw,
                                  psi_calibration = args_raw$psi_calibration,
                                  max_psi = 900,
                                  max_position = 22.45)
  }

  # now creating a new results folder to store files
  base_name <- unlist(strsplit(filepath, "/"))[3]
  dir_name <- paste("../results/", base_name, sep = "")
  # checking to see if the folder is already in the directory
  if (!(dir_name %in% list.dirs("../results"))){
    dir.create(dir_name)
  }

  # spar sequence for future loop
  spar_seq <- seq(0, 1, by = 0.05)
  # start spar loop for investigating impact of different spar values on the
  # dive statistics
  for (i in 1:length(spar_seq)){
    trace_i <- smooth_trace_dive(trace_raw,
                                 spar_h = spar_seq[i],
                                 depth_thresh = args_raw$depth_bounds_smooth)

    # writing to csv file in results folder:
    results_filepath <- paste(dir_name, "/", base_name, "_", spar_seq[i],".csv", sep = "")
    write.csv(trace_i, results_filepath)
    # just printing to the console so the user can see what iteration of the
    # loop the function is on
    print(spar_seq[i])
  }
  # adding dive stats for each spar iteration
  dive_stats <- .get_divestats(folder = paste(dir_name, "/", sep = ""))
  # clean the results and add to global environment
  dive_stats <<- .clean_divestats(dive_stats)
  # return the best spar value
  return(.best_spar(dive_stats))
}

#' Get the dive statistics for each spar value iteration
#' @param filepath folder containing the trace, time dots, and argument files.
#' @return data frame of all dive statitics for each dive for all spar value
#' scenarios.
#' @importFrom diveMove readTDR calibrateDepth diveStats
#' @examples
#' \dontrun{
#' filepath <- system.file("extdata", "WS_25_1981", package = "recoverKBTDR")
#' .get_divestats(filepath)
#' }
# reads in all data for dive analysis, achieves step two of the function
# described above
.get_divestats <- function(folder = "../results/WS_25_1981/"){
  # getting record ID's in folder
  record_id <- unlist(strsplit(folder, "/"))[3]
  # listing all files in folder
  file_names <- list.files(folder, paste(record_id, "_", sep = ""))

  # empty df to store results
  dive_stats <- data.frame()

  # looping through all files
  for (i in 1:length(file_names)){
    # getting file name for iteration of the loop
    file_i_name <- paste(folder, file_names[i], sep = "")
    # reading in the file to conver to tdr object
    file_i <- read.csv(file_i_name)
    # printing so user can see iteration of that loop
    print(file_i_name)

    # creating a TDR object
    tdr <- diveMove::readTDR(file_i_name,
                             dateCol = which(names(file_i) == "date_time"),
                             depthCol = which(names(file_i) == "smooth_depth"),
                             subsamp = 2,
                             dtformat = "%Y-%m-%d %H:%M:%S")

    # calibrating TDR object
    ctdr <- diveMove::calibrateDepth(tdr,
                                     zoc.method = "offset",
                                     offset = 10,
                                     dive.thr = 10)

    # getting the divestats
    dive_stats_i <- diveStats(ctdr)
    # getting filename value as unique identifier
    dive_stats_i$spar <- file_names[i]
    # binding it to data frame
    dive_stats <- rbind(dive_stats, dive_stats_i)
  }
  # return the dive_stats
  return(dive_stats)
}

#' Tidy dive stats
#' @param dive_stats data frame containing dive stats from diveMove package
#' @return tidy data frame with dive numbers and better string management
#' @importFrom dplyr case_when
#' @examples
#' \dontrun{
#' .clean_divestats(dive_stats)
#' }
#'
# function for tidying the dive_stats data. This includes adding dive numbers,
# providing better names, and also creating a data frame of the best spar value
# to use for that record.
.clean_divestats <- function(dive_stats){

  # tidying column names
  names <- sub('\\.csv$', '', basename(as.character(dive_stats$spar)))
  sub_names <- substring(names, regexpr("([^_]+)(?:_[^_]+){0}$", names))
  dive_stats$spar_val <- as.numeric(sub_names)

  # adding dive numbers
  # first, creating a helper data frame to identify when spar value changes
  helper <- data.frame(spar = dive_stats$spar, lag_spar = lag(dive_stats$spar))
  helper$diveNo <- dplyr::case_when(helper$spar == helper$lag_spar ~ 1,
                                    helper$spar != helper$lag_spar ~ 0)
  # fixing for special case, due to lag function
  helper$diveNo[1] <- 1
  # calling a helper function
  diveNo <- .fun(helper)
  # adding to original dive stats file
  dive_stats_num <- cbind(dive_stats, diveNo)
  # creating small correction for when the function starts counting at 0 rather
  # than one
  dive_stats_num$correct <- dplyr::case_when(dive_stats_num$spar_val !='0.05'~ as.double((dive_stats_num$diveNo + 1)),
                                             dive_stats_num$spar_val == '0.05' ~ as.double(dive_stats_num$diveNo))
  # adding to dive stats data frame
  dive_stats$diveNo <- dive_stats_num$correct

  # returning the clean dive_stats data frame
  return(dive_stats)
}

#' Internal function for assigning dive numbers
#' @param x helper data frame for assigning dive numbers
#' @return vector of dive numbers
#' @examples
#' \dontrun{
#' .fun(helper_data_frame)
#' }
.fun <- function(x) {
  test <- x$diveNo > 0
  y <- cumsum(test)
  c(y - cummax(y * !test))
}


#' Find the best spar value
#' @param dive_stats dive statistics data frame after tidying
#' @return a data frame containing the spar value that minimized overall bottom
#' distance and the best spar value for that record (i.e., spar value with
#' minimum average bottom distance / 2)
#' @importFrom dplyr select group_by summarize
#' @examples
#' \dontrun{
#' .best_spar(dive_stats)
#' }
# function that calculates the spar value that minimizes average bottom distance
# and divides this spar value by two to identify the ideal spar value for that
# record
.best_spar <- function(dive_stats){

  # finding spar value that minimizes bottom distance:
  bot_all <- dplyr::select(dive_stats, c("bottdist", "spar_val"))
  # spar val that gives minimum bottom distance for each WS_id
  mean_bt <- dplyr::group_by(bot_all, spar_val) %>% dplyr::summarize(mean_bott = mean(.data$bottdist))
  # finding the spar value that minimizes mean bottom distance, this will give the
  # lower end of the unimodal distribution
  best_spar <- dplyr::group_by(mean_bt) %>% dplyr::summarize(min_spar = mean_bt[which.min(.data$mean_bott),]$spar_val)
  # divide this by two, since you'd want the median as a compromise
  # between overfitting and underfitting the data
  best_spar$new_spar <- round(as.numeric(best_spar$min_spar)/2, 2)

  return(best_spar)
}

################################################################################
# functions for finding the center_y value
################################################################################


#' Find the center_y value
#'
#'Center_y is the height of the pivot-point of the KBTDR trasucer arm above
#'depth = 0, in centimetes. While most center_y values are close to 11cm, these
#'functions are only ESTIMATES of center_y. These calculations need to be
#'confirmed visually to ensure that it does not introduce any abnormal skew
#'across the record. However, any variation in this height is < 1mm at the scale
#'of the KBTDR.

#' @param beg_dive numeric vector of the x & y coordinates of the beginning of
#' the dive
#' @param depth_dive numeric vector of the x & y coordinates of a point at depth
#' @param rate rate of film movement, estimated by timing dots
#' @param psi_calibration data frame of psi calibration curve, produced after
#' center_scan if present in the record
#' @param max_depth maximum depth, if no psi calibration curve present
#' @param df tidy trace data frame, if no psi calibration curve present
#' @export
#' @return numeric value of an estimated center_y value to use for arc removal.
#' @examples
#' \dontrun{
#' # if psi calibration curve is present:
#' find_center_y(beg_dive, depth_dive, rate, psi_calibration)
#' find_center_y(beg_dive = c(1142.945, 0), depth_dive = c(1140.55, 9.3), rate = 0.16, psi_calibration)
#'
#' # if only maximum depth is known:
#' find_center_y(beg_dive, depth_dive, rate, max_depth, trace)
#' find_center_y(beg_dive = c(65.258, y1 = -0.056), depth_dive = c(63.442, 5.341), rate = 0.21, max_depth = 484, df = trace)
#' }

# wrapper function to account for if a psi calibration curve is present or not:
find_center_y <- function(beg_dive = c(x1, y1), depth_dive = c(x2, y2), rate,
                          psi_calibration = NULL, max_depth = NULL,
                          df = NULL){
  # defining things to make future functions easier:
  x1 <- beg_dive[1]
  y1 <- beg_dive[2]
  x2 <- depth_dive[1]
  y2 <- depth_dive[2]

  # sorting based on if psi calibration curve present
  if(!is.null(psi_calibration)){
    .find_center_y_psi(x1, y1, x2, y2, rate, psi_calibration)
  } else if (is.null(max_depth) | is.null(df)){
    print("ERROR: need to specify both max depth and trace data frame")
  } else {
    # or if we just know maximum depth
    .find_center_y_nopsi(x1, y1, x2, y2, rate = rate, max_depth = max_depth, df = df)
  }
}


# This function finds the center_y value of the arm given two points along the
# descent of a dive (x1, y1; x2, y2). This dive should be rapid with little to
# no bottom time, since this would imply that the seal was moving at a constant
# rate. To do this, the function calculates the amount of time it would've taken
# the seal to descend to depth, assuming the seal is descending at 1.1 m/s
# (from Williams et al., 2015). The time of this descent was then used to
# transform x2 over in the -x direction, such that the two points would be along
# the same circle with a center of (h, k). Then, the center of the circle was
# calculated using methods described in:
#
# https://math.stackexchange.com/questions/1781438/finding-the-center-of-a-
# circle-given-two-points-and-a-radius-algebraically
#
# because this math is easier to put in code rather than solving a system of
# equations. The returned K value should be the center_y value for the record.

# This function also takes: r = 21.14, which is the constant length of the
# traducer arm; the rate that the record was moving at the time these two points
# were taken ((time point 2 - time point 1) / 12 minutes); and the file used
# for psi calibration (only available for 1981 traces).

#' Find Center Y, with psi calibration curve
#'
#' Find an estimated value to use for the height of the transducer arm's pivot
#' point if a psi calibration curve is present.
#'
#' @param x1 x-value of the surface point
#' @param y1 y-value of the surface point, should be close if not 0.
#' @param x2 x-value of the point at depth
#' @param y2 y-value of the point at depth
#' @param rate rate of film movement, estimated by timing dots in cm/min
#' @param psi_calibration psi calibration data frame after centering
#' @return numeric value of an estimated center_y value to use for arc removal.
#' @examples
#' \dontrun{
#' .find_center_y_psi(x1, y1, x2, y2, rate, psi_calibration = psi_calibration)
#'
#' .find_center_y_psi(x1 = 1142.945, y1 = 0, x2 = 1140.55, y2 = 9.3, rate = 0.16, psi_calibration)
#' }

.find_center_y_psi <- function(x1, y1, x2, y2, rate, psi_calibration = psi_calibration){
  # First, I am transforming y2 and y1 to depth in meters. This was needed to
  # estimate the amount of time it would've taken for the seal to descend to
  # this depth.
  y_vals_df <- data.frame(interp_y = c(y2, y1))
  depth_2 <- .transform_psitodepth(y_vals_df, psi_calibration)$depth[1]
  # finding time it took for seal to descend to that depth assuming it is
  # descending at 1.1 m/s (Williams et al., 2015) and transforming it to
  # minutes
  t_2 <- (depth_2 / 1.1) / 60

  # transforming x over in the -x direction such that two points would be along
  # the same circle with center (h,k)
  x2 = x2 - (rate * t_2)

  # y1 should be very close to 0, but I added extra calculations here to handle
  # non-zero y1 values.
  if (y1 !=0){
    depth_1 <- .transform_psitodepth(y_vals_df, psi_calibration)$depth[2]
    t_1 <- (depth_1 / 1.1) / 60
    x1 = x1 - (rate * t_1)
  }

  # geometry from stack exchange (link above-- I also have sample calculations
  # to confirm that this method of using the geometry of a rhombus works):
  xa =  0.5 * (x2 - x1)
  ya =  0.5 * (y2 - y1)

  a = sqrt(xa^2 + ya^2)
  b = sqrt(21.14^2 - a^2)

  x0 = x1 + xa
  y0 = y1 + ya

  h = x0 + ((b * ya)/a)
  k = y0 - ((b * xa)/a)

  return(rbind(h, k))
}



#' Find Center Y, without psi calibration curve
#'
#' Find an estimated value to use for the height of the transducer arm's pivot
#' point if only a max depth is known
#'
#' @param x1 x-value of the surface point
#' @param y1 y-value of the surface point, should be close if not 0.
#' @param x2 x-value of the point at depth
#' @param y2 y-value of the point at depth
#' @param rate rate of film movement, estimated by timing dots
#' @param max_depth maximum depth, if known
#' @param trace tidy trace data frame
#' @return numeric value of an estimated center_y value to use for arc removal.
#' @examples
#' \dontrun{
#' .find_center_y_nopsi(x1, y1, x2, y2, rate, max_depth, trace)
#'
#' .find_center_y_nopsi(x1 = 65.258, y1 = -0.056, x2 = 63.442, y2 = 5.341, rate = 0.21, max_depth = 484, trace)
#' }
#'
################################################################################
# for records before 1981 without a psi_calibration file, but max depth value
################################################################################
.find_center_y_nopsi <- function(x1, y1, x2, y2, r = 21.14, rate, max_depth, df){

  # finding the depth of y2
  depth_2 <- ((y2 * max_depth) / max(df$y_val, na.rm = TRUE))

  # finding time it took for seal to descend to that depth assuming it is
  # descending at 1.1 m/s (Williams et al., 2015) and transforming it to
  # minutes
  t_2 <- (depth_2 / 1.1) / 60

  # transforming x2 over in the -x direction such that x1 and x2 would be along
  # the same circle with center (h,k)
  x2 = x2 - (rate * t_2)

  if (y1 != 0) {
    ## if the first y value is not = 0:
    depth_1 <-((y1 * max_depth) / max(df$y_val, na.rm = TRUE))

    # finding time it took for seal to descend to that depth assuming it is
    # descending at 1.1 m/s (Williams et al., 2015) and transforming it to
    # minutes
    t_1 <- (depth_1 / 1.1) / 60

    # transforming x1 over in the -x direction such that x1 and x2 would be along
    # the same circle with center (h,k)
    x1 = x1 - (rate * t_1)
  }

  # geometry from stack exchange (link above-- I also have sample calculations
  # to confirm that this method works):
  xa =  0.5 * (x2 - x1)
  ya =  0.5 * (y2 - y1)

  a = sqrt(xa^2 + ya^2)
  b = sqrt(21.14^2 - a^2)

  x0 = x1 + xa
  y0 = y1 + ya

  h = x0 + ((b * ya)/a)
  k = y0 - ((b * xa)/a)

  return(rbind(h, k))
}
