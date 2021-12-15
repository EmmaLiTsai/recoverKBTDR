################################################################################
# Authors: EmmaLi Tsai, Dylan W. Schwilk
# Creates scaled time and depth axes from a Kooyman-Billups TDR dive trace
################################################################################

# STEPS

# 1. Recenter and fix misalignment (both data inputs)
# 2. Transform coordinates by radius arm eqn
# 3. Transform x axis to dates & times
# 4. Interpolate between missing time points
# 5. Transform y axis to depth
# 6. Smoothing

###############################################################################
# Global constants:

# Radius of the KBTDR arm when scaled up to the size of the physical traces.
# This is usually constant across all records (21.14 cm). This might have to
# be a user-defined value in future developments, since I don't know much other
# labs xerographed their records.
RADIUS <- 21.14

# This was used for the psi to depth calculation, for every 1m increase in
# depth, there is 1.4696 increase in PSI in saltwater
PSI_TO_DEPTH <- 1.4696

################################################################################
# STEP ONE: Recenter and fix misalignment (both data inputs) ###################
################################################################################

# Code here is absent because this is really more related to image processing
# methods, but can be found in r/centering_functions (to center the scan, and
# center the psi calibration curve), and r/zoc_functions to zero-offset correct
# the data.

################################################################################
## STEP TWO AND THREE: Apply radius arm transformation and transform to time ###
################################################################################

#' Remove arc in record and transform x-axis to time
#'
#' This function removes the characteristic left-leaning arc in the record by
#' using the equation of the circle the KBTDR arm makes. It also uses the timing
#' dots the transform the x-axis to time, in minutes from the start. This can
#' later be transformed to POSIXct date times and transformed into a regular
#' time series in the add_dates_times function.
#'
#' @param trace tidy trace data frame after centering, contains the x and y
#' values of the trace.
#' @param time_dots tidy time dots data frame, contains the x and y positions
#' of the timing dots.
#' @param center_y height of transducer arm pivot point. This value is usually
#' close to 11 cm, but there is slight variation (<1 mm) at the scale of the
#' KBTDR. Two functions (find_center_y_psi, find_center_y_nopsi) can be used to
#' estimate this value.
#' @param time_period_min minutes elapsed between two time periods.
#' @return trace data frame after arc removal.
#' @importFrom dplyr mutate lead
#' @importFrom tidyr drop_na
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' # if the height of the transducer arm pivot point is 11.1 cm above depth = 0:
#' transform_x_vals(trace, time_dots, center_y = 11.1, time_period_min = 12)
#' }
#'
###############################################################################
# Function: transform_x_vals(trace, time_dots, center_y = 11.1, time_period_min = 12)
# Author:   EmmaLi Tsai
# Date:     3/30/21
#
# Function takes the tidy trace and time dots files to complete two steps:
#
#   (2) - apply radius arm transformation using the geometry of the KBTDR device
#         which uses the globally defined constants above.
#
#   (3) - transform the x axis from time using the timing dots. To do this, I
#         created a helper data frame with the start and end points of a
#         time period and the corresponding scale value. This data frame is
#         needed to help make the cut() function easier, so I can break
#         the trace up into sections that would identify which time period a
#         specific x value belongs to along a trace. Then, I do a merge() with
#         the trace data frame. Using this merged data frame, I do a mutate()
#         for some simple calculations that will use the distance a point is
#         from the start point of a time period and the scale value I made
#         earlier to estimate the time value of a specific point.
#
# Input:
#
#   - time_dots   : tidy time_dots data frame, contains the x and y values of
#                   the time dots for a trace
#
#   - trace       : tidy trace data frame, contains the x and y values of the
#                   trace
#
#   - center_y    : height of the pivot point of the transducer arm from depth
#                   = 0. This varies slightly across traces, but sample
#                   calculations can be found in the r_scripts/find_center_y.R
#                   file. This value also needs to be visually confirmed (i.e.,
#                   no abnormal skew across the record)
#
#   - time_period_min : minutes between each time period. This is 12 minutes for
#                       most traces.
#
# Output:
#
#   - trace      : trace data frame complete with time periods, and time
#                  of an x value in minutes from when the device started
#                  gathering data. I kept all columns to ensure that the
#                  function was working properly.
###############################################################################
transform_x_vals <- function(trace, time_dots, center_y = 11.1, time_period_min = 12) {
  ## Start Step Two: Transform Coordinates by Radius Arc Eqns #################

  # applying my new equation, basically just the equation of a circle but takes
  # the original x/y and calculates where the center of the circle would be
  # (h), and uses this new center to find the x value when depth = 0. I did
  # some algebra to fit this math into one line of code, but it should be noted
  # that points close to the origin and < 0 will often get transformed in the
  # -x direction and placed before the origin. This is likely unimportant
  # because this would be exactly when the TDR was was turned on and therefore
  # not attached to the animal yet.
  trace$new_x <- -sqrt((RADIUS^2) - (center_y^2)) +
    (trace$x_val + sqrt(RADIUS^2 - (trace$y_val - center_y)^2))

  # ordering the file based on new_x value-- this is needed to create accurate
  # time periods in step three below
  trace <- trace[order(trace$new_x),]

  ## Starting Step Three: scale X based on time dots ###########################
  # creating zero stating time dot for time assignment
  time_dots_zero <- c(0, time_dots$x_val)

  # creating a data frame with time periods and start/end points for the time
  # period... this will be used to cut the data
  tp_df <- data.frame(time_period = seq(1:length(time_dots_zero)),
                      start_x = time_dots_zero,
                      end_x = dplyr::lead(time_dots_zero),
                      stringsAsFactors = FALSE)

  # adding the scale value for each time period, which will be multiplied by:
  # (trace$new_x - tp_df$start_x) to assign a time to each new_x
  tp_df$scale = time_period_min / (tp_df$end_x - tp_df$start_x)

  # adding this as a time period variable to the trace using the cut() function
  trace$time_period <- cut(trace$new_x,
                           breaks = tp_df$start_x,
                           include.lowest = TRUE,
                           labels = tp_df$time_period[1:(nrow(tp_df) - 1)])

  # merging the trace file with the time points data frame, which uses time
  # periods as an ID variable:
  trace <- merge(trace, tp_df, by = 'time_period', all.x = TRUE)

  # mutating to create the time scale. This mutate function first calculates
  # the difference between the new x value and the start x value of the time
  # period, and then multiplies this by the scale value. I needed this value
  # to calculate time, which uses this scale value and relates this information
  # to the time period.
  trace <- dplyr::mutate(trace,
                         diff = .data$new_x - .data$start_x,
                         diff_with_scale = diff * .data$scale,
                         time = .data$diff_with_scale + (as.numeric(.data$time_period)-1) * time_period_min)

  # removing extra columns created by the function
  trace <- trace[,!(names(trace) %in% c("start_x", "scale", "end_x", "diff", "diff_with_scale", "time_period"))]

  # returning final trace -- there will be some NAs from points that happened
  # after the last time dot (and therefore couldn't be assigned a time), or
  # points that were negative and very close to the origin (and therefore arc
  # removal moved them over in the -x direction and before the origin). I also
  # order the record here.
  return(tidyr::drop_na(trace[order(trace$time),]))
}

#' Add POSIXct date times and create a regular time series
#'
#' The seal often moved faster than the transducer arm could document the dive,
#' which results in large gaps as the seal is descending/ascending. This
#' function transforms the record into a regular time series, which greatly
#' benefits future spline smoothing and dive analysis. This also transforms
#' the x-axis into POSIXct date times.
#'
#' @param trace tidy trace data frame after arc removal, contains the x and y
#' values of the trace.
#' @param start_time time TDR was turned on, in y:m:d h:m:s format.
#' @param on_seal time TDR was placed on the seal, in y:m:d h:m:s format.
#' @param off_seal time TDR taken off the seal, in y:m:d h:m:s format.
#' @param tz time zone to use for POSIXct date times.
#' @return trace data frame with POSIXct date times and interpolated points to
#' fill sparse parts of the record.
#' @importFrom dplyr filter
#' @importFrom lubridate ymd_hms minutes seconds
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' trace <- add_dates_times(trace, start_time = "1981:01:16 15:10:00",
#' on_seal = "1981:01:16 17:58:00", off_seal = "1981:01:23 15:30:00",
#' tz = "Antarctica/McMurdo")
#' }
#'
###############################################################################
# Function: add_dates_times(trace, start_time, on_seal, off_seal)
# Author:   EmmaLi Tsai
# Date:     4/10/21
#
# Function takes the trace data frame after arc removal, the time the TDR was
# turned on, and the time the TDR was placed on/off the seal to assign POSIXct
# times to the record. This also contains an internal function
# (create_regular_ts) which transforms the record into a regular time series.
# This was particularly needed for discontinuous records where the TDR didn't
# pick up on the ascent/descent behavior of the seal, and all dive analysis
# packages that I know of assume a regular time series. This function also snips
# the trace data frame to only contain data from [on_seal:off_seal,].
#
# Input:
#
#   - trace       : tidy trace data frame following arc removal containing
#                   an x-axis that is time in minutes from the start.
#
#   - start_time  : time the TDR was turned on in ymd_hms format.
#
#   - on_seal     : time the TDR was placed on the seal, in ymd_hms format.
#
#   - off_seal    : time the TDR was taken off the seal, in ymd_hms format.
#
# Output:
#
#   - trace       : trace data frame complete with POSIXct dates, times, and a
#                   regular time series containing interpolayed y-values.
###############################################################################
add_dates_times <- function(trace, start_time, on_seal, off_seal, tz = "Antarctica/McMurdo"){
  # adding dates and times from lubridate package
  trace$date_time <- lubridate::ymd_hms(start_time, tz = tz) +
    lubridate::minutes(as.integer(trace$time)) +
    lubridate::seconds(as.integer((trace$time %% 1) * 60))

  # removing duplicated times -- this happened when two points were very close
  # together and got assigned the same time. Dive analysis packages cannot
  # handle duplicated times
  trace <- trace[!duplicated(trace$date_time),]

  # need to convert to ymd_hms format
  on_seal <- lubridate::ymd_hms(on_seal, tz = tz)
  off_seal <- lubridate::ymd_hms(off_seal, tz = tz)

  # filtering the data based on the time the TDR was placed on the seal to when
  # it was taken off
  trace <- trace %>% dplyr::filter(.data$date_time >= on_seal & .data$date_time <= off_seal)
  # transforming to regular time series, this will probably be an internal
  # function (see function below-- achieves step 4 of the recovery process)
  trace <- .create_regular_ts(trace, on_seal, off_seal, tz = tz)
  # returning the trace
  return(trace)
}

################################################################################
## STEP FOUR: Create regular time series #######################################
################################################################################
#' creating regular time series for trace data
#' @param trace tidy trace data frame after arc removal, contains the x and y
#' values of the trace.
#' @param on_seal time TDR was placed on the seal.
#' @param off_seal time TDR taken off the seal.
#' @param tz time zone for calculating date/times
#' @return trace data frame with POSIXct date times and interpolated points to
#' fill sparse parts of the record.
#' @importFrom zoo na.approx
#' @importFrom lubridate ymd_hms
#' @examples
#' \dontrun{
#' trace <- .create_regular_ts(trace, on_seal = "1981:01:16 17:58:00",
#' off_seal = "1981:01:23 15:30:00", tz = "Antarctica/McMurdo")
#' }
###############################################################################
# Function: create_regular_ts(trace, on_seal, off_seal)
# Author:   EmmaLi Tsai
# Date:     6/20/21
#
# creating a regular time series. This was necessary to make future dive
# analysis more reliable, since the diveMove package was really only built to
# handle regular time series. Some records were also very discontinuous, so this
# step also helps with future spline smoothing. However, this does come at a
# cost of larger files and longer run time. This function is nested in the
# add_dates_times function above, and will likely become an internal function in
# future commits. Essentially, it creates a data frame containing a row for
# every second the TDR was on the seal. I then do a complete merge with the
# original trace data frame, which retains all original data points. I then
# linearly interpolate to fill all NAs created after the merge with y-values,
# while keeping all original data.
#
# Input:
#
#   - trace       : tidy trace data frame following arc removal containing
#                   an irregular time series that is POSIXct date&time object.
#
#   - on_seal     : time the TDR was placed on the seal, in ymd_hms format.
#
#   - off_seal    : time the TDR was taken off the seal, in ymd_hms format.
#
# Output:
#
#   - trace       : trace data frame complete with POSIXct dates, times, and a
#                   regular time series containing interpolayed y-values.
###############################################################################
.create_regular_ts <- function(trace, on_seal, off_seal, tz = "Antarctica/McMurdo"){
  # convert to ymd_hms format, if needed
  on_seal <- lubridate::ymd_hms(on_seal, tz = tz)
  off_seal <- lubridate::ymd_hms(off_seal, tz = tz)
  # creating regular time series
  reg_time <- seq(on_seal, off_seal, by = "sec")
  # transform to data frame
  reg_time <- data.frame(reg_time = reg_time)
  # merge regular time series into irregular time series, keeping all original
  # data
  trace_reg <- merge(trace, reg_time, by.x = "date_time", by.y = "reg_time", all.y = TRUE)
  # replacing NAs with linearly interpolated values
  interp <- zoo::na.approx(trace_reg$y_val, trace_reg$date_time)
  # cutting to match
  interp <- interp[1:nrow(trace_reg)]
  # interpolated depth for regular time series
  trace_reg$interp_y <- interp
  # removing NA values at the tail end of the record
  trace_reg <- trace_reg[which(!is.na(trace_reg$interp_y)),]
  # final return
  return(trace_reg)
}

################################################################################
## STEP FIVE: Transform Y Axis to Depth ########################################
################################################################################
#' Transform the y-axis from position to depth in meters
#'
#' Transforms the y-axis of the record from position to depth, using either
#' the psi calibration curve at the end of the record, or a known maximum depth
#' for that record.
#'
#' @param trace tidy trace data frame after arc removal, contains the x and y
#' values of the trace.
#' @param max_depth maximum depth of trace in meters, if psi calibration curve
#' is not present.
#' @param psi_calibration data frame containing the centered psi calibration
#' curve.
#' @param max_psi maximum psi of the TDR, often not captured in psi calibration
#' curve.
#' @param max_position position of maximum psi reading for TDR in cm.
#' @export
#' @return trace data frame with depth in meters.
#' @examples
#' \dontrun{
#'# if the record has a psi calibration curve at the end:
#' trace <- transform_psitodepth(trace, psi_calibration,
#' max_psi = 900, max_position = 22.45)
#'
#' # if only the maximum depth is known:
#' trace <- trace(trace, max_depth = 317)
#' }
transform_y_vals <- function(trace, max_depth = NULL, psi_calibration = NULL, max_psi = NULL, max_position = NULL){
  # if we just know maximum depth
  if(!is.null(max_depth)){
    trace <- .transform_todepth(trace, max_depth)
  } else if(is.null(max_psi) | is.null(max_position)){ # if we have a psi calibration curve
    # need both max_psi and max_position -- return error
    print("ERROR: need position and maximum psi values")
  } else {
    trace <- .transform_psitodepth(trace, psi_calibration, max_psi, max_position)
  }
  return(trace)
}

#' Transform the y-axis from position to depth in meters using the psi
#' calibration curve
#' @param trace tidy trace data frame after arc removal, contains the x and y
#' values of the trace.
#' @param psi_calibration data frame containing the centered psi calibration
#' curve.
#' @param max_psi maximum psi of the TDR, often not captured in psi calibration
#' curve.
#' @param max_position position of maximum psi reading for TDR in cm.
#' @return trace data frame with depth in meters.
#' @importFrom tidyr separate
#' @importFrom dplyr case_when lead
#' @examples
#' \dontrun{
#' trace <- .transform_psitodepth(trace, psi_calibration, max_psi = 900, max_position = 22.45)
#' }

###############################################################################
# Function: transform_psitodepth(trace, psi_calibration, max_psi = 900, max_position = 22.45))
# Author:   EmmaLi Tsai
# Date:     4/09/21
#
# This function takes the tidy trace file (containing x and y values of the
# trace) and a csv file (psi_calibration) containing the intervals and positions
# of the psi calibration curve at the end of the trace to determine the psi
# values of the trace. Essentially, this function creates breaks and labels out
# of the psi_calibration csv file to cut() the trace into different categories
# based on the y_val of a point. From these categories and the labels defined,
# this function then uses proportions to calculate the psi value of a specific
# point. These psi values can then be transformed to depth (in meters) using a
# simple calculation.
#
# It should also be noted that this requires a segmented calibration, since
# the scale changes between psi intervals. This made the code slightly more
# complicated.
#
# Input:
#
#   - trace       : tidy trace data frame, contains the x and y values of the
#                   trace after centering.
#
#   - psi_calibration : centered data frame produced after centering that that
#                       contains two columns for the cut():
#                           - psi_interval: the psi intervals at the end of the
#                                           record (i.e., 100psi, 200psi, etc.)
#                           - psi_position: the y_val that corresponds to that
#                                           psi interval in cm.
#
#   - max_psi     : max psi for the TDR, which wasn't documented on the record.
#                   default set to 900psi
#
#   - max_position: position of the maximum psi reading, which would be the top
#                   border of the trace. This, combined with max_psi help catch
#                   really deep and rare depth readings.
#
# Output:
#
#   - trace      : trace data frame complete with the psi value and depth. I
#                  kept both so we can ensure that the psi calibration curve at
#                  the end of the record is precise.
###############################################################################
.transform_psitodepth <- function(trace, psi_calibration, max_psi = 900, max_position = 22.45) {
  # defining labels and adding the maximum psi of the TDR
  labels <- c(0, psi_calibration$psi_interval, max_psi)

  # defining the breaks and adding the maximum position of the TDR and also the
  # minimum position to capture the lower values
  breaks <- c(min(trace$interp_y, na.rm = TRUE), psi_calibration$psi_position, max_position)

  # combining the breaks and labels for future calculations
  labels_combined <- paste(labels, breaks, sep = ":")
  # combining the labels again to capture the full interval a y_val falls into
  labels_combined <- paste(labels_combined, dplyr::lead(labels_combined), sep = ":")[1:length(labels_combined)-1]

  # cutting the data frame using the above breaks and labels
  psi_interval_both <- as.data.frame(cut(trace$interp_y, breaks = breaks,
                                         include.lowest = TRUE, labels = labels_combined))
  # changing name of column
  names(psi_interval_both) <- "psi_interval_both"

  # splitting the label created by the cut function in to four separate columns
  # since this made the calculations easier
  psi_interval_sep <- tidyr::separate(psi_interval_both, col = 1,
                                      sep = ":",
                                      into = c("psi_interval_1", "psi_position_1",
                                               "psi_interval_2", "psi_position_2"))

  # changing to numeric values
  tidy_cols <- as.data.frame(sapply(psi_interval_sep, function(x) as.numeric(paste(x))))

  # helper vectors for future calculations. I basically needed to do a segmented
  # calibration since the scales between psi intervals are different.
  # finding the difference in psi between intervals
  diff_psi <- tidy_cols$psi_interval_2 - tidy_cols$psi_interval_1
  # calculating the difference in position between two intervals
  diff_pos <- tidy_cols$psi_position_2 - tidy_cols$psi_position_1
  # finding difference in y value from the lower psi value of the interval it
  # fell into
  diff_y_val <- trace$interp_y - tidy_cols$psi_position_1

  # calculating psi -- had to be modified for y-values that were < 0, where only
  # interval 2 would be used as a scale. Y-vals that fell in higher intervals
  # had to be scaled differently.
  trace$psi <- dplyr::case_when(tidy_cols$psi_interval_1 == 0 ~ (tidy_cols$psi_interval_2 * trace$interp_y) / tidy_cols$psi_position_2,
                                tidy_cols$psi_interval_1 > 0 ~ tidy_cols$psi_interval_1 + ((diff_y_val * diff_psi) / diff_pos))

  # final transformation
  trace$depth <- trace$psi / PSI_TO_DEPTH
  # returning the trace
  return(trace)
}

#' Transform the y-axis from position to depth in meters using a known maximum
#' depth.
#' @param trace tidy trace data frame after arc removal, contains the x and y
#' values of the trace.
#' @param max_depth maximum depth of trace in meters.
#' @return trace data frame with depth in meters.
#' @examples
#' \dontrun{
#' trace <- .trace(trace, max_depth = 317)
#' }

# This is a simple function for the 1978 and 1979 traces without a psi
# calibration curve at the end. I don't have the calibration records for these
# traces, so I'll have to calibrate depth using the max depths from the
# Castellini et al., 1992 bulletin.
.transform_todepth <- function(trace, max_depth){
  # calculating depth using the max depth the user defines and the max
  # value of the trace:
  trace$depth <- ((trace$interp_y * max_depth) / max(trace$interp_y, na.rm = TRUE))
  # returning trace
  return(trace)
}

################################################################################
## STEP SIX: Smoothing #########################################################
################################################################################
#' Spline smoothing to reduce record transducer noise
#'
#' The transducer arm often produced extra chatter at depth = 0, from when there
#' was less tension on the arm. This function spline smoothes the data to
#' account for this noise by using a dive detector such that resolution of
#' spline smoothing increases when a dive is detected, and less when the seal
#' is resting. This reduces noise in depth = 0, while retaining post-dive
#' surface interval information.
#'
#' @param trace tidy trace data frame after arc removal, contains time and depth
#' of the trace.
#' @param spar_h higher-resolution spar value to use during a dive. This can
#' either be visually determined, but computation methods are available in
#' find_best_spar(). Default set to 0.3.
#' @param depth_thresh depth(m) threshold to use for when a dive is detected.
#' Default set to 5 meters.
#' @return data frame with depth_smooth, which is the depth of the record when
#' smoothed.
#' @importFrom caTools runmean
#' @importFrom dplyr case_when mutate lag
#' @importFrom stats smooth.spline predict deriv
#' @export
#' @examples
#' \dontrun{
#' trace <- smooth_trace_dive(trace, spar_h = 0.3, depth_thresh = 5)
#' }
###############################################################################
# Function: smooth_trace_dive(trace, spar_h = 0.3, depth_thresh = 5)
# Author:   EmmaLi Tsai
# Date:     6/15/2021
#
# Function takes the trace (after time and depth have been transformed) to
# perform penalized spline smoothing on the data. First, it uses a rolling mean
# function to detect when the average depth is >= depth_thresh, where it is
# possible to assume that the seal is diving. The window size used for this
# rolling mean is ~0.2% of the rows in the record. When a dive is detected, it
# increases the resolution of spline smoothing by decreasing the smoothing
# penalty to spar_h, and knots = ~3% of the rows in the record. This was an
# attempt to increase the resolution of smoothing when the seal was in a bout of
# dives and to retain the surface intervals between dives (which would be lost
# in a smoothing method bounded by just depth). This also decreases the
# resolution of spline smoothing when the seal is not diving (to spar = 0.8,
# knots = 1000), to reduce chatter created by the transducer arm at shallow
# depths.
#
# Input:
#
#   - trace        : trace dataframe after time and depth axis transformation.
#
#   - spar_h       : spar value to use at higher depths. Should be < 0.8, and
#                    default is set to 0.3.
#
#   - depth_thresh : depth threshold to use for the rolling mean. Default is set
#                    to 5m, such that rolling means >= 5m would be considered
#                    diving behavior.
#
# Output:
#
#   - smooth_trace : trace data frame with smoothed values (smooth_depth), and
#                    also with dive component assignment.
###############################################################################
smooth_trace_dive <- function(trace, spar_h = 0.3, depth_thresh = 5){
  # defining spar, nknots, and window values:
  spar = c(0.8, spar_h)
  nknots = c(100, signif(nrow(trace) * .02, 1))
  window = signif(floor(nrow(trace) * 0.001), 1)

  # ordering
  trace <- trace[order(unclass(trace$date_time)),]
  # detecting a bout of dives using the runmean function on a window of the
  # data:
  detect_bout <- data.frame(runmean = (caTools::runmean(trace$depth, window)),
                            depth = trace$depth,
                            date_time = trace$date_time)
  # defining a bout as when the mean depth is >= depth threshold in that window
  trace$bout <- dplyr::case_when(detect_bout$runmean >= depth_thresh ~ 1,
                                 detect_bout$runmean < depth_thresh ~ 0)
  # separating parts of the record not in about
  trace_nobout <- trace[which(trace$bout == 0), ]
  # separating parts of the record in a bout
  trace_bout <- trace[which(trace$bout == 1), ]
  # spline smoothing for the parts of the record not in bout
  smooth_fit_nobout <- stats::smooth.spline(trace_nobout$date_time,
                                            trace_nobout$depth,
                                            spar = spar[1], nknots = nknots[1])
  # predicting for the parts of the record not in a bout
  trace_nobout$smooth <- stats::predict(smooth_fit_nobout,
                                        unclass(trace_nobout$date_time))$y
  # spline smoothing for the parts of the record in a bout
  smooth_fit_bout <- stats::smooth.spline(trace_bout$date_time,
                                          trace_bout$depth,
                                          spar = spar[2], nknots = nknots[2])
  # predicting for the parts of the record in a bout
  trace_bout$smooth <- stats::predict(smooth_fit_bout,
                                      unclass(trace_bout$date_time))$y

  # recombining the two:
  smooth_trace <- rbind(trace_nobout, trace_bout)
  # ordering
  smooth_trace <- smooth_trace[order(smooth_trace$date_time),]

  # recursive and final smoothing
  spline_mod_bout <- stats::smooth.spline(smooth_trace$date_time,
                                          smooth_trace$smooth,
                                          spar = spar[2],
                                          nknots = nknots[2])
  # added final smoothing and dive component assignment -- this can be removed
  # later but I was experimenting with it here
  smooth_trace <- dplyr::mutate(smooth_trace,
                                smooth_depth = stats::predict(spline_mod_bout, unclass(smooth_trace$date_time))$y,
                                deriv = stats::predict(spline_mod_bout, unclass(smooth_trace$date_time), deriv=1)$y,
                                ascent = deriv < 0,
                                deriv_diff = dplyr::lag(sign(deriv)) - sign(deriv),
                                peak = case_when(deriv_diff < 0 ~ "TOP",
                                                 deriv_diff > 0 ~ "BOTTOM"))
  # removing extra column
  smooth_trace <- smooth_trace[,!(names(smooth_trace) %in% c("smooth"))]
  # removing excess noise at the surface
  if (any(smooth_trace$smooth_depth < 0)) {
    smooth_trace[smooth_trace$smooth_depth < 0,]$smooth_depth <- 0
  }

  # final return
  return(smooth_trace)
}
