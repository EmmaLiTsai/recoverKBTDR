###############################################################################
# Function: spar_dive_stats(filepath = "../sample_data/WS_25_1981)
# Author:   EmmaLi Tsai
# Date:     3/30/21
# 
# One of the biggest challenges is finding the best smoothing penalty (spar 
# value, in this case) that would effectively capture all of the dives without 
# overfitting (producing ghost wiggles) or underfitting (not capturing
# inflections in the record at depth). As expected, this function takes awhile 
# to run but outputs of the functions (i.e., dive_stats csv file, 
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
#   - filepath    : folder that contains all files for the record, including the 
#                   argument file that is used for fast recovery. 
#   
# Output: 
#   - adds a new data frame (dive_stats) to the global environment, and also 
#     returns a small data frame that contains the spar value that minimized
#     average bottom distance and the "ideal" spar value that should be used.
###############################################################################
spar_dive_stats <- function(filepath = "../sample_data/WS_25_1981"){
  # read in data 
  read_trace(filepath = filepath)
  # center data 
  trace <- center_scan(trace, time_dots, dist_timedot = args$dist_timedot)
  # extract the centered calibration curve, if this record does not have a 
  # maximum depth on record 
  if (is.na(args$max_depth)){
    psi_calibration <- centered_psi_calibration(trace)
  }
  # zoc, if needed
  if (!is.na(args$k_h)){
    # if there is big drift:
    if(args$depth_bounds_h > 1){
      trace <- zoc_big_drift(trace, 
                             k_h = args$k_h, 
                             depth_bounds = c(args$depth_bounds_l, args$depth_bounds_h))
      
    } else { # if drift is minor:
      trace <- zoc(trace, 
                   k_h = args$k_h, 
                   depth_bounds = c(args$depth_bounds_l, args$depth_bounds_h))
    }
  }
  # transforming x-axis to time (minutes from the start)
  trace <- transform_coordinates(trace, time_dots, center_y = args$center_y, time_period_min = args$time_period_min)
  
  # adding dates, times, and converting to regular time series 
  trace <- add_dates_times(trace, 
                           start_time = args$date_start, 
                           on_seal = args$on_seal, 
                           off_seal = args$off_seal)
  
  # calculating depth
  if (is.na(args$max_depth)){
    # if the record has a calibration curve 
    trace <- transform_psitodepth(trace, psi_calibration, 
                                  max_psi = 900, max_position = 22.45)
  } else {
    # if the record only has a max depth reading 
    trace <- transform_todepth(trace, max_depth = args$max_depth)
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
    trace_i <- trace
    
    trace_i <- smooth_trace_dive(trace_i, 
                                 spar_h = spar_seq[i], 
                                 depth_thresh = args$depth_bounds_smooth)
    
    # writing to csv file in results folder: 
    results_filepath <- paste(dir_name, "/", base_name, "_", spar_seq[i],".csv", sep = "")
    write.csv(trace_i, results_filepath)
    # just printing to the console so the user can see what iteration of the 
    # loop the function is on
    print(spar_seq[i])
  }
  # adding dive stats for each spar iteration 
  dive_stats <<- get_divestats(folder = paste(dir_name, "/", sep = ""))
  return(clean_divestats(dive_stats))
}

# reads in all data for dive analysis, achieves step two of the function 
# described above
get_divestats <- function(folder = "../results/WS_25_1981/"){
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

# function for tidying the dive_stats data. This includes adding dive numbers,
# providing better names, and also creating a data frame of the best spar value 
# to use for that record. 
clean_divestats <- function(dive_stats){
  
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

# internal helper function for assigning dive numbers 
.fun <- function(x) {
  test <- x$diveNo > 0
  y <- cumsum(test) 
  c(y - cummax(y * !test))
}

# function that calculates the spar value that minimizes average bottom distance 
# and computes the ideal spar value for that record
best_spar <- function(dive_stats){
  
  # finding spar value that minimizes bottom distance:
  bot_all <- dplyr::select(dive_stats, c("bottdist", "spar_val"))
  # spar val that gives minimum bottom distance for each WS_id
  mean_bt <- dplyr::group_by(bot_all, spar_val) %>% summarize(mean_bott = mean(bottdist))
  # finding the spar value that minimizes mean bottom distance, this will give the 
  # lower end of the unimodal distribution 
  best_spar <- dplyr::group_by(mean_bt) %>% summarize(min_spar = mean_bt[which.min(mean_bott),]$spar_val)
  # divide this by two, since you'd want the median as a compromise 
  # between overfitting and underfitting the data 
  best_spar$new_spar <- round(as.numeric(best_spar$min_spar)/2, 2)
  
  return(best_spar)
}
