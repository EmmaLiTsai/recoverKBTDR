###############################################################################
# Function: fast_recovery(filepath = "../sample_data")
# Author:   EmmaLi Tsai
# Date:     6/29/21
# 
# Wrapper function that allows for fast recovery of a trace using an argument 
# file that passes all function arguments to the functions in this repository. 
# The argument file contains 14 different arguments: 
#
#   - radius  : the length of the radius arm in cm. This is constant across all 
#               records, but some of the earlier records that I have were 
#               magnified by a slightly larger amount, and therefore the scale  
#               is slightly different. 
#
#   - center_y: height of the pivot point of the transducer arm from depth = 0 
#               in cm. Changes across all records, but I have helper functions 
#               to help provide esimates (see find_center_y_functions.R). 
#
#   - dist_timedot: distance in cm used for centering the record. All time dots 
#               will be centered along y = dist_timedot. 
# 
#   - time_period_min: time elapsed between two time dots. 
# 
#   - spar_h  : spar value used for high depths of the records to increase 
#               resolution for the smooth_trace_dive() function. 
# 
#   - depth_bounds_smooth: depth threshold to use for the rolling mean function 
#               to determine what depths should be considered diving behavior. 
# 
#   - date_start: start of the record in ymd_hms format 
# 
#   - max_depth: maximum depth to use for depth transformation. Only for records
#                older than 1981 without a psi calibration curve 
# 
#   - k_h     : larger window to use for zoc() function, if needed
# 
#   - depth_bounds_l, depth_bounds_h: low and high depth bounds that encapsulate 
#               where depth = 0 is likely to be. Used for zoc() function. 
# 
#   - window  : just a helper window that gives a nice segment of the record for
#               plotting. This can be taken out later, but was mainly just for 
#               me to note the position of different bouts in a record. 
# 
#   - on_seal : time the tdr was placed on seal. In ymd_hms format. 
# 
#   - off_seal: time the tdr was taken off seal. In ymd_hms format.
#
# This was mainly created to start thinking about user interface, but also so I 
# could quickly read in traces with the appropriate arguments. This was more 
# important for my private repo with all the records. 
# 
# Input: 
#   - filepath   : file path that points to all the csv files needed for 
#                  recovery (trace, time_dots, and args file)   
# Output: 
#   - trace      : fully recovered record (centered, zoc (if needed), time and 
#                  depth transformation, smoothed, with POSIXct dates and times 
#                  added) 
###############################################################################
fast_recovery <- function(filepath = "../sample_data"){
  # read data 
  read_trace(filepath = filepath)
  
  # get radius value (usually constant across all records, but earlier ones were 
  # magnified by 8x instead of 7x, so scale is slightly different)
  source("../r_scripts/dive_trace_tidy_functions.R")
  
  # center scan
  trace <- center_scan(trace, time_dots, dist_timedot = args$dist_timedot)
  
  # psi calibration curve, if available
  if (is.na(args$max_depth)){
    psi_calibration <- centered_psi_calibration(trace)
  }
  
  # zoc, if needed
  if (!is.na(args$k_h)){
    # if there is big drift:
    if(args$depth_bound_h > 1){
      trace <- zoc_big_drift(trace, 
                             k_h = args$k_h, 
                             depth_bounds = c(args$depth_bounds_l, args$depth_bounds_h))
      
    } else { # if drift is minor:
      trace <- zoc(trace, 
                   k_h = args$k_h, 
                   depth_bounds = c(args$depth_bounds_l, args$depth_bounds_h))
    }
  }
  
  # remove arc and time assignment
  trace <- transform_coordinates(trace, time_dots, 
                                 center_y = args$center_y, 
                                 time_period_min = args$time_period_min)
  
  # if the record has a psi calibration curve or simply a max depth value:
  if (is.na(args$max_depth)){
    trace <- transform_psitodepth(trace, psi_calibration, 
                                  max_psi = 900, max_position = 22.45)
  } else {
    trace <- transform_todepth(trace, max_depth = args$max_depth)
  }
  
  # smooth the record using the bout method 
  trace <- smooth_trace_dive(trace, 
                             spar_h = args$spar_h, 
                             depth_thresh = args$depth_bounds_smooth)

  # add dates and times and put the output in the global environment
  trace <<- add_dates_times(trace, 
                           start_time = args$date_start, 
                           on_seal = args$on_seal, 
                           off_seal = args$off_seal)

  # eventually write this output into csv file in a different folder(results 
  # folder?), to read into diveMove package...
}
