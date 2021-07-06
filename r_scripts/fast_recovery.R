# fast-lane recovery code using an argument file that passes values to all 
# functions in the repository. This was mainly created to start thinking about 
# user interface, but also so I could easily read in files with the appropriate 
# arguments. This was more important for my private repo with all the records. 
recover_record <- function(filepath = "../sample_data"){
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
                             k = c(2, args$k_h), 
                             probs = c(0.5, 0.02), 
                             depth_bounds = c(args$depth_bounds_l, args$depth_bounds_h))
      
    } else { # if drift is minor:
      trace <- zoc(trace, 
                   k = c(2, args$k_h), 
                   probs = c(0.5, 0.02), 
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
  
  # smooth the record
  trace <- smooth_trace_bout(trace, 
                             spar = c(0.8, args$spar_h), 
                             nknots = c(1000, signif(nrow(trace) * .03, 1)), 
                             window = nrow(trace)/200, 
                             depth_thresh = args$depth_bounds_smooth)

  # add dates and times
  trace <<- add_dates_times(trace, 
                           start_time = args$date_start, 
                           on_seal = args$on_seal, 
                           off_seal = args$off_seal)

  # eventually write this output into csv file in a different folder(results 
  # folder?), to read into diveMove package...
}
