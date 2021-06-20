# very simple function for spline smoothing of trace data 
smooth_trace <- function(trace, spar = 0.3, nknots = 5900){ 
  # need to order first: 
  trace <- trace[order(trace$time),]
  # creating smoothing fit with the spar and knots the user defines
  smooth_fit <- smooth.spline(trace$time, trace$depth, 
                              spar = spar, nknots = nknots)
  # adding the smoothed predict values to the trace data frame 
  trace$smooth_depth <- predict(smooth_fit, trace$time)$y
  
  # return trace data 
  return(trace)
}

# Another possible spline smoothing function: 
# More complex spline smoothing function with depth bounds. This function
# smooths at a lower resolution for depths < depth_bound (defaults set to 
# spar = 0.8 and 1000 knots), and higher resolution at depths > depth_bound 
# (defaults set to spar = 0.3 and 5900 knots). This is an attempt to reduce 
# chatter in the transducer arm when the seal is resting or hauled out, while 
# keeping higher resolution in the dives at depth to retain wiggles. This 
# method would be sound considering there would be less tension on the 
# transducer arm at shallow depths, which produced unnecessary noise in the 
# record. Here, I also added dive component assignment. 
smooth_trace_bounded <- function(trace, spar = c(0.8, 0.3), nknots = c(1000, 5900), depth_bound = 5){ 
  # need to order first: 
  trace <- trace[order(trace$time),]
  # separating shallow parts of the record 
  trace_shallow <- trace[which(trace$depth <= depth_bound), ]
  # separating deep parts of the record 
  trace_deep <- trace[which(trace$depth > depth_bound), ]
  # spline smoothing for the shallow parts of the record 
  smooth_fit_shallow <- smooth.spline(trace_shallow$time, trace_shallow$depth, 
                                      spar = spar[1], nknots = nknots[1])
  # predicting for the shallow parts of the record 
  trace_shallow$smooth <- predict(smooth_fit_shallow, trace_shallow$time)$y
  # spline smoothing for the deeper parts of the record 
  smooth_fit_deep <- smooth.spline(trace_deep$time, trace_deep$depth, 
                                   spar = spar[2], nknots = nknots[2])
  # predicting for the deeper parts of the record 
  trace_deep$smooth <- predict(smooth_fit_deep, trace_deep$time)$y
  
  # recombining the two: 
  smooth_trace <- rbind(trace_shallow, trace_deep)
  # ordering 
  smooth_trace <- smooth_trace[order(smooth_trace$time),]
  
  # recursive and final smoothing 
  spline_mod_depth <- smooth.spline(smooth_trace$time, smooth_trace$smooth, 
                                  spar = spar[2], nknots = nknots[2])
  # added final smoothing and dive component assignment 
  smooth_trace <- dplyr::mutate(smooth_trace, 
                                smooth_depth = predict(spline_mod_depth, smooth_trace$time)$y,
                                deriv = predict(spline_mod_depth, smooth_trace$time, deriv=1)$y,
                                ascent = deriv < 0,
                                deriv_diff = lag(sign(deriv)) - sign(deriv),
                                peak = case_when(deriv_diff < 0 ~ "TOP",
                                                 deriv_diff > 0 ~ "BOTTOM"))
  # removing extra column 
  smooth_trace <- smooth_trace[,!(names(smooth_trace) %in% c("smooth"))]
  
  # final return 
  return(smooth_trace)
}


# here is another possible smoothing method that attempts to increase resolution 
# in spline smoothing when the seal is undergoing a bout of dives. It is very 
# similar to the method above, but uses a rolling mean to detect instances where
# the average depth is >= 10 meters and we can therefore assume that a bout of 
# dives is happening. It then increases the resolution of the smoothing spline. 
smooth_trace_bout <- function(trace, spar = c(0.8, 0.3), nknots = c(1000, 5900), window = 50, depth_thresh = 10){
  # ordering 
  trace <- trace[order(trace$time),]
  # detecting a bout of dives using the runmean function: 
  detect_bout <- data.frame(runmean = (runmean(trace$depth, window)), 
                            depth = trace$depth, 
                            time = trace$time)
  # defining a bout as when the mean depth is >= 10 meters in that window 
  trace$bout <- dplyr::case_when(detect_bout$runmean >= depth_thresh ~ 1, 
                                 detect_bout$runmean < depth_thresh ~ 0)
  # separating parts of the record not in about
  trace_nobout <- trace[which(trace$bout == 0), ]
  # separating parts of the record in a bout
  trace_bout <- trace[which(trace$bout == 1), ]
  # spline smoothing for the parts of the record not in bout
  smooth_fit_nobout <- smooth.spline(trace_nobout$time, trace_nobout$depth, 
                                     spar = spar[1], nknots = nknots[1])
  # predicting for the parts of the record not in a bout 
  trace_nobout$smooth <- predict(smooth_fit_nobout, trace_nobout$time)$y
  # spline smoothing for the parts of the record in a bout
  smooth_fit_bout <- smooth.spline(trace_bout$time, trace_bout$depth, 
                                   spar = spar[2], nknots = nknots[2])
  # predicting for the parts of the record in a bout 
  trace_bout$smooth <- predict(smooth_fit_bout, trace_bout$time)$y
  
  # recombining the two: 
  smooth_trace <- rbind(trace_nobout, trace_bout)
  # ordering 
  smooth_trace <- smooth_trace[order(smooth_trace$time),]
  
  # recursive and final smoothing 
  spline_mod_bout <- smooth.spline(smooth_trace$time, smooth_trace$smooth, 
                              spar = spar[2], nknots = nknots[2])
  # added final smoothing and dive component assignment 
  smooth_trace <- dplyr::mutate(smooth_trace, 
                                smooth_depth = predict(spline_mod_bout, smooth_trace$time)$y,
                                deriv = predict(spline_mod_bout, smooth_trace$time, deriv=1)$y,
                                ascent = deriv < 0,
                                deriv_diff = lag(sign(deriv)) - sign(deriv),
                                peak = case_when(deriv_diff < 0 ~ "TOP",
                                                 deriv_diff > 0 ~ "BOTTOM"))
  # removing extra column 
  smooth_trace <- smooth_trace[,!(names(smooth_trace) %in% c("smooth"))]
  
  return(smooth_trace)
}


# cross validation methods: 
# leave-one-out cross validation method- inefficient and slow due to nested for 
# loop. 

find_spar_loocv <- function(trace){
  # creating smaller trace data frame 
  trace_cv <- trace[sample(1:nrow(trace), 1000),]
  # spar sequence 
  spar_seq <- seq(from = 0.05, to = 1.0, by = 0.02)
  
  # creating an empty vector to store values in the loop 
  cv_error_spar <- rep(NA, length(spar_seq))
  
  # looping through the spar sequence ... I don't like this nested for loop
  for (i in 1:length(spar_seq)){
    # grabbing the spar value
    spar_i <- spar_seq[i]
    # cross validation error 
    cv_error <- rep(NA, nrow(trace_cv))
    # looping through the trace segment to determine cross validation error 
    for (v in 1:nrow(trace_cv)){
      # testing data
      x_val <- trace_cv$time[v]
      y_val <- trace_cv$depth[v]
      # training data, by leaving out the first row 
      x_train <- trace_cv$time[-v]
      y_train <- trace_cv$depth[-v]
      # smooth fit and prediction using the training data set 
      smooth_fit <- smooth.spline(x = x_train, y = y_train, spar = spar_i)
      # predict on the training data set 
      y_predict <- predict(smooth_fit, x = x_val)
      # calculating the error from the testing data 
      cv_error[v] <- (y_val - y_predict$y)^2
    }
    # getting mean error for that value of spar 
    cv_error_spar[i] <- mean(cv_error)
  }
  
  # plotting prediction error 
  plot(x = spar_seq, y = cv_error_spar, type = "b", lwd = 3, col = "blue",
       xlab = "Value of 'spar'", ylab = "LOOCV prediction error")
  
  # finding the spar value that had the lowest prediction error 
  return(spar_seq[which(cv_error_spar == min(cv_error_spar))])
}

# function to help find a good spar value. The function runs through a sequence 
# of spar values and returns a long data frame that can be used to visualize 
# different outputs in ggplot
view_spar_options <- function(trace, increase_spar = 0.05, nknots = 5900){
  # defining knots and spar sequence
  spar_seq <- seq(0, 1, by = increase_spar)
  spar_gcv <- rep(NA, length(spar_seq))
  nknots <- nknots
  # ordering trace 
  trace <- trace[order(trace$time),]
  data_i <- trace
  # looping through each spar value, keeping note of the gcv value for future 
  # comparisons 
  for(i in 1:length(spar_seq)){
    smooth_fit_i <- smooth.spline(trace$time, trace$depth, 
                                  spar = spar_seq[i], nknots = 5900)
    spar_gcv[i] <- smooth_fit_i$crit
    predict_i <- predict(smooth_fit_i, trace$time)$y
    data_i <- cbind(data_i, predict_i)
  }
  # organizing and tidying for final graph 
  spar_names <- paste("spar_", spar_seq, sep = "")
  spar_names <- paste(spar_names, round(spar_gcv, 2), sep = "_")
  names(data_i)[(ncol(trace)+1):ncol(data_i)] <- spar_names
  # just grabbing spar values, time, and depth for graphing
  just_spar <- data_i[, grep("^(s|d|t)", names(data_i))]
  # pivor longer for easier graphing
  spar_long <- tidyr::pivot_longer(just_spar, spar_names)
  # returning the final output 
  return(spar_long)
}
