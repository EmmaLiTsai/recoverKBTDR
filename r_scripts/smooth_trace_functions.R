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
# record
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
  spline.mod <- smooth.spline(smooth_trace$time, smooth_trace$smooth, 
                                  spar = spar[2], nknots = nknots[2])
  # added final smoothing and dive component assignment 
  smooth_trace <- dplyr::mutate(smooth_trace, 
                                smooth_depth = predict(spline.mod, smooth_trace$time)$y,
                                deriv = predict(spline.mod, smooth_trace$time, deriv=1)$y,
                                ascent = deriv < 0,
                                deriv_diff = lag(sign(deriv)) - sign(deriv),
                                peak = case_when(deriv_diff < 0 ~ "TOP",
                                                 deriv_diff > 0 ~ "BOTTOM"))
  # removing extra column 
  smooth_trace <- smooth_trace[,!(names(smooth_trace) %in% c("smooth"))]
  
  # final return 
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
