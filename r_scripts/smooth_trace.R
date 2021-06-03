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
