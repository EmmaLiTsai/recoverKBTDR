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
# keeping higher resolution in the dives at depth to retain wiggles.
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
  
  # recombinig the two: 
  smooth_trace <- rbind(trace_shallow, trace_deep)
  # ordering 
  smooth_trace <- smooth_trace[order(smooth_trace$time),]
  
  # recursive smoothing -- final smoothing 
  smooth_trace_2 <- smooth.spline(smooth_trace$time, smooth_trace$smooth, 
                                  spar = spar[2], nknots = nknots[2])
  smooth_trace$smooth_2 <- predict(smooth_trace_2, smooth_trace$time)$y
  
  # final return 
  return(smooth_trace)
}


