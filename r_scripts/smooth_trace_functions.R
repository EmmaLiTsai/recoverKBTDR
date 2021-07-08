# very simple function for spline smoothing of trace data 
# Function likely deprecated, replaced by smooth_trace_dive()
smooth_trace <- function(trace, spar = 0.3){ 
  # finding the number of knots to use for the record based on the record length
  nknots = signif(nrow(trace) * .03, 1)
  # need to order first: 
  trace <- trace[order(trace$time),]
  # creating smoothing fit with the spar and knots the user defines
  smooth_fit <- smooth.spline(trace$time, trace$depth, 
                              spar = spar, nknots = nknots)
  # adding the smoothed predict values to the trace data frame 
  trace$smooth_depth <- predict(smooth_fit, trace$time)$y
  
  # removing extra noise
  trace[trace$smooth_depth < 0,]$smooth_depth <- 0
  
  # return trace data 
  return(trace)
}

# Another possible spline smoothing function: 
# Function likely deprecated, replaced by smooth_trace_dive()
# More complex spline smoothing function with depth bounds. This function
# smooths at a lower resolution for depths < depth_bound (defaults set to 
# spar = 0.8 and 1000 knots), and higher resolution at depths > depth_bound 
# (defaults set to spar = 0.3 and 5900 knots). This is an attempt to reduce 
# chatter in the transducer arm when the seal is resting or hauled out, while 
# keeping higher resolution in the dives at depth to retain wiggles. This 
# method would be sound considering there would be less tension on the 
# transducer arm at shallow depths, which produced unnecessary noise in the 
# record. Here, I also added dive component assignment. 
smooth_trace_bounded <- function(trace, spar_h =  0.3, depth_bound = 5){ 
  
  # creating spar and nknots values for low depth values (spar = 0.8 and 
  # nknots = 1000), and high depth values (spar = spar_h, and nknots depending
  # on the length of ht record)
  spar = c(0.8, spar_h)
  nknots = c(1000, signif(nrow(trace) * .03, 1))
  
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
  
  # removing excess noise 
  smooth_trace[smooth_trace$smooth_depth < 0,]$smooth_depth <- 0
  
  # final return 
  return(smooth_trace)
}

## another possible smoothing method using a dive detector ## 
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
#   - smooth_trace : trace data frame with smoothed values (smooth_depth), and 
#                    also with dive component assignment. 
###############################################################################
smooth_trace_dive <- function(trace, spar_h = 0.3, depth_thresh = 5){
  # defining spar, nknots, and window values: 
  spar = c(0.8, spar_h)
  nknots = c(1000, signif(nrow(trace) * .03, 1)) 
  window = signif(floor(nrow(trace) * 0.002), 1)
  
  # ordering 
  trace <- trace[order(trace$time),]
  # detecting a bout of dives using the runmean function on a window of the 
  # data: 
  detect_bout <- data.frame(runmean = (caTools::runmean(trace$depth, window)), 
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
  # added final smoothing and dive component assignment -- this can be removed 
  # later but I was experimenting with it here
  smooth_trace <- dplyr::mutate(smooth_trace, 
                                smooth_depth = predict(spline_mod_bout, smooth_trace$time)$y,
                                deriv = predict(spline_mod_bout, smooth_trace$time, deriv=1)$y,
                                ascent = deriv < 0,
                                deriv_diff = lag(sign(deriv)) - sign(deriv),
                                peak = case_when(deriv_diff < 0 ~ "TOP",
                                                 deriv_diff > 0 ~ "BOTTOM"))
  # removing extra column 
  smooth_trace <- smooth_trace[,!(names(smooth_trace) %in% c("smooth"))]
  # removing excess noise at the surface 
  smooth_trace[smooth_trace$smooth_depth < 0,]$smooth_depth <- 0
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
    spar_gcv[i] <- smooth_fit_i$cv.crit
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
  spar_long <- tidyr::pivot_longer(just_spar, all_of(spar_names))
  # returning the final output 
  return(spar_long)
}
