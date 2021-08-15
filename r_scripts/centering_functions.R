###############################################################################
# Function: center_scan(trace, time_dots, dist_timedot = 1.1)
# Author:   Dr. Dylan W. Schwilk
#  
# Function takes the trace and timedots files and uses the y values of the 
# time dots to move the trace up/down to center the scan, such that all the 
# y-values of the time dots are along y = -dist_timedot that the user defines in 
# the function call. It basically creates a rolling mean function on the x_vals 
# of the timing gots to to cut the x_vals of the trace data frame, which allows 
# for centering. This function is a large improvement from the original fuzzy 
# join methods, and also works on records with time dot issues. 
#
# This function was needed to ensure that any drift in the trace would be from 
# the TDR and not from scanning. This drift is common with modern TDRs and can 
# be easily handled in future zoc functions. 
#
# Input: 
#   - trace        : tidy trace file 
#   - time_dots    : tidy time_dots file
#   - dist_timedot : the y-axis the user would like to use to center the scan. 
#                    The trace will be centered such that all time dots will 
#                    fall along y = -dist_timedots. Default is set to 1.1cm from 
#                    my own personal measurements, but this value varies between 
#                    records. 
#
# Output: 
#   - trace : centered trace with two columns: x_val and y_val
###############################################################################
center_scan <- function(trace, time_dots, dist_timedot = 1.1) {
  
  # Replacing slow fuzzy merge with simple cut operation. First step is to find
  # x midpoints between time dots to use for cutting
  cutpoints <- c(0, rollmean(time_dots$x_val, 2), max(trace$x_val))
  # Then cut to assign every trace point an index from the time_points df:
  time_dot_indices <- cut(trace$x_val, breaks = cutpoints, labels = FALSE)
  # Now do the adjustment
  trace$y_val <- trace$y_val - time_dots$y_val[time_dot_indices] - dist_timedot
  
  return(trace)
}

# Simple rolling mean function wrapped inside center_scan above. Window size is 
# n. function returns a vector that is shorter than original and does not pad 
# with NAs. An alternative would be create a rolling mean fx with filter():
# ma <- function(x, n = 2){stats::filter(x, rep(1 / n, n), sides = 2)}
# But I suspect cumsum is faster and I like not getting the NAs.
rollmean <- function(x, n) {
  cx <- c(0, cumsum(x))
  return((cx[(n+1):length(cx)] - cx[1:(length(cx) - n)]) / n)
}

###############################################################################
# Function: centered_psi_calibration(trace, psi_interval = c(100, 200, 400, 600, 800))
# Author:   EmmaLi Tsai
# Date:     6/09/21
# 
# This function takes the centered trace file to find the new centered values 
# of the psi calibration intervals at the end of the record. This is supposed 
# to be an improved method from the original, where the psi calibration 
# intervals were measured on the scanned image in ImageJ before centering. 
# Although the change in the position of the psi intervals is slight, I thought 
# this should make future depth calculations more precise.  
# 
# Input: 
# 
#   - trace       : tidy trace data frame after centering, contains the x and y 
#                   values of the trace.
#
#   - psi_intervals : a numeric vector of the psi intervals at the end of the 
#                     record. Default is set to 100, 200, 400, 600, and 800 psi. 
#   
# Output: 
#   - psi_calibration : data frame that contains the new psi positions after 
#                       the trace had been centered
###############################################################################
centered_psi_calibration <- function(trace, psi_interval = c(100, 200, 400, 600, 800)){
  # grabbing the last chunk of data in the trace 
  start_row <- nrow(trace) - 2000
  # snipping the tail end of the record to capture the psi calibration 
  trace_snip <- trace[start_row:nrow(trace),]
  # grouping by rounded y-value and finding the mean
  psi_summary <- dplyr::group_by(trace_snip, round(y_val)) %>% summarize(mean = mean(y_val), .groups = "drop")
  # recursive grouping to help handle values between two integers
  psi_simple <- dplyr::group_by(psi_summary, floor = floor(mean)) %>% summarize(mean = mean(mean), .groups = "drop")
  # cutting values that wouldn't be with the psi calibration 
  psi_simple <- psi_simple[psi_simple$floor > 0,]
  
  # some extra filtering to handle values that are close but were not captured 
  # by the grouping functions above. I do this by taking the difference between 
  # the two intervals using the lag() function and creating another filter 
  
  # creating a helper data frame to make future calculations easier 
  final_filter <- data.frame(segment = psi_simple$floor, 
                             mean = psi_simple$mean, 
                             lag = lag(psi_simple$mean))
  # finding the difference between intervals 
  final_filter$diff <- final_filter$mean - final_filter$lag
  
  # trying to catch values that are close but might not have been grouped in the 
  # filters above using the difference between intervals 
  final_psi <- dplyr::case_when(final_filter$diff < 1 && final_filter$segment > 2 ~ mean(c(final_filter$mean, final_filter$lag)), 
                                final_filter$diff > 1 | is.na(final_filter$diff) ~ final_filter$mean)
  # omit if NAs were produced 
  final_psi <- na.omit(final_psi)
  # making psi data frame
  psi_calibration <- data.frame(psi_interval = psi_interval, 
                                psi_position = final_psi)
  # returning final data frame 
  return(psi_calibration)
}

