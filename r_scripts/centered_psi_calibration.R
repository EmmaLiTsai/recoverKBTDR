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
  psi_summary <- dplyr::group_by(trace_snip, round(y_val)) %>% summarize(mean = mean(y_val))
  # recursive grouping to help handle values between two integers
  psi_simple <- dplyr::group_by(psi_summary, floor = floor(mean)) %>% summarize(mean = mean(mean))
  # cutting values that wouldn't be with the psi calibration 
  psi_simple <- psi_simple[psi_simple$floor > 0,]
  
  # some extra filtering to handle values that are close but were not captured 
  # by the grouping functions above
  
  # creating a helper data frame to make future calculations easier 
  final_filter <- data.frame(segment = psi_simple$floor, mean = psi_simple$mean, lag = lag(psi_simple$mean))
  final_filter$diff <- final_filter$mean - final_filter$lag
  
  # trying to catch larger values that might not have been grouped 
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

