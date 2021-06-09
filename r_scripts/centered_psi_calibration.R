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
  return(psi_calibration)
}

