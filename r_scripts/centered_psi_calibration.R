centered_psi_calibration <- function(trace, start_row, psi_interval = c(100, 200, 400, 600, 800)){
  # snipping the tail end where the user defines
  trace_snip <- trace[start_row:nrow(trace),]
  # grouping by rounded by value and finding the mean
  psi_summary <- group_by(trace_snip, mean(y_val)) %>% summarize(mean = mean(y_val))
  # recursive grouping to help handle values between two integers
  psi_simple <- group_by(psi_summary, floor(mean)) %>% summarize(mean = mean(mean))
  # cutting values that wouldn't be with the psi calibraton 
  psi_simple <- psi_simple[psi_simple$`floor(mean)` > 0,]
  # making psi data frame
  psi_calibration <- data.frame(psi_interval = psi_interval, 
                                psi_position = psi_simple$mean)
  
  return(psi_calibration)
}

