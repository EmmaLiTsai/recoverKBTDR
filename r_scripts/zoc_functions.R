# Topic: zero offset correcting (zoc) the data before arc removal ##############
# Date: 5/20/2021 
# 
# Zoc would make arc removal more reliable since the arc equation assumes 
# that depth = 0 is the same as y = 0, but there is (slight) drift in y = 0 
# for some of the records. This issue also happens with modern TDRs, but zero
# offset correction is needed before the arc in the data is removed. While the 
# sample data in this repo is somewhat free of this drift, there are a few older 
# records with extreme level shifts and drift in depth = 0 within a bout of 
# dives. Therefore, I thought it might be helpful to include this file in the 
# github repo.  

###############################################################################
# Function: zoc(trace, k_h = 500, depth_bounds = c(-1, 1))
# Author:   EmmaLi Tsai
# Date:     6/1/2021
# 
# Function takes the tidy trace to perform a zero-offset correction using the 
# recursive filtering method that can be found in Luque & Fried, 2011: Recursive 
# filtering for zero offset correction of diving depth time series with GNU R 
# package diveMove. 
# 
# Here, I modified some code from the diveMove package on GitHub to work with 
# the tidy trace data. This code relies on the caTools package that specializes 
# in moving window statistics, and the code I wrote within the zoc function is 
# modeled after the code in the .depthFilter function on the GitHub page. His 
# .depthFilter function runs on S4 objects, so I had to modify the code here for 
# the simple trace data and so I could continue working with the data (i.e., 
# remove arc, add times, depth calibration, etc.) before final dive analysis. 
# 
# Input: 
# 
#   - trace       : tidy trace data frame, contains the x and y values of the 
#                   trace
#
#   - k_h         : size of the larger window used for the second filter. This 
#                   default is set to 500, but should be smaller for records 
#                   with extreme drift 
# 
#   - depth_bounds: restricted search window for where y = 0 should likely 
#                   be. For the records, this should be in cm. Default set 
#                   c(-1, 1).
# 
# Output: 
#   - zoc_trace   : trace data frame after it has been zero-offset corrected
###############################################################################
zoc <- function(trace, k_h = 500, depth_bounds = c(-1, 1)){
  
  # defining the window sizes. k_h is the larger window, and needs to be smaller 
  # for records with extreme drift 
  k = c(2, k_h)
  # probabilities to use for the first and second quantiles. This is constant 
  # across all records. 
  probs = c(0.5, 0.02)
  
  # logical vector if there is an NA depth
  d_na <- is.na(trace$y_val)
  # seeing if the depth is in the bounds of where y = 0 should likely be:
  d_in_bounds <- trace$y_val > depth_bounds[1] & trace$y_val < depth_bounds[2]
  # grabbing the depths that fall into the category above or is NA:
  d_ok <- which(d_in_bounds | is.na(trace$y_val)) # numeric 
  # creating a matrix of y_values for each filter: 
  filters <- matrix(trace$y_val, ncol = 1)
  
  # for each k window: 
  for (i in seq(length(k))) {
    # add a column of y values from the trace df
    filters <- cbind(filters, trace$y_val)
    # grab the depths that are near depth = 0 for the window:
    dd <- filters[d_ok, i]
    # calls the runquantile function, which creates a rolling quantile across the 
    # window. This function also relies of the EndRule function which takes the 
    # result of runquantile and transforms it into a vector.
    filters[d_ok, i + 1] <- caTools::runquantile(dd, k=k[i], probs=probs[i])
    
    ## Linear interpolation for depths out of bounds-- in other words,
    # approximate the position of depth = 0 when the seal is diving at greater 
    # depths 
    offbounds <- which(!d_in_bounds)
    offbounds_fun <- approxfun(seq(length(trace$y_val))[d_in_bounds],
                               filters[d_in_bounds, i + 1], rule=2)
    filters[offbounds, i + 1] <- offbounds_fun(offbounds)
    
    ## NA input should be NA output regardless of na.rm
    filters[d_na, i + 1] <- NA
  }
  
  # finding the depth adjustment by taking the y value and subtracting by the 
  # filters
  depth_adj <- trace$y_val - filters[, ncol(filters)]
  # binding new depth adjustment with original trace data 
  # in the future, may need to subtract the depth adjustment by a small amount 
  # to account for the thickness of the trace.
  zoc_trace <- cbind(trace$x_val, depth_adj)
  # transforming to df 
  zoc_trace <- as.data.frame(zoc_trace)
  # changing names for future functions: 
  names(zoc_trace) <- c("x_val", "y_val")
  
  # adding small amount to account for thickness of the trace: 
  shallow <- zoc_trace[which(zoc_trace$y_val < 0.5),]
  shallow_diff_y <- dplyr::group_by(shallow, x_val) %>% summarize(diff_y = diff(y_val), .groups = "drop")
  zoc_offset <- mean(shallow_diff_y$diff_y)
  
  # adding the offset 
  zoc_trace$y_val <- zoc_trace$y_val + zoc_offset
 
  # returning the final output 
  return(zoc_trace)
}

# This function is intended to help zoc traces that have extreme drift in depth 
# = 0. I only have two traces that require this extra correction, but I figured 
# this might be helpful with future package development. Essentially, it adds 
# another offset correction before the zoc() function is called, using a rolling 
# min with window = 2000. This created a jagged line that represents the drift 
# in y = 0 across the record. Then, the rolling min vector is smoothed out using 
# a rolling mean of the same window, to help smooth out the minimum line for 
# y-val correction (similar to the centering scan process). Then, the centered 
# record is corrected using the original zoc function. 
zoc_big_drift <- function(trace, k_h = 500, depth_bounds = c(-1, 1)){
  
  # defining the window sizes. k_h is the larger window, and needs to be smaller 
  # for records with extreme drift.  
  k = c(2, k_h)
  # probabilities to use for the first and second quantiles. This is constant 
  # across all records. 
  probs = c(0.5, 0.02)
  
  # detecting drift line across the record 
  min <- caTools::runmin(trace$y_val, k = 2000)
  mean <- caTools::runmean(min, k = 2000)
  # creating a data frame that has the needed y_val correction 
  drift_correct <- data.frame(x_val = trace$x_val, y_val = trace$y_val, zoc_y = mean)
  drift_correct$corr <- drift_correct$y_val - drift_correct$zoc_y
  # changing names for zoc 
  drift_correct <- drift_correct[, names(drift_correct) %in% c("x_val", "corr")]
  names(drift_correct) <- c("x_val", "y_val")
  
  # final zoc 
  zoc_trace <- zoc(drift_correct, k = k, probs = probs, depth_bounds = depth_bounds)
  
  # final return 
  return(zoc_trace)
}
