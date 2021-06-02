# Topic: zero offset correcting (zoc) the data before arc removal ##############
# Date: 5/20/2021 
# 
# Still a work in progress! 
# 
# Zoc would make arc removal more reliable since the arc equation assumes 
# that depth = 0 is the same as y = 0, but there is (slight) drift in y = 0 
# for some of the records. This issue also happens with modern TDRs, but zero
# offset correction is needed before the arc in the data is removed. While the 
# sample data in this repo is somewhat free of this drift, there are a few older 
# records with extreme level shifts and drift in depth = 0 within a bout of 
# dives. Therefore, I thought it might be helpful to include this (still 
# preliminary) file in the github repo.  
# 
# in order to remove the diveMove dependency, I think I will need to code 
# something in C. The error I'm having is in the .C() line in the .runquantile 
# function... 

# I think I would have to compile it with R CMB SHLIB and then load it as a 
# dynamic library in dyn.load("../src/run_quantile.c")
# or use rcpp package to directly write c code?

###############################################################################
# Function: zoc(trace, k = c(3, 500), probs = c(0.5, 0.02), depth.bounds = c(-5, 1))
# Author:   EmmaLi Tsai
# Date:     6/1/2021
# 
# Function takes the tidy trace to perform a zero-offset correction using the 
# recursive filtering method that can be found in Luque & Fried, 2011: Recursive 
# filtering for zero offset correction of diving depth time series with GNU R 
# package diveMove. 
# 
# Here, I modified some code from the diveMove package on GitHub to work with 
# the tidy trace data. This code relies on two functions that I pulled from the 
# page (.runquantile, and .EndRule-- both originally from the caTools package 
# that specializes in moving window statistics. Both present at the end of this 
# file), and the code I wrote within the zoc function is modeled after the code  
# in the .depthFilter function on the GitHub page. His .depthFilter function 
# runs on S4 objects, so I had to modify the code here for the simple trace
# data and so I could continue working with the data (i.e., remove arc, add 
# times, depth calibration, etc.) before final dive analysis. 
# 
# Input: 
# 
#   - trace       : tidy trace data frame, contains the x and y values of the 
#                   trace
#   
#   - depth.bounds: restricted search window for where depth = 0 should likely 
#                   be. For the records, this should be in cm. Defualt set 
#                   c(-5, 1)
#
#   - k           : size of window used for first and second filters. 
#                   default set to c(3, 500)
#
#   - probs       : quantiles to extract for each step of k. 
#                   default set to c(0.5, 0.02)
#
# Output: 
#   - zoc_trace   : trace data frame after it has been zero-offset corrected
###############################################################################
zoc <- function(trace, k = c(3, 500), probs = c(0.5, 0.02), depth.bounds = c(-5, 1)){
  
  # logical vector if there is an NA depth
  d.na <- is.na(trace$y_val)
  # seeing if the depth is in the bounds of where y = 0 should likely be:
  d.in.bounds <- trace$y_val > depth.bounds[1] & trace$y_val < depth.bounds[2]
  # grabbing the depths that fall into the category above or is NA:
  d.ok <- which(d.in.bounds | is.na(trace$y_val)) # numeric 
  # creating a matrix of y_values for each filter: 
  filters <- matrix(trace$y_val, ncol = 1)
  
  # for each k window of smoothing: 
  for (i in seq(length(k))) {
    # add a column of y values from the trace df
    filters <- cbind(filters, trace$y_val)
    # grab the depths that are near depth = 0 for the window of smoothing:
    dd <- filters[d.ok, i]
    # calls the runquantile function, which creates a rolling quantile across the 
    # window. This function also relies of the EndRule function which takes the 
    # result of runquantile and transforms it into a vector.
    filters[d.ok, i + 1] <- caTools::runquantile(dd, k=k[i], probs=probs[i])
    
    ## Linear interpolation for depths out of bounds-- in other words,
    # approximate the position of depth = 0 when the seal is diving at greater 
    # depths 
    offbounds <- which(!d.in.bounds)
    offbounds.fun <- approxfun(seq(length(trace$y_val))[d.in.bounds],
                               filters[d.in.bounds, i + 1], rule=2)
    filters[offbounds, i + 1] <- offbounds.fun(offbounds)
    
    ## NA input should be NA output regardless of na.rm
    filters[d.na, i + 1] <- NA
  }
  
  # finding the depth adjustment by taking the y value and subtracting by the 
  # filters
  depth.adj <- trace$y_val - filters[, ncol(filters)]
  # binding new depth adjustment with original trace data 
  # I subtracted the depth adjustment by a small amount to account for the  
  # thickness of the trace.
  zoc_trace <- cbind(trace$x_val, (depth.adj - 0.15))
  # transforming to df 
  zoc_trace <- as.data.frame(zoc_trace)
  # changing names for future functions: 
  names(zoc_trace) <- c("x_val", "y_val")
  
  # returning the final output 
  return(zoc_trace)
}
