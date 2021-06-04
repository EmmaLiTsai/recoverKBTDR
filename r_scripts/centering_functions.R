###############################################################################
# Function: old_center_scan(trace, time_dots, dist_timedot = 1.1)
# Author:   EmmaLi Tsai
# Date:     3/27/21
# 
# Function takes the trace and timedots files and uses the y values of the 
# time dots to move the trace up/down to center the scan, such that all the 
# y-values of the time dots are along y = -dist_timedot that the user defines in 
# the function call. This function does a fuzzy full distance merge based on the 
# x_val columns in the trace and time_dots data frame, and then calculates how 
# much the y values of the trace need to be corrected to center the scan. 
# 
# This function was needed to ensure that any drift in the trace would be from 
# the TDR and not from scanning. This drift is common with modern TDRs and can 
# be easily handled with the diveMove package with zero offset correction 
# filters (see Luque & Fried, 2011). 
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
#   - fuzzy_merge_trace : fuzzy merged trace with centered y values and original
#                         x values of the scan. 
###############################################################################
old_center_scan <- function(trace, time_dots, dist_timedot = 1.1){
  # first, I needed to find the appropriate merge distance. This distance should 
  # be large enough to merge with trace values between time dots that are far 
  # apart. However, I remove duplicated values later in this function, so 
  # large values also produce a lag in centering...
  merge_dist <- max(abs(diff(time_dots$x_val)))
  
  # this is a fuzzy distance merge that I did using the "fuzzyjoin" package. 
  # it produces a data frame with x and y values of the trace and the connected
  # x and y values of the trace (headers x_val.y and Y, respectively). 
  
  # I needed this in order to use the y values of the time dots to center the 
  # scan.
  fuzzy_merge_trace <- fuzzyjoin::difference_left_join(trace, time_dots, 
                                                       by = c("x_val"), 
                                                       max_dist = merge_dist)
  
  # calculating how far the y time dot values are from the dist_timedot value. 
  # this was needed to move the y values of the trace up/down such that the 
  # time dots are centered along the same horizontal line of y = dist_timedots. 
  
  # case_when function was needed to account for when the time dots might be > 0! 
  fuzzy_merge_trace$y_val_corr <- dplyr::case_when(fuzzy_merge_trace$y_val.y < 0 ~ (abs(fuzzy_merge_trace$y_val.y) - dist_timedot), 
                                                   fuzzy_merge_trace$y_val.y > 0 ~ ((-dist_timedot) - abs(fuzzy_merge_trace$y_val.y)),
                                                   fuzzy_merge_trace$y_val.y == 0 ~ (abs(fuzzy_merge_trace$y_val.y) - dist_timedot))
  
  # centering the scan using the above y corrected values  
  fuzzy_merge_trace$center_y <- fuzzy_merge_trace$y_val.x + fuzzy_merge_trace$y_val_corr
  
  # removing duplicated values -- this happened when a point along a trace 
  # was close to both time dots. This code will keep the first duplicated value 
  # but remove the trailing ones. Since any errors due to scanning would be 
  # gradual and the time dots are usually ~1.2 cm apart, I think this method 
  # should be sound 
  # resolved issue #11 in GitHub repo:
  fuzzy_merge_trace <- fuzzy_merge_trace[!duplicated(fuzzy_merge_trace[,1:2]),]
  
  # some final tidying -- this is removing unimportant columns resulting from 
  # the merge and giving these columns meaningful names. 
  fuzzy_merge_trace <- fuzzy_merge_trace[,c(1,6)]
  names(fuzzy_merge_trace) <- c("x_val", "y_val")
  
  # returning centered y-values 
  return(fuzzy_merge_trace)
}

## New Method created by Dr. Schwilk: 

# Simple rolling mean function. Window size is n. function returns a vector
# that is shorter than original and does not pad with NAs. An alternative would
# be create a rolling mean fx with filter():
# ma <- function(x, n = 2){stats::filter(x, rep(1 / n, n), sides = 2)}
# But I suspect cumsum is faster and I like not getting the NAs.
rollmean <- function(x, n) {
  cx <- c(0 , cumsum(x))
  return((cx[(n+1):length(cx)] - cx[1:(length(cx) - n)]) / n)
}


center_scan <- function(trace, time_dots, dist_timedot = 1.1) {
  
  # Replacing slow fuzzy merge with simple cut operation. First step is to find
  # x midpoints between time dots to use for cutting
  cutpoints <- c(0, rollmean(time_dots$x_val, 2), max(trace$x_val))
  # Then cut to assign every trace point an index from the time_points df:
  time_dot_indices <- cut(trace$x_val, breaks=cutpoints, labels=FALSE)
  # Now do the adjustment
  trace$y_val <- trace$y_val - time_dots$y_val[time_dot_indices] - dist_timedot
  
  return(trace)
}






################################################################################
# Topic: Centering methods for records with time dot issues 
# Date: 5/5/2021
################################################################################

# This function centers records with time dot issues, where the time dots were 
# too far apart for center_scan() to effectively center the trace. 

###############################################################################
# Function: center_scan_td_issue(trace, time_dots, dist_timedot = 1.1, merge_dist = 0.4)
# 
# Centering for unique scans with time dots that are particularly far apart
# (i.e., man-made time dots because the time dots were unreliable in the record. 
# This was needed for 2 records out of the 20 that I have in the lab). This 
# function creates a line between two time dots and creates filler time dots 
# along the line to make centering more reliable for these records. Basic 
# structure of the code is modeled after the center_scan() function in the
# scan_tidying_functions.R file 
# 
# Input: 
#     - trace       : tidy trace file 
#
#     - time_dots   : tidy time dots file 
#
#     - dist_timedot: the negative y value the time dots should center along 
#                     (i.e., all time dots will be centered along the line 
#                      y = -dist_timedot). Default set to 1.1cm.
#
#     - merge dist  : distance used to create a sequence of filler points and 
#                     also for the fuzzy merge. Default set to 0.4cm. 
# 
# Output: 
#     - fuzzy_merge_trace : fuzzy merged trace with centered y values and 
#                           original x values of the scan. 
###############################################################################
center_scan_td_issue <- function(trace, time_dots, dist_timedot = 1.1, merge_dist = 0.4){
  
  # creating helper data frame: 
  center_dots <- data.frame(x1 = time_dots$x_val, 
                            y1 = time_dots$Y, 
                            x2 = lead(time_dots$x_val), 
                            y2 = lead(time_dots$Y))
  
  # adding a slope value and intercept to add filler time dots between two 
  # points: 
  center_dots$slope <- ((center_dots$y2 - center_dots$y1) / (center_dots$x2 - center_dots$x1))
  center_dots$intercept <- center_dots$y1 - (center_dots$slope * center_dots$x1)
  # this happened at the end of the trace due to lead() function 
  center_dots <- tidyr::drop_na(center_dots)
  
  # creating empty data frame
  center_seq <- data.frame()
  # looping through data frame:
  for (i in 1:nrow(center_dots)){
    # empty df for storing loop values 
    center_seq_loop <- data.frame()
    # generating sequence to create filler time dots for centering 
    center_seq_loop <- data.frame(x_val = seq(center_dots$x1[i], 
                                              center_dots$x2[i], 
                                              by = merge_dist))
    # creating y values using the slope and intercept values between two points
    center_seq_loop$Y <- (center_dots$slope[i] * center_dots$x1[i]) + center_dots$intercept[i]
    # rbinding to original df 
    center_seq <- rbind(center_seq, center_seq_loop)
  }
  
  # code taken from center_scan() in scan_tidying_functions.R: 
  # this is a fuzzy distance merge that I did using the "fuzzyjoin" package. 
  # it produces a data frame with x and y values of the trace and the connected
  # x and y values of the trace (headers x_val.y and Y, respectively). 
  
  # I needed this in order to use the y values of the time dots to center the 
  # scan.. 
  
  # TODO: sometimes R will return: cannot allocate vector of large size for
  # some of the larger files. Look into R memory storage. 
  fuzzy_merge_trace <- fuzzyjoin::difference_left_join(trace, center_seq, 
                                                       by = c("x_val"), 
                                                       max_dist = merge_dist)
  
  # calculating how far the y time dot values are from the dist_timedot value. 
  # this was needed to move the x or y values up or down to center the scan 
  # this has been fixed to account for when the time dots might be > 0! 
  fuzzy_merge_trace$y_val_corr <- dplyr::case_when(fuzzy_merge_trace$Y > 0 ~ (abs(fuzzy_merge_trace$Y) - dist_timedot), 
                                                   fuzzy_merge_trace$Y < 0 ~ ((-dist_timedot) - abs(fuzzy_merge_trace$Y)),
                                                   fuzzy_merge_trace$Y == 0 ~ (abs(fuzzy_merge_trace$Y) - dist_timedot))
  
  # centering the scan using the above y corrected values  
  fuzzy_merge_trace$center_y <- fuzzy_merge_trace$y_val + fuzzy_merge_trace$y_val_corr
  
  # removing duplicated values -- this happened when a point along a trace 
  # was close to both time dots, and the way this code is set up it will keep 
  # the first duplicated value but remove the trailing ones. 
  fuzzy_merge_trace <- fuzzy_merge_trace[!duplicated(fuzzy_merge_trace[,1:2]),]
  
  # some final tidying -- this is removing unimportant columns resulting from 
  # the merge and giving these columns meaningful names. 
  fuzzy_merge_trace <- fuzzy_merge_trace[,c(1,6)]
  names(fuzzy_merge_trace) <- c("x_val", "y_val")
  
  # returning centered y-values 
  return(fuzzy_merge_trace)
}


