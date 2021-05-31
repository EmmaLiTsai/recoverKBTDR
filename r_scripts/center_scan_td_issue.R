################################################################################
# Topic: Centering methods for records with time dot issues 
# Date: 5/5/2021
################################################################################

# This function centers records with time dot issues, where the time dots were 
# too far apart for center_scan() to effectively center the trace. 

# First: tidying the trace and time_dots file from scan_tidying_functions.R file
time_dots <- tidy_timedots(time_dots)
trace <- tidy_trace(trace)

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
#     - time_dots   : tidy time dots file 
#     - dist_timedot: the negative y value the time dots should center along 
#                     (i.e., all time dots will be centered along the line 
#                      y = -dist_timedot). Default set to 1.1cm.
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
# calling the function 
center_scan_td_issue <- center_scan_td_issue(trace, time_dots)

# Visualizing it -- comparing centering with original record, the record after 
# center_scan, and the record after center_scan_long. Center_scan_long should 
# be an improvement from both methods. 
# ggplot(center_trace, aes(x = x_val, y = y_val)) + geom_point() + 
#   geom_point(data = center_scan_td_issue, aes(x = x_val, y = y_val), color = "red") + 
#   geom_point(data = trace, aes(x = x_val, y = y_val), color = "blue")

