################################################################################
# Data tidying code and centering the scan 
# Date: 3/26/2021
################################################################################

# This file contains all extra tidying and centering code to clean up the scans
# All of these preliminary steps are necessary in order to continue with the 
# recovery process. 

################################################################################
# Outline of tidying steps: 
# 
#     Step 1: Tidy the trace and time_dots file headers, names, and y-values due 
#             to defaults from ImageJ. 
#             Trace is tidy using the tidy_trace() function. 
#             Time dots is tidy using the tidy_timedots() function. 
#
#     Step 2: Centering the scan. This was necessary because my hand was moving 
#             while the trace was being fed into the scanner, and therefore 
#             needed centering. 
#
# ##############################################################################

## STEP ONE #################################################################### 
# Code below achieves step 1 of the cleaning and file tidying process: 
################################################################################
# Function: tidy_trace(trace)
# Author:   EmmaLi Tsai
# Date:     3/27/21
# 
# Function takes the default column names from ImageJ and image processing and
# does some basic tidying so that the code is easier/cleaner to work with. This
# involves selecting certain columns and changing name values. 
# 
# Input: 
#   - trace   : raw csv file from ImageJ after image processing 
#
# Output: 
#   - trace   : tidy csv file with correct columns and column names 
###############################################################################
tidy_trace <- function(trace){
  # changing the corrected y-value-- this was needed from the odd way ImageJ 
  # handles origin placement, where values in the +y direction are negative and 
  # values in the -y direction are positive. 
  trace$y_corr <- dplyr::case_when(trace$Y < 0 ~ abs(trace$Y),
                                   trace$Y > 0 ~ -(trace$Y), 
                                   trace$Y == 0 ~ (trace$Y))
  
  # selecting the correct columns and removing unnecessary ones 
  trace <- dplyr::select(trace, c("X", "y_corr"))
  # changing the name values 
  names(trace) <- c("x_val", "y_val")
  # ordering the trace by increasing x value
  trace <- trace[order(trace$x_val),]
  # returning trace 
  return(trace)
}
###############################################################################
# Function: tidy_timedots(time_dots)
# Author:   EmmaLi Tsai
# Date:     3/27/21
# 
# Function takes the default column names from ImageJ and image processing and
# does some basic tidying so that the code is easier/cleaner to work with. This
# involves selecting certain columns and changing name values, and fixing the 
# y values after origin placement. This function also adds a new row to the 
# time dots csv file such that the first value in the trace csv file becomes the 
# first time dot-- this was needed for centering, time functions, and also was 
# the way the previous group in the 90's analyzed these traces. 
#
# Input: 
#   - time_dots   : raw csv file from ImageJ after image processing 
#
# Output: 
#   - time_dots   : tidy csv file with correct columns and column names 
###############################################################################
tidy_timedots <- function(time_dots){
  # selecting the correct columns for these data
  time_dots <- dplyr::select(time_dots, c("X", "Y"))
  # creating the first time dot, which is when the trace begins. There is no time 
  # dot in the trace when it first starts gathering data, so I had to add one 
  # here: 
  time_dots <- rbind(c(0, time_dots$Y[1]), time_dots)
  # changing names
  names(time_dots) <- c("x_val", "Y")
  # changing y-values due to odd ImageJ origin placement
  time_dots$Y <- dplyr::case_when(time_dots$Y < 0 ~ abs(time_dots$Y),
                                  time_dots$Y > 0 ~ -(time_dots$Y), 
                                  time_dots$Y == 0 ~ (time_dots$Y))
  # returning tidy time_dots file 
  return(time_dots)
}

# applying the changes
time_dots <- tidy_timedots(time_dots)
trace <- tidy_trace(trace)

# STEP TWO CODE ###############################################################
# The code below centers the scan to achieve step two of this file. 

###############################################################################
# Function: center_scan(trace, time_dots, dist_timedot = 1.1)
# Author:   EmmaLi Tsai
# Date:     3/27/21
# 
# Function takes the trace and timedots csv files and uses the y values of the 
# time dots to move the traces up/down to center the scan, such that all the 
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
#   - trace        : tidy trace csv file 
#   - time_dots    : tidy time_dots csv file
#   - dist_timedot : the y-axis the user would like to use to center the scan. 
#                    The trace will be centered such that all time dots will 
#                    fall along y = -dist_timedots. Default is set to 1.1cm from 
#                    my own personal measurements, but this value varies between 
#                    records. 
#
# Output: 
#   - fuzzy_merge_trace : fuzzy merged trace with centered y values and original
#                    x values of the scan. 
###############################################################################
center_scan <- function(trace, time_dots, dist_timedot = 1.1){
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
  fuzzy_merge_trace$y_val_corr <- dplyr::case_when(fuzzy_merge_trace$Y < 0 ~ (abs(fuzzy_merge_trace$Y) - dist_timedot), 
                                                   fuzzy_merge_trace$Y > 0 ~ ((-dist_timedot) - abs(fuzzy_merge_trace$Y)),
                                                   fuzzy_merge_trace$Y == 0 ~ (abs(fuzzy_merge_trace$Y) - dist_timedot))
  
  # centering the scan using the above y corrected values  
  fuzzy_merge_trace$center_y <- fuzzy_merge_trace$y_val + fuzzy_merge_trace$y_val_corr
  
  # removing duplicated values -- this happened when a point along a trace 
  # was close to both time dots, and the way this code is set up it will keep 
  # the first duplicated value but remove the trailing ones. Since any errors 
  # due to scanning would be gradual and the time dots are usually ~1.2 cm apart,
  # I think this method should be sound 
  fuzzy_merge_trace <- fuzzy_merge_trace[!duplicated(fuzzy_merge_trace[,1:2]),]
 
  # some final tidying -- this is removing unimportant columns resulting from 
  # the merge and giving these columns meaningful names. 
  fuzzy_merge_trace <- fuzzy_merge_trace[,c(1,6)]
  names(fuzzy_merge_trace) <- c("x_val", "y_val")
  
  # returning centered y-values 
  return(fuzzy_merge_trace)
}
# calling the function 
center_trace <- center_scan(trace, time_dots)





