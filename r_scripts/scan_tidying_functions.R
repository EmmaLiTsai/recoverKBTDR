################################################################################
# Data tidying code and centering the scan 
# Date: 3/26/2021
################################################################################

# This file contains all extra tidying and centering code to clean up the scans
# All of these preliminary steps are necessary in order to continue with the 
# recovery process. 

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

## STEP ONE CODE ##############################################################
# Code below achieves step 1 of the cleaning and file tidying process: 
# however, I change the global variables in this process... is that bad 
# practice?
tidy_trace <- function(trace){
  # changing the corrected y-value
  trace$y_corr <<- trace$y_corr_p2
  # selecting the correct columns and removing unnecessary ones 
  trace <<- select(trace, -c("Y", "y_corr"))
  # changing the name values 
  names(trace) <<- c("x_val", "y_val")
  # returning trace 
  return(trace)
}
tidy_timedots <- function(time_dots){
  # selecting the correct columns for these data
  time_dots <<- select(time_dots, -c("X.1"))
  # changing names
  names(time_dots) <<- c("x_val", "Y")
  # correcting for default y scale in ImageJ
  time_dots$Y <<- time_dots$Y * -1
  # returning the final output 
  return(time_dots)
}

tidy_timedots(time_dots)
tidy_trace(trace)

# STEP TWO CODE ###############################################################
# The code below centers the scan to achieve step two of this file. 

# this was a library I found that could do fuzzy merges with numerical values: 
library("fuzzyjoin")

# this code centers the scan after doing an imperfect full merge: 
center_scan <- function(trace, time_dots, dist_timedot = 1.1){
  
  # this is a fuzzy distance merge that I did using the "fuzzyjoin" package. 
  # it produces a data frame with x and y values of the trace and the connected
  # x and y values of the trace (headers x_val.y and Y, respectively). 
  
  # I needed this in order to use the y values of the time dots to center the 
  # scan.. there is a lag with this merging process since my max_dist = 3, but
  # I don't think this is too much of an issue ...
  fuzzy_merge_trace <- difference_full_join(trace, time_dots, by = c("x_val"), 
                                          max_dist = 3)
  
  # calculating how far the y time dot values are from the dist_timedot value. 
  # this was needed to move the x or y values up or down to center the scan 
  fuzzy_merge_trace$y_val_corr <- (abs(fuzzy_merge_trace$Y) - dist_timedot)
  
  # centering the scan using the above y corrected values  
  fuzzy_merge_trace$center_y <- fuzzy_merge_trace$y_val + fuzzy_merge_trace$y_val_corr
  
  # removing duplicated values 
  fuzzy_merge_trace <- fuzzy_merge_trace[!duplicated(fuzzy_merge_trace[,1:2]),]
 
   # some final tidying 
  fuzzy_merge_trace <- fuzzy_merge_trace[,c(1,6)]
  names(fuzzy_merge_trace) <- c("x_val", "y_val")
  
  # returning centered y-values 
  return(fuzzy_merge_trace)
}
# calling the function 
center_trace <- center_scan(trace, time_dots)




