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
#             needed centering. (see r_scripts/centering_functions.R)
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
# involves selecting certain columns, changing name values, and correcting the 
# y values of the trace from odd origin placement in ImageJ (i.e., values above 
# the origin are negative by default).
# 
# Input: 
#   - trace   : raw csv file from ImageJ after image processing 
#
# Output: 
#   - trace   : tidy file with correct columns and column names 
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
# first time dot-- this was needed for centering and time functions. This is 
# cosistent with the way the previous group in the 90's analyzed these traces. 
#
# Input: 
#   - time_dots   : raw csv file from ImageJ after image processing 
#
# Output: 
#   - time_dots   : tidy file with correct columns and column names 
###############################################################################
tidy_timedots <- function(time_dots){
  # selecting the correct columns for these data
  time_dots <- dplyr::select(time_dots, c("X", "Y"))
  # creating the first time dot, which is when the trace begins. There is not a  
  # time dot created when the device starts gathering data, so I had to add one 
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

# STEP TWO CODE ###############################################################
# The code in r_scripts/centering_functions.R centers the scan to achieve step 
# two. 
###############################################################################