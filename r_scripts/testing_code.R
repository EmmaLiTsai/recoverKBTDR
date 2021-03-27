################################################################################
# This file is used to test code created in other .R files, to separate testing 
# and production code. 
################################################################################

# Basic structure of this file is to call functions in using the source() 
# function and to investigate the product of those functions. This file can be
# broken up into the main steps outlined for this project: 

#   1. Recenter and fix misalignment (both data inputs)
#   2. transform coordinates by radius arm eqn
#   3. Transform x axis according to time dots
#   4. Transform y axis to depth
#   5. Smoothing
#   6. Dive statistics, direction flagging, etc.


# reading in example data: #####################################################
trace <- read.csv("../sample_data/skele_trace.csv", header = TRUE, 
                  stringsAsFactors = FALSE)

time_dots <- read.csv("../sample_data/skele_timedots.csv", header = TRUE, 
                      stringsAsFactors = FALSE)

# some basic libraries for visualizing the output of some of these functions: 
library(ggplot2)


# STEP ONE: re-centering and misalignment functions: ###########################

# Not sure if these functions will be included in the final package, but these 
# functions help with transforming and tidying the csv files from ImageJ. This 
# code also centers the scan, which was necessary after scanning the traces. 

# These functions can be found in the scan_tidying_functions.R file in the
# r_scripts folder. 
source("../r_scripts/scan_tidying_functions.R")

# the output of this file is a centered trace with x and y values

# plotting the centered trace with the original trace to see how the script 
# ran and how centering performed: 
ggplot(center_trace, aes(x = x_val, y = y_val)) + geom_line() + 
  geom_line(data = trace, aes(x = x_val, y = y_val), color = "red")
# warning message is from the last value of the trace, which produced an NA 
# value
trace <- center_trace


# STEP TWO: Transform coordinates by arm equation #############################

trace <- transform_coordinates(trace, time_dots, time_period_min = 12)

ggplot(trace, aes(x = time, y = y_val)) + geom_line()


# STEP THREE: Transform x axis according to time dots ##########################
# in progress 

# STEP FOUR: Transform Y axis from psi to depth ################################
# in progress 

# STEP FIVE: Smoothing #########################################################
# in progress 

