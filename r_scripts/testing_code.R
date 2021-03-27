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

################################################################################
# reading in example data: #####################################################
################################################################################

trace <- read.csv("../sample_data/skele_trace.csv", header = TRUE, 
                  stringsAsFactors = FALSE)

time_dots <- read.csv("../sample_data/skele_timedots.csv", header = TRUE, 
                      stringsAsFactors = FALSE)

# some basic libraries for visualizing the output of some of these functions: 
library(ggplot2)

################################################################################
# STEP ONE: re-centering and misalignment functions: ###########################
################################################################################

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


################################################################################
# STEP TWO AND THREE: Transform coordinates by arm equation and time scale######
################################################################################

# calling the function here: 
trace <- transform_coordinates(trace, time_dots, time_period_min = 12)

# plotting: 
ggplot(trace, aes(x = time, y = y_val)) + geom_line()

################################################################################
# STEP FOUR: Transform Y axis from psi to depth ################################
################################################################################

# calling the function I created 
trace <- transform_todepth(trace)

# plotting to look at the whole trace
ggplot(trace, aes(x = time, y = depth)) + geom_line()

# looking at different bouts to assess how this method worked
# bout one 
ggplot(trace[1000:9000,], aes(x = time, y = depth)) + geom_line()
# 
# # bout two 
ggplot(trace[39000:45000,], aes(x = time, y = depth)) + geom_line() 
# 
# # bout three 
ggplot(trace[76000:84800,], aes(x = time, y = depth)) + geom_line() 

# plotting again... this is close to what the final product should be. 
ggplot(trace, aes(x = time, y = depth)) + 
  geom_line() +
  theme_bw() + 
  labs(x = "Time (min)", y = "Depth (m)", title = "WS_25_1981") + 
  scale_x_continuous(position = "top") + 
  scale_y_reverse()

################################################################################
## STEP FIVE: Smoothing ########################################################
################################################################################
# in progress 


################################################################################
## STEP SIX:  Dive statistics, direction flagging, etc##########################
################################################################################
ggplot(trace, aes(x = date_time, y = depth)) + geom_line()

