################################################################################
# This file is used to test code created in other .R files, to separate testing 
# and production code. 
################################################################################

# Basic structure of this file is to call functions in using the source() 
# function and to investigate the product of those functions, or use a loaded 
# function from the dive_traces_tidy.R file to investigate the outputs. 

# Similar to the dive_traces_tidy.R file, this file has been broken up into the 
# main steps outlined for this project: 

#   1. Recenter and fix misalignment (both data inputs)
#   2. Transform coordinates by radius arm eqn
#   3. Transform x axis according to time dots
#   4. Transform y axis to depth
#   5. Smoothing
#   6. Dive statistics, direction flagging, etc.

################################################################################
# Reading in example data: #####################################################
################################################################################

# some basic libraries that are required:  
library(ggplot2) # for visualizing outputs in this file 
library(fuzzyjoin) # for fuzzy merge in scan centering, difference_left_join()
library(dplyr) # for select(), mutate(), and case_when()
library(tidyr) # for separate()
library(lubridate) # for dates and times 
library(caTools) # for zoc using moving window statistics 
# within functions, I have them tagged as :: so we know what functions come 
# from what package. 

## Needed functions
source("../r_scripts/read_trace.R")
source("../r_scripts/centering_functions.R")
source("../r_scripts/dive_trace_tidy_functions.R")
source("../r_scripts/find_center_y_functions.R")
source("../r_scripts/smooth_trace.R")
## Functions to handle unique issues in the records:
source("../r_scripts/zoc.R")

# reading in full trace data (i.e., trace, time dots, and psi calibration): 
read_trace(filepath = "../sample_data")

################################################################################
# STEP ONE: re-centering and misalignment functions: ###########################
################################################################################
# Not sure if these functions will be included in the final package, but these 
# functions help with transforming the csv files from ImageJ to center the 
# record, which was necessary after scanning the traces.

# -- 
# If the scan has issues with the time dots (records 16 and 17), center the 
# scan using the r_scripts/center_scan_td_issue.R function:

# center_scan_td_issue <- center_scan_td_issue(trace, time_dots)

# Comparing centering with original record, the record after center_scan, and 
# the record after center_scan_td_issue, which should be an improvement 
# from both methods. 
# ggplot(center_scan_td_issue, aes(x = x_val, y = y_val)) + geom_point() + 
#   geom_point(data = trace, aes(x = x_val, y = y_val), color = "blue")
# -- 

center_trace1 <- old_center_scan(trace, time_dots)
center_trace2 <- center_scan(trace, time_dots)

nrow(center_trace1)
nrow(center_trace2)

## Don't match. The old version deletes rows!
## DWS ok with removing dupes it works better. still not itndeitcal
identical(center_trace1, center_trace2)

center_trace1[10000:10050,]
center_trace2[10000:10050,]

nrow(trace[!duplicated(trace[,1:2]),])
# ^ this is the same as the number of observations produced after centering, so 
# I think methods for centering should be okay. Must be an bug with imageJ
# selection tool from image processing

# plotting the centered trace with the original trace to see how the script 
# ran and how centering performed: 
ggplot(center_trace2, aes(x = x_val, y = y_val)) + geom_line() + 
  geom_line(data = trace, aes(x = x_val, y = y_val), color = "red") + 
  geom_line(data = center_trace1, aes(x = x_val, y = y_val), color = "blue")

# I could add this in the function call, but I kept it out so I could visually 
# compare the output with the original trace csv file
trace <- center_trace2

################################################################################
# STEP TWO AND THREE: Transform coordinates by arm equation and time scale######
################################################################################

# Before running this code, confirm that the correct center_y value 
# has been calculated for the transform_coordinates function.

# running find_center_y with sample values from this record:
find_center_y_psi(1142.9, 0, 1140.5, 9.3, 21.14, 0.16, psi_calibration)

# -- 
# if the record does not have a psi_calibration file (1978 - 1979 records), 
# use this function instead: 
# example from WS_14: 
# find_center_y_nopsi(65.258, -0.056, 63.442, 5.341, 21.14, 0.21, 484, trace)
# -- 

# -- 
# If the scan has major drift in depth = 0 and/or level shifts, zero offset the 
# data using the r_scripts/zoc.R file before using the transform_coordinates 
# function. This code (modified from the diveMove package) correct the data such 
# that depth = 0 aligns better with y = 0 for more reliable arc removal. 
zoc_trace <- zoc(trace, k = c(3, 500), probs = c(0.5, 0.02), depth.bounds = c(-5, 1))
# plotting to view data after zoc
ggplot(zoc_trace[1000:19000,], aes(x = x_val, y = y_val)) + geom_point() + 
  geom_point(data = trace[1000:19000,], aes(x = x_val, y = y_val), color = "red")
# --

# calling the function to transform x-axis here: 
trace <- transform_coordinates(trace, time_dots, center_y = 11.19, time_period_min = 12)
# any warning here would be from points that happened after the last time dot

# ordering -- this needs to be out of the function
trace <- trace[order(trace$time),]

# plotting: 
ggplot(trace[1000:9000,], aes(x = time, y = y_val)) + geom_line()
# times here align with the times I produced with my original code.

################################################################################
# STEP FOUR: Transform Y axis from psi to depth ################################
################################################################################
# calling the function to transform y-axis to depth: 
trace <- transform_psitodepth(trace, psi_calibration)

# -- 
# if the record is before 1981 and does not have a psi calibration curve at the
# end, use the transform_todepth function (example below):
# transform_todepth(trace, 318)
# -- 

# plotting 
ggplot(trace, aes(x = time, y = depth)) + geom_line()

# max depth value in the bulletin is 317 meters, which is very close to the one 
# calculated here of 318 meters!:
max(trace[1:200000,]$depth)

# looking at different bouts of dives to assess how this method worked: 
# bout one 
ggplot(trace[1000:9000,], aes(x = time, y = depth)) + geom_line()

# bout two 
ggplot(trace[39000:45000,], aes(x = time, y = depth)) + geom_line() 

# bout three 
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

# spline smoothing with knots, since spline smoothing is usually more 
# computationally efficient. 

# function smooth_trace is a simple spline smoothing function: 
trace <- smooth_trace(trace, spar = 0.3, nknots = 5900)

# function smooth_trace_bounded is a more complex recursive spline smoothing
# function with depth bounds, such that shallower depths have a higher spar 
# value to  reduce chatter created by the transducer arm, while retaining 
# wiggles in the dives at depth. Supposed to be an improvement from 
# smooth_trace function above. 
smooth_bounded <- smooth_trace_bounded(trace, spar = c(0.8, 0.3), nknots = c(1000, 5900), depth_bound = 15)
# this function would be sound considering there is less tension on the 
# transducer arm at shallow depths, which produced extra noise in the record 
# when the seal was resting at the surface or hauled out. 

# comparing the two smoothing methods with the original data: 
# smoothing with depth bounds is in blue, and normal smoothing is in red 
ggplot(trace[1000:11000,], aes(x = time, y = depth)) + geom_line(color = "grey") + 
  geom_line(data = smooth_bounded[1000:11000,], aes(x = time, y = smooth_2), color = "blue", size = 1) +  
  geom_line(data = trace[1000:11000,], aes(x = time, y = smooth_depth), color = "red", size = 1) 

# plotting
ggplot(trace[1000:11000,], aes(x = time, y = depth)) + 
  geom_line() +
  geom_line(aes(x = time, y = smooth_depth), color = "red", size = 1)
# this method is pretty good-- need to try out different number of knots and 
# spar combinations... it would be nice if there was a way to mathematically 
# determine appropriate number of knots & spar values based on the number of 
# observations in a trace. From looking at the literature, this may involve 
# generalized cross validation or AIC. 

# Looking at the difference between the depth and smoothed values 
# to make sure nothing weird is happening here: 
ggplot(trace, aes(x = time, y = (depth - smooth_depth))) + geom_line()

# some potential cross validation code, see issue #17 in GitHub 
# creating cross validation example for spline smoothing using leave one out 
# cross validation method: 
# creating smaller trace data frame 
trace_cv <- trace[1:500,]
# spar sequence 
spar_seq <- seq(from = 0.05, to = 1.0, by = 0.02)

cv_error_spar <- rep(NA, length(spar_seq))

for (i in 1:length(spar_seq)){
  spar_i <- spar_seq[i]
  cv_error <- rep(NA, nrow(trace_cv))
  for (v in 1:nrow(trace_cv)){
    # validation
    x_val <- trace_cv$time[v]
    y_val <- trace_cv$depth[v]
    # training 
    x_train <- trace_cv$time[-v]
    y_train <- trace_cv$depth[-v]
    # smooth fit and prediction
    smooth_fit <- smooth.spline(x = x_train, y = y_train, spar = spar_i)
    y_predict <- predict(smooth_fit, x = x_val)
    
    cv_error[v] <- (y_val - y_predict$y)^2
  }
  cv_error_spar[i] <- mean(cv_error)
}

cv_error_spar

# plotting prediction error 
plot(x = spar_seq, y = cv_error_spar, type = "b", lwd = 3, col = "red",
     xlab = "Value of 'spar'", ylab = "LOOCV prediction error")

spar_seq[which(cv_error_spar == min(cv_error_spar))]

################################################################################
## STEP SIX:  Dive statistics, direction flagging, etc##########################
################################################################################
# calling the function 
trace <- add_dates_times(trace, start_time = "1981:01:16 15:10:00")

# plotting 
ggplot(trace[190000:198272,], aes(x = date_time, y = smooth_depth)) + geom_line()
# checking out the end slice -- the end time should be 1/23/1981 11:10:00, as 
# defined by the 1990's team, which checks out! For this record they defined the 
# end of the record after the last dive was made by the seal. 

