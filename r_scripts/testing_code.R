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
#   2. transform coordinates by radius arm eqn
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
# within functions, I have them tagged as :: so we know what functions come 
# from what package. 

## Needed functions
source("../r_scripts/scan_tidying_functions.R")
source("../r_scripts/dive_traces_tidy.R")
source("../r_scripts/find_center_y.R")
## Functions to handle unique issues in the records:
source("../r_scripts/center_scan_td_issue.R")
source("../r_scripts/zoc.R")


# reading in trace
trace <- read.csv("../sample_data/skele_trace.csv", 
                  header = TRUE, 
                  stringsAsFactors = FALSE)

# reading in time dots 
time_dots <- read.csv("../sample_data/skele_timedots.csv", 
                      header = TRUE, 
                      stringsAsFactors = FALSE, 
                      row.names = 1)

# psi calibration file
psi_calibration <- read.csv("../sample_data/skele_psi_calibration.csv", 
                            header = TRUE, 
                            stringsAsFactors = FALSE)
# CAUTION- sometimes psi_calibration csv files will transform the psi_interval 
# column (text format) to another format when the csv file is opened. It seems 
# to read in well with read.csv, but I'm currently investigating a better way to 
# manage and store these files so this doesn't happen.

################################################################################
# STEP ONE: re-centering and misalignment functions: ###########################
################################################################################
# Not sure if these functions will be included in the final package, but these 
# functions help with transforming and tidying the csv files from ImageJ. This 
# code also centers the scan, which was necessary after scanning the traces.


# If the scan has issues with the time dots (records 16 and 17), center the 
# scan using the r_scripts/center_scan_td_issue.R function:

# center_scan_td_issue <- center_scan_td_issue(trace, time_dots)

# Comparing centering with original record, the record after center_scan, and 
# the record after center_scan_td_issue, which should be an improvement 
# from both methods. 
# ggplot(center_trace, aes(x = x_val, y = y_val)) + geom_point() + 
#   geom_point(data = center_scan_td_issue, aes(x = x_val, y = y_val), color = "red") + 
#   geom_point(data = trace, aes(x = x_val, y = y_val), color = "blue")


# scan tidying functions can be found in the scan_tidying_functions.R file in 
# the r_scripts folder. 
# the output of this file is a centered trace with x and y values

# Moved these lines from scan_tidying_functions.R
time_dots <- tidy_timedots(time_dots)
trace <- tidy_trace(trace)
center_trace <- center_scan(trace, time_dots)

nrow(trace[!duplicated(trace[,1:2]),])
# ^ this is the same as the number of observations produced after centering, so 
# I think methods for centering should be okay. Must be an bug with imageJ
# selection tool from image processing

# plotting the centered trace with the original trace to see how the script 
# ran and how centering performed: 
ggplot(center_trace, aes(x = x_val, y = y_val)) + geom_line() + 
  geom_line(data = trace, aes(x = x_val, y = y_val), color = "red")

# I could add this in the function call, but I kept it out so I could visually 
# compare the output with the original trace csv file
trace <- center_trace

################################################################################
# STEP TWO AND THREE: Transform coordinates by arm equation and time scale######
################################################################################

# Before running this code, confirm that the correct center_y value 
# has been calculated for the transform_coordinates function.

# running find_center_y with sample values from this record:
find_center_y(1142.9, 0, 1140.5, 9.3, 21.14, 0.16, psi_calibration)

# If the scan has major drift in depth = 0 and/or level shifts, zero offset the 
# data using the r_scripts/zoc.R file before using the transform_coordinates 
# function. This code (modified from the diveMove package) correct the data such 
# that depth = 0 aligns better with y = 0 for more reliable arc removal. 

# calling the function here: 
trace <- transform_coordinates(trace, time_dots, center_y = 11.19, time_period_min = 12)
# any warning here would be from points that happened after the last time dot

## DWS: This function was not defined. I've added a source call to the top of
## this script to define it.

# ordering -- this needs to be out of the function
trace <- trace[order(trace$time),]

# plotting: 
ggplot(trace[1000:9000,], aes(x = time, y = y_val)) + geom_line()
# times here align with the times I produced with my original code in the 
# example_trace.R file. 

################################################################################
# STEP FOUR: Transform Y axis from psi to depth ################################
################################################################################
# calling the function -- the other function (transform_todepth) is needed for 
# 1978 - 1979 traces without a psi calibration curve at the end of the record
trace <- transform_psitodepth(trace, psi_calibration)

# plotting 
ggplot(trace, aes(x = time, y = depth)) + geom_line()

# max depth value in the bulletin is 317 meters, which is very close to the one 
# calculated here of 318 meters!:
max(trace[1:200000,]$depth)

# looking at different bouts of dives to assess how this method worked: 
# bout one 
ggplot(trace[1000:9000,], aes(x = time, y = depth)) + geom_line()
# 
# bout two 
ggplot(trace[39000:45000,], aes(x = time, y = depth)) + geom_line() 
# 
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
# section is in development:  

# trying out local fitting: 
library(locfit)
# smoothing method below doesn't work well as the trace progresses 
ggplot(trace[1500:99800,], aes(x = new_x, y = depth)) + geom_line() + 
  geom_smooth(method = 'locfit', method.args = list(deg=20, alpha = 0.1))

# trying out kernel smoothing: 
k_fit <- with(trace, ksmooth(time, depth, kernel = "box", bandwidth = 1.5))
# time values look off
trace$ksmooth <- k_fit$y
# plotting 
ggplot(trace[5000:9000,], aes(x = time, y = ksmooth)) + 
  geom_line(color = "red") + 
  geom_line(aes(x = time, y = depth))

# trying out spline smoothing with knots, since spline smoothing is usually more 
# computationally efficient: 
s_fit <- smooth.spline(trace$time, trace$depth, spar = 0.3, nknots = 5900)
# adding it to the trace df
trace$ssmooth <- predict(s_fit, trace$time)$y
# plotting
ggplot(trace[1000:11000,], aes(x = time, y = depth)) + 
  geom_line() +
  geom_line(aes(x = time, y = ssmooth), color = "red", size = 1)
# this method is pretty good-- need to try out different number of knots and 
# spar combinations... it would be nice if there was a way to mathematically 
# determine appropriate number of knots & spar values based on the number of 
# observations in a trace. From looking at the literature, this may involve 
# generalized cross validation or AIC. 

# This spline smoothing method is also used in the diveMove package, but these 
# data need to be smoothed before they can be read in as a TDR object in this 
# package. 

# Looking at the difference between the depth and smoothed values 
# to make sure nothing weird is happening here: 
ggplot(trace, aes(x = time, y = (depth - ssmooth))) + geom_line()

# loess smoothing: 
l_fit <- loess(time ~ depth, degree = 1, span = 0.1)
# this takes so much longer to run...
trace$lsmooth <- l_fit$fitted
# plotting 
ggplot(trace[1000:9000,], aes(x = time, y = lsmooth)) + geom_line()

################################################################################
## STEP SIX:  Dive statistics, direction flagging, etc##########################
################################################################################
# calling the function 
trace <- add_dates_times(trace, start_time = "1981:01:16 15:10:00")

# plotting 
ggplot(trace[190000:198272,], aes(x = date_time, y = depth)) + geom_line()
# checking out the end slice -- the end time should be 1/23/1981 11:10:00, as 
# defined by the 1990's team, which checks out! For this record they defined the 
# end of the record after the last dive was made by the seal. 

