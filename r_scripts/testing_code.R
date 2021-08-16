################################################################################
# This file is used to test code created in other .R files, to separate testing 
# and production code. 
################################################################################

# Basic structure of this file is to call functions in using the source() 
# function and to investigate the product of those functions, or use a loaded 
# function from the dive_traces_tidy.R file to investigate the outputs. 

# Similar to the dive_traces_tidy.R file, this file has been broken up into the 
# main steps outlined for this project: 

# 1. Recenter and fix misalignment (both data inputs)
# 2. Transform coordinates by radius arm eqn
# 3. Transform x axis to dates & times
# 4. Interpolate between missing time points 
# 5. Transform y axis to depth 
# 6. Smoothing

################################################################################
# Reading in example data: #####################################################
################################################################################

# for visualizing outputs in this file:
library(ggplot2) 

# dependencies: 
library(dplyr) # for select(), mutate(), case_when(), group_by(), summarize(), 
# filter()
library(tidyr) # for separate() and drop_na() 
library(lubridate) # for ymd_hms() 
library(caTools) # for runmean() and runquantile() 
library(zoo) # for na.approx()
library(diveMove) # this is only for finding the best spar value, where I see  
# how different spar values influence bottom distance. I know this dependency 
# isn't ideal, but I hope to remove it in future commits.

# within functions, I have them tagged as :: so we know what functions come 
# from what package. 

## Needed functions
source("../r_scripts/read_trace.R")
source("../r_scripts/centering_functions.R")
source("../r_scripts/find_center_y_functions.R")
source("../r_scripts/dive_trace_tidy_functions.R")
source("../r_scripts/find_spar_value_functions.R")
## Functions to handle unique issues in the records:
source("../r_scripts/zoc_functions.R")

# Fast-track recovery function that works with an argument csv file (args) to 
# pass trace-specific arguments to all the functions in this repository. 
# Basically a wrapper function for fast recovery. 
source("../r_scripts/fast_recovery.R")
fast_recovery(filepath = "../sample_data/WS_25_1981")
# output is a fully recovered trace with dates, times, and smoothed depths. 
# I created this to quickly read in data without having to tab through this 
# file, and also for future diveMove analysis. This was more important for my 
# private repo with the whole data set for this project. 

# step-by-step functions
# reading in full trace data (i.e., trace, time dots, and argument file): 
read_trace(filepath = "../sample_data/WS_25_1981")

################################################################################
# STEP ONE: re-centering and misalignment functions: ###########################
################################################################################
# Not sure if this function will be included in the final package, but these 
# functions help with transforming the files from ImageJ to center the record, 
# which was necessary after scanning the traces.

center_trace <- center_scan(trace, time_dots, dist_timedot = 0.9)
# can also confirm that the center_scan method works well for records with time 
# dot issues (time dots are farther apart than normal). Comparisons between 
# centering methods can be found in previous commits in this repository. 

# plotting the centered trace with the original trace to see how the script 
# ran and how centering performed: 
ggplot(trace[1000:11000,], aes(x = x_val, y = y_val)) + geom_point() + 
  geom_point(data = center_trace[1000:11000,], aes(x = x_val, y = y_val), color = "red") 

# creating another window to compare the different methods: 
ggplot(trace[88000:100000,], aes(x = x_val, y = y_val)) + geom_point() + 
  geom_point(data = center_trace[88000:100000,], aes(x = x_val, y = y_val), color = "red") 

# I could add this in the function call, but I kept it out so I could visually 
# compare the output with the original trace file
trace <- center_trace

################################################################################
# STEP TWO AND THREE: Transform coordinates by arm equation and time scale######
################################################################################

# find the psi calibration curve after centering: 
psi_calibration <- centered_psi_calibration(trace)

# running find_center_y with sample values from this record. This function is 
# meant to find an approximate value for center_y, but this still needs to be 
# visually confirmed. 
find_center_y_psi(1142.945, 0, 1140.55, 9.3, 21.14, 0.16, psi_calibration)

# -- start unique case -- 
# if the record does not have a psi_calibration file (1978 - 1979 records), 
# use this function instead: 
# example from WS_14: 
# find_center_y_nopsi(65.258, -0.056, 63.442, 5.341, 21.14, 0.21, 484, trace)
# -- end unique case -- 

# -- start unique case -- 
# If the scan has major drift in depth = 0 and/or level shifts, zero offset the 
# data using the r_scripts/zoc_functions.R file before using the 
# transform_coordinates function. This code (modified from the diveMove package) 
# correct the data such that depth = 0 aligns better with y = 0 for more 
# reliable arc removal. If the trace has extreme drift in depth = 0 after 
# centering, use the function zoc_big_drift, which adds another correction 
# filter before zoc. 
zoc_trace <- zoc(trace, k_h = 500, depth_bounds = c(-1, 1))

# plotting to view data after zoc
ggplot(zoc_trace[1000:19000,], aes(x = x_val, y = y_val)) + geom_point() + 
  geom_point(data = trace[1000:19000,], aes(x = x_val, y = y_val), color = "red")
# plotting another view 
ggplot(zoc_trace[145000:160000,], aes(x = x_val, y = y_val)) + geom_point() + 
  geom_point(data = trace[145000:160000,], aes(x = x_val, y = y_val), color = "red")
# plotting the whole record 
ggplot(zoc_trace, aes(x = x_val, y = y_val)) + geom_point() + 
  geom_point(data = trace, aes(x = x_val, y = y_val), color = "red")

# If there is HUGE drift in the record (WS_1_part2 and WS_22), zoc the data with 
# the function zoc_big_drift(). It basically calls the zoc function above, but 
# adds an extra filtering process beforehand to help make zoc more reliable. 
# -- end unique case -- 

# Before running this code below, confirm that the correct center_y value has 
# been calculated for the transform_coordinates() function.

# calling the function to transform x-axis here: 
trace <- transform_coordinates(trace, 
                               time_dots, 
                               center_y = 11.19, 
                               time_period_min = 12)
# any observations removed were points that happened after the last time dot, 
# or ones that were moved before the origin after arc removal (only points that 
# were extremely close to the origin and negative). 

# plotting: 
ggplot(trace[1000:11000,], aes(x = time, y = y_val)) + geom_line()
# times here align with the times I produced with my original code.

################################################################################
# STEP FOUR: Interpolate between missing time points  ##########################
################################################################################
# calling the function to add dates and times to the data, and to also 
# interpolate between missing data points 
trace <- add_dates_times(trace, 
                         start_time = "1981:01:16 15:10:00", 
                         on_seal = "1981:01:16 17:58:00", 
                         off_seal = "1981:01:23 15:30:00")

# plotting 
ggplot(trace[550000:nrow(trace),], aes(x = date_time, y = interp_y)) + 
  geom_point(color = "grey") + 
  geom_line(aes(x = date_time, y = interp_y), color = "red")
# checking out the end slice -- the end time should be 1/23/1981 11:10:00, as 
# defined by the 1990's team, which checks out! For this record they defined the 
# end of the record after the last dive was made by the seal. 

################################################################################
# STEP FIVE: Transform Y axis from psi to depth ################################
################################################################################
# calling the function to transform y-axis to depth: 
trace <- transform_psitodepth(trace, 
                              psi_calibration, 
                              max_psi = 900, 
                              max_position = 22.45)
# the psi curve at the end matches the intervals on the record

# -- start unique case -- 
# if the record is before 1981 and does not have a psi calibration curve at the
# end, use the transform_todepth function (example below):
# transform_todepth(trace, 318)
# -- end unique case -- 

# plotting 
ggplot(trace, aes(x = date_time, y = depth)) + geom_line()

# max depth value in the bulletin is 319 meters, which is very close to the one 
# calculated here but will decrease with smoothing 
max(trace$depth)

# looking at different bouts of dives to assess how this method worked: 
# bout one 
ggplot(trace[1000:19000,], aes(x = date_time, y = depth)) + geom_line()

# bout two 
ggplot(trace[179000:245000,], aes(x = date_time, y = depth)) + geom_line() 

# bout three 
ggplot(trace[330000:434800,], aes(x = date_time, y = depth)) + geom_line() 

# bout four, this one contains the deepest dive 
ggplot(trace[445000:580000,], aes(x = date_time, y = depth)) + geom_line() 

################################################################################
## STEP SIX: Smoothing ########################################################
################################################################################

## Finding the best spar value using dive statistics: ##

# I tried many different cross-validation methods, but I believe this method 
# is the most reliable out of the ones that I explored.  
# Here is a method created using the find_spar_value_functions.R file, 
# which recovers a record 21 separate times using different spar values 
# [0-1], by = 0.05, and retrieves the dive statistics for each spar scenario. 
# This function takes a long time to run, so I saved the output as a csv file 
# to avoid others having to run it again: 
dive_stats <- read.csv("../sample_data/WS_25_1981/WS_25_1981_dive_stats.csv")

# plotting how bottom distance changes across spar values: 
ggplot(dive_stats, aes(x = as.factor(spar_val), y = bottdist)) + 
  geom_point(aes(color = as.factor(spar_val))) + 
  theme_bw() + 
  labs(x = "Spar Value", y = "Bottom Distance (m)") + 
  theme(legend.position = "none", axis.text.x = element_text(angle = 90))
# finding the ideal spar value: 
best_spar(dive_stats)
# spar value should be 0.22 for this record! 

## spline smoothing: ##

# this is method increases the resolution of spline smoothing when the seal is 
# in diving or in a bout of dives. In comparison to just smoothing based off 
# depth, this function would help retain the resolution between dives for 
# post-dive surface intervals. 
trace <- smooth_trace_dive(trace, spar_h = 0.22, depth_thresh = 15)

# here is what this smoothing method looks like-- dive detected smoothing is 
# light blue line and no dive detected smoothing is dark blue line
ggplot(trace, aes(x = date_time, y = depth)) + geom_line(color = "grey") + 
  geom_line(aes(x = date_time, y = smooth_depth, color = bout), size = 1) + 
  theme(legend.position = "none")

# you can certainly see the different bouts of dives in this method! 
ggplot(trace[458000:560000,], aes(x = date_time, y = depth)) + geom_line(color = "grey") + 
  geom_line(aes(x = date_time, y = smooth_depth, color = bout), size = 1) + 
  theme(legend.position = "none")

# also added dive component assignment within the smoothing functions. I can 
# always remove this, but just wanted to experiment with it here to see if it 
# would be possible to remove the diveMove depencency: 
ggplot(trace[458000:560000,], aes(x = date_time, y = smooth_depth)) +
  geom_line(aes(date_time, depth), color= "gray", size = 0.2) +
  geom_point(aes(color = deriv > 0)) +
  geom_line() + 
  scale_color_discrete(name = "", labels = c("Ascent", "Descent"))

# Looking at the difference between the depth and smoothed values 
# to make sure nothing weird is happening here: 
ggplot(trace, aes(x = date_time, y = (depth - smooth_depth))) + geom_line()

# Looking at the new maximum depth: this one should be as close as possible to 
# 319 meters, but has definitely decreased with smoothing 
max(trace$smooth_depth)

# plotting again... this is close to what the final product should be. 
ggplot(trace, aes(x = date_time, y = smooth_depth)) + 
  geom_line() +
  theme_bw() + 
  labs(x = "Time (min)", y = "Depth (m)", title = "WS_25_1981") + 
  scale_x_continuous(position = "top") + 
  scale_y_reverse()

# After this final step, the proposed workflow is to export the final trace data 
# to a csv file, which can be read for further dive analysis in the diveMove 
# package. The diveMove package creates an S4 object using the date_time column 
# and depth, and assumes a regular time series (which is why interpolating was 
# necessary).

# I was wondering if I could detect haul-out behavior from the difference 
# between time dots (dots should be closer since the motor rolling the film  
# slows from Antarctic air temperatures). But there doesn't seem to be a clear 
# trend in this record: 
ggplot(trace, aes(x = x_val, y = y_val)) + geom_point() + 
  geom_line(data = time_dots, aes(x = x_val, y = c(0, diff(y_val)*100)), color = "red", size = 1)
# I only see a trend in a handful of the records, but this could also mean that 
# the seal might've just not hauled out in some of the records (like this one).

