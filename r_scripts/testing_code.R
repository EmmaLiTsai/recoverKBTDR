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

# for visualizing outputs in this file:
library(ggplot2) 
# dependencies: 
library(dplyr) # for select(), mutate(), case_when()
library(tidyr) # for separate()
library(lubridate) # for dates and times 
library(caTools) # for zoc using moving window statistics 
# within functions, I have them tagged as :: so we know what functions come 
# from what package. 

## Needed functions
source("../r_scripts/read_trace.R")
source("../r_scripts/centering_functions.R")
source("../r_scripts/find_center_y_functions.R")
source("../r_scripts/dive_trace_tidy_functions.R")
source("../r_scripts/smooth_trace_functions.R")
## Functions to handle unique issues in the records:
source("../r_scripts/zoc_functions.R")

# fast-track recovery function that works with an argument csv file to pass 
# trace-specific arguments to all the functions in this repository: 
source("../r_scripts/fast_recovery.R")
recover_record(filepath = "../sample_data")
# output is a fully recovered trace with dates, times, and smoothed depths. 
# I created this to quickly read-in data without having to tab through this 
# file, and also for future diveMove analysis. This was more important for my 
# private repo with the whole data set for this project. 

# reading in full trace data (i.e., trace and time dots): 
read_trace(filepath = "../sample_data")

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

# -- unique case -- 
# if the record does not have a psi_calibration file (1978 - 1979 records), 
# use this function instead: 
# example from WS_14: 
# find_center_y_nopsi(65.258, -0.056, 63.442, 5.341, 21.14, 0.21, 484, trace)
# -- unique case -- 

# -- unique case -- 
# If the scan has major drift in depth = 0 and/or level shifts, zero offset the 
# data using the r_scripts/zoc_functions.R file before using the 
# transform_coordinates function. This code (modified from the diveMove package) 
# correct the data such that depth = 0 aligns better with y = 0 for more 
# reliable arc removal. If the trace has extreme drift in depth = 0 after 
# centering, use the function zoc_big_drift, which adds another correction 
# filter before zoc. 
zoc_trace <- zoc(trace, 
                 k = c(2, 500), 
                 probs = c(0.5, 0.02), 
                 depth_bounds = c(-1, 1))

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

# -- unique case -- 

# Before running this code below, confirm that the correct center_y value has 
# been calculated for the transform_coordinates function.

# calling the function to transform x-axis here: 
trace <- transform_coordinates(trace, time_dots, 
                               center_y = 11.2, 
                               time_period_min = 12)
# any observations removed were points that happened after the last time dot, 
# or ones that were moved before the origin after arc removal (only points that 
# were extremely close to the origin and negative). 

# plotting: 
ggplot(trace[1000:11000,], aes(x = time, y = y_val)) + geom_line()
# times here align with the times I produced with my original code.

################################################################################
# STEP FOUR: Transform Y axis from psi to depth ################################
################################################################################
# calling the function to transform y-axis to depth: 
trace <- transform_psitodepth(trace, 
                              psi_calibration, 
                              max_psi = 900, 
                              max_position = 22.45)
# the psi curve at the end matches the intervals on the record

# -- unique case -- 
# if the record is before 1981 and does not have a psi calibration curve at the
# end, use the transform_todepth function (example below):
# transform_todepth(trace, 318)
# -- unique case -- 

# plotting 
ggplot(trace, aes(x = time, y = depth)) + geom_line()

# max depth value in the bulletin is 319 meters, which is very close to the one 
# calculated here but will decrease with smoothing 
max(trace[1:210000,]$depth)

# looking at different bouts of dives to assess how this method worked: 
# bout one 
ggplot(trace[1000:9000,], aes(x = time, y = depth)) + geom_line()

# bout two 
ggplot(trace[39000:45000,], aes(x = time, y = depth)) + geom_line() 

# bout three 
ggplot(trace[76000:84800,], aes(x = time, y = depth)) + geom_line() 

# bout four, this one contains the deepest dive 
ggplot(trace[145000:160000,], aes(x = time, y = depth)) + geom_line() 

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

## First, here are some possible cross validation methods: ##
# A potential cross validation function in smooth_trace.R, see issue #17 in 
# GitHub. This attempts to find the best spar value for smoothing using leave  
# one out cross validation (loocv) method on a random sample of the trace data. 
# It is really slow with a nested for loop... 
find_spar_loocv(trace)
# seems to usually produce 0.27, but this method seems to produce extremely low 
# values for some of the other records... 

# This is an example using ordinary leave-one-out methods: 
smooth.spline(trace$time, trace$depth, nknots = 5900, cv = TRUE)
# and another example using generalized cross validation:
smooth.spline(trace$time, trace$depth, nknots = 5900, cv = FALSE)
# seems to be a delicate balance between knots and smoothing penalties, but the 
# general approach from the literature is to have a high number of knots and let 
# the smoothing penalties control the fit (Perperoglou et al., 2019).

# A helper function to visually compare different spar values: 
spar_options <- view_spar_options(trace, increase_spar = 0.05)
# plotting-- this takes a bit to run 
ggplot(spar_options[1000:300000,], aes(x = time, y = value, color = name)) + 
  geom_line() + 
  facet_wrap(~name) + 
  theme_bw() + 
  theme(legend.position = "none")

## Three possible smoothing methods: ## 
# function smooth_trace is a simple spline smoothing function: 
trace_smooth <- smooth_trace(trace, 
                             spar = 0.27, 
                             nknots = signif(nrow(trace) * .03, 1))

# function smooth_trace_bounded is a more complex recursive spline smoothing
# function with depth bounds, such that shallower depths have a higher spar 
# value to  reduce chatter created by the transducer arm, while retaining 
# wiggles in the dives at depth. Supposed to be an improvement from 
# smooth_trace function above. 
trace <- smooth_trace_bounded(trace, 
                              spar = c(0.8, 0.27), 
                              nknots = c(1000, signif(nrow(trace) * .03, 1)), 
                              depth_bound = 0)
# this function would be sound considering there is less tension on the 
# transducer arm at shallow depths, which produced extra noise in the record 
# when the seal was resting at the surface or hauled out. 
# plotting
ggplot(trace[1000:11000,], aes(x = time, y = depth)) + 
  geom_line(color = "grey") +
  geom_line(aes(x = time, y = smooth_depth), color = "red", size = 1)
# this method is pretty good, I wonder if this smoothing method is better than 
# the bout smoothing method below? 

# this is another possible method that increases the resolution of spline 
# smoothing when the seal is in a bout of dives
trace_smooth_bout <- smooth_trace_bout(trace, spar = c(0.8, 0.27), 
                                       nknots = c(1000, signif(nrow(trace) * .03, 1)), 
                                       window = nrow(trace)/200,
                                       depth_thresh = 10)

# here is what this smoothing method looks like-- bout is light blue line 
ggplot(trace, aes(x = time, y = depth)) + geom_line(color = "grey") + 
  geom_line(data = trace_smooth_bout, aes(x = time, y = smooth_depth, color = bout), size = 1) + 
  theme(legend.position = "none")
# you can certainly see the different bouts of dives in this method! I created 
# this smoothing method because it might help increase the resolution of 
# post-dive surface intervals. The gcv and residual sums of squares value for 
# this model is lowest in this model in comparison to the two above. 
# here is another look at this method: 
ggplot(trace[118000:160000,], aes(x = time, y = depth)) + geom_line(color = "grey") + 
  geom_line(data = trace_smooth_bout[118000:160000,], aes(x = time, y = smooth_depth, color = bout), size = 1) + 
  theme(legend.position = "none")

# comparing the three smoothing methods with the original data: 
# smoothing with depth bounds is in blue, normal smoothing is in red, and bout 
# smoothing method is in dark blue 
ggplot(trace[120000:140000,], aes(x = time, y = depth)) + geom_line(color = "grey") + 
  geom_line(aes(x = time, y = smooth_depth), color = "blue", size = 1) +  
  geom_line(data = trace_smooth[120000:140000,], aes(x = time, y = smooth_depth), color = "red", size = 1) + 
  geom_line(data = trace_smooth_bout[120000:140000,], aes(x = time, y = smooth_depth, color = bout), size = 0.5) + 
  theme(legend.position = "none")

# comparing another section of the record, where the methods diverge: 
ggplot(trace[1000:11000,], aes(x = time, y = depth)) + geom_line(color = "grey") + 
  geom_line(aes(x = time, y = smooth_depth), color = "blue", size = 1) +  
  geom_line(data = trace_smooth[1000:11000,], aes(x = time, y = smooth_depth), color = "red", size = 1) + 
  geom_line(data = trace_smooth_bout[1000:11000,], aes(x = time, y = smooth_depth, color = bout), size = 0.5) + 
  theme(legend.position = "none")

# comparing another section with the max depth: 
ggplot(trace[130000:160000,], aes(x = time, y = depth)) + geom_line(color = "grey") + 
  geom_line(aes(x = time, y = smooth_depth), color = "blue", size = 1) +  
  geom_line(data = trace_smooth[130000:160000,], aes(x = time, y = smooth_depth), color = "red", size = 1) + 
  geom_line(data = trace_smooth_bout[130000:160000,], aes(x = time, y = smooth_depth, color = bout), size = 0.5) + 
  theme(legend.position = "none")

# also added dive component assignment within the smoothing functions: 
ggplot(trace[143000:157000,], aes(x = time, y = smooth_depth)) +
  geom_line(aes(time, depth), color= "gray", size = 0.2) +
  geom_point(aes(color = deriv > 0)) +
  geom_line() + 
  scale_color_discrete(name = "", labels = c("Ascent", "Descent"))

# Looking at the difference between the depth and smoothed values 
# to make sure nothing weird is happening here: 
ggplot(trace, aes(x = time, y = (depth - smooth_depth))) + geom_line()

# Looking at the new maximum depth: this one should be as close as possible to 
# 319 meters, but has definitely changed with smoothing 
max(trace$smooth_depth[1:210000])

################################################################################
## STEP SIX:  Dive statistics, direction flagging, etc##########################
################################################################################
# calling the function to add dates and times to the data 
trace <- add_dates_times(trace, start_time = "1981:01:16 15:10:00")

# plotting 
ggplot(trace[190000:198272,], aes(x = date_time, y = smooth_depth)) + geom_line()
# checking out the end slice -- the end time should be 1/23/1981 11:10:00, as 
# defined by the 1990's team, which checks out! For this record they defined the 
# end of the record after the last dive was made by the seal. 

# careful -- this function also creates duplicated times from points that are 
# close together: 
group_times <- dplyr::group_by(trace, date_time) %>% summarize(count = n())
# plotting duplicated points; most happen around a bout of dives
ggplot(trace, aes(x = date_time, y = smooth_depth)) + geom_line() + 
  geom_line(data = group_times, aes(x = date_time, y = count), color = "red")
# many observations are dropped when removing duplicates:
trace_no_dupes <- trace[!duplicated(trace$date_time),]
# but visaully the record still looks the same:
ggplot(trace[1:16000,], aes(x = date_time, y = depth)) + geom_line() + 
  geom_line(data = trace_no_dupes[1:10948,], aes(x = date_time, y = depth), color = "red")

# I was wondering if I could detect haul-out behavior from the difference 
# between time dots (dots should be closer since the motor rolling the film  
# slows from Antarctic air temperatures). But there doesn't seem to be a clear 
# trend in this record: 
ggplot(trace, aes(x = x_val, y = y_val)) + geom_point() + 
  geom_line(data = time_dots, aes(x = x_val, y = c(0, diff(y_val)*100)), color = "red", size = 1)
# I only see a trend in a handful of the records, but this could also mean that 
# the seal might've just not hauled out in some of the records (like this one).

# After this final step, the proposed workflow is to export the final trace data 
# to a csv file, which can be read for further dive analysis in the diveMove 
# package. The diveMove package creates an S4 object using the date_time column 
# and depth. 
