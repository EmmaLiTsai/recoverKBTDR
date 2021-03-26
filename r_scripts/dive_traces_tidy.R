################################################################################
# Authors: EmmaLi Tsai, Dylan W. Schwilk
# Creates scaled time by depth data from a Kooyman-Billups TDR dive trace
################################################################################

# STEPS

# 1. Recenter and fix misalignment (both data inputs)
# 2. transform coordinates by radius arm eqn
# 3. Transform x axis according to time dots
# 4. Smoothing
# 6. Dive statistics, direction flagging, etc.


###############################################################################
# Global constants: 

# radius of the KBTDR arm when scaled up to the size of the physical traces
RADIUS <- 20.6
# height of the KBTDR pivot point when scaled up to the size of the physical
# trace
CENTER_Y <- 11.3

# STEP ONE: Recenter and fix misalignment (both data inputs) #################

# code here is absent because this is really more related to image processing 
# methods, but I created code that would fix this step and center the scan in
# the scan_tidying_functions.R file. This function tidys the trace and csv files 
# that were created from the ImageJ defaults using two functions: 
#   tidy_trace(trace) and tidy_timedots(time_dots)
#
# Here, I also centered the scan using the center_scan(trace, time_dots) 
# function. This function did a fuzzy distance full merge using the "fuzzyjoin" 
# package to use the y-values of the time dots to center the scan in the trace 
# file. 

# Functions found to complete this step were tested in step one of the 
# testing_code.R file. 

## STEP TWO: Apply radius arm transformation #################################
transform_coordinates <- function(trace, time_dots) {
  # applying my new equation, basically just the equation of a circle but takes
  # the original x/y and calculates where the center of the circle would be
  # (h), and uses this new center to find the x value when depth = 0. I did
  # some algebra to fit this math into one line of code
  trace$new_x <- -sqrt((RADIUS^2) - (CENTER_Y^2)) +
    (trace$x_val + sqrt(RADIUS^2 - (trace$y_val - CENTER_Y)^2))

  ## TODO: scale X based on time dots

  return(trace)
}

################################################################################
#   Function: add_timepoints(df, time_dots)
#   Author: EmmaLi Tsai 
#   Date: 12/02/2020
#
#   Function to add the random time points at the bottom of the trace, this was 
#   to link the time points with the data frame, similar to what I will actually 
#   be working with. Modified from Tsai_DiveTrace_Dec11.R file to match the 
#   variable names here. 
# 
#   Improved on 1/9 (ET) - included the beginning of the first time period as 
#   the start of the trace 
#
#   Input: 
#       -   df          : a dataframe that contains x/y values of a dive trace 
#       -   time_dots   : a vector of the x-value of desired time dots 
#
#   Output: 
#       -   df          : with x and y value (y-values are always -1) for the 
#                         correct time points
# 
################################################################################

## add_timepoints_red <- function(df, time_dots){
##   # defining some empty numeric vectors to store values
##   time_points <- numeric(length = nrow(df))
##   time_points_y <- numeric(length = nrow(df))
##   # stepping through each row of the time points 
##   for (i in 1:nrow(time_dots)){
##     # finding the index in the df where the x_val is closest to the 
##     # time point 
##     if (i == 1){
##       # coding for the special case in the trace before the first time 
##       # dot was made. This still counts as a first time period. 
##       time_points[i] = df$new_x[i]
##       time_points_y[i] = time_dots$Y[i]
##     }
    
##     min.index <- which.min(abs(df$new_x - time_dots$x_val[i]))
##     # stepping through the data frame to connect these two 
##     for (z in 1:nrow(df)){
##       if (z == min.index){
##         # if the index row is the minimum index that was identified, 
##         # add these two values to our vectors 
##         time_points_y[z] = time_dots$Y[i]
##         time_points[z] = time_dots$x_val[i]
##       }
##     }
##   }
##   return(data.frame(time_points = time_points, time_points_y = time_points_y))
## }

# making sure it still works -- woo!!

# this produces a really long data frame that has 0's where the data point is 
# not close to a time point. This was needed so I could easily add these as 
# columns to the full data frame 
time_points <- add_timepoints_red(trace, time_dots)

# adding it to trace data frame
trace$time_points_x <- time_points$time_points
trace$time_points_y <- time_points$time_points_y

# just checking to see that all the time points were connected correctly
# just_timepoints <- trace[(trace$time_points_x!=0),]

################################################################################
# Approach for x-axis Transformation
################################################################################

# First, I created a flagging variable to identify the time period for each part 
# of the trace. 

################################################################################
#   Function: flag_timeperiod(df)
#   Author: EmmaLi Tsai 
#   Date: 1/21/2021
#
#   Function that creates a numeric vector that identifies the time periods for 
#   each segment of a trace. This function was needed in order to make the time 
#   x-axis easier to calculate. 
# 
#   Input: 
#       -   df          : a dataframe that contains x/y values of a dive trace 
#
#   Output: 
#       -   time_flag   : numeric vector that contains the time period for that 
#                         segment of the trace. This can be added as a new row 
#                         the input df. 
# 
################################################################################

flag_timeperiod <- function(df){
  # empty numeric vector to hold the time period value
  time_flag <- numeric()
  # time period number 
  time_period = 0 
  # stepping through each time period 
  for (i in 1:nrow(df)){
    # if the a new time point is encountered 
    if (df$time_points_x[i] != 0){
      # overwrite the rest of the df with that time period value 
      time_flag[i:nrow(df)] = time_period + 1 
      # add a single value to the time period for the next period that is 
      # encountered 
      time_period = time_period + 1
    }
  }
  # return the time flag vector 
  return(time_flag)
}

# adding it to trace df 
trace$time_period <- flag_timeperiod(trace)

# Okay, now that I have a flag for the different time periods, time to see if I  
# can create a running time with the time scales I created above. 

# I noticed issues with time in previous approach. Here, I am trying a new on  
# 3/3/2021 ET

# just grabbing the time points on the trace: 
just_tp <- trace[which(trace$time_points_x!=0),]

# making a small data frame with time periods, start/end values, and scale 
# factor:
tp_df <- data.frame(tp = just_tp$time_period, 
                    start_x = just_tp$new_x, 
                    end_x = lead(just_tp$new_x))

# creating the scale using the start/end time periods 
tp_df$scale = TIME_PERIOD / (tp_df$end_x - tp_df$start_x)

################################################################################
#   Function: running_time(trace, tp_df)
#   Author: EmmaLi Tsai 
#   Date: 3/3/2021
#
#   This simple function takes the trace and time periods data frame (with time
#   period, start_x, end_x, and scale) to create a running time. I believe this 
#   might be easier with a mutate function, but essentially this assesses the 
#   distance a point is from the beginning of a time period and multiples that 
#   distance by the scale I calculated in the tp_df data frame. 
# 
#   Input: 
#       -   trace       : a data frame that contains x/y values of a dive trace 
#                         and time periods. 
#       -   tp_df       : a data frame that contains the time period (tp), start
#                         x (start_x), and end x (end_x) of that time period, as 
#                         well as the scaling factor that I calculated (scale). 
#                         This was basically a helper data frame to make this 
#                         function very simple. 
#
#   Output: 
#       -   time        : numeric vector that contains all the running time 
#                         calculations. This can later be attached to the trace 
#                         data frame using cbind()
# 
################################################################################

running_time <- function(trace, tp_df){
  # creating an empty numeric vector to store time values 
  time <- numeric()
  # looping through each row in the trace
  for (i in 1:nrow(trace)){
    # grabbing the index value that corresponds with that time period. I needed 
    # this in order to use the tp_df (time_periods data frame) that we created 
    # earlier 
    index = trace$time_period[i]
    # calculating the time for that data point. This basically just calculates 
    # the distance between the data point and the beginning of the time period 
    # and uses that distance with the time scale from tp_df to calculate the
    # time for that point 
    time[i] <- ((trace$new_x[i] - tp_df$start_x[index]) * tp_df$scale[index]) + ((tp_df$tp[index]-1)*12)
  }
  # returning the time numeric vector 
  return(time)
}
# checking to see if it works: 
time <- running_time(trace, tp_df)
# adding it to trace df 
trace$time <- time

# proving that it works: 
# trace[which(trace$time_points_x!=0),]

# plot of bout one to assess the quality of this method: 
# ggplot(trace[500:9000,], aes(x = time, y = smooth_y)) + geom_line()

################################################################################
# Author: EmmaLi Tsai
# Topic:  Centering the scan!
# Date:   2/1/2021
################################################################################
# The code below is still a work in progress... 
# The function I made works, but there is definitely an easier way to do this! 
# Centering was necessary since the trace was moving as I was feeding it into
# the scanner. 

# This approach takes the y values of the time dots and moves them and the trace
# above up/down such that the y-values are all y = DIST_TIMEDOT. This just 
# "straightens" the trace by making the time dots into a straight line. 
time_dots$y_val_corr <- (abs(time_dots$Y) - DIST_TIMEDOT)

################################################################################
#   Function: center_scan(df, time_dots)
#   Author: EmmaLi Tsai 
#   Date: 2/10/2021
#
#   This function takes the trace df and time_dots csv file to "center" the scan 
#   of the trace. This step is necessary because the data are off centered as my
#   hands were guiding the trace into the scanner. Essentially, this function 
#   loops through the data frame and uses the distance the time points are from 
#   y = DIST_TIMEDOTS to move the smooth_y values up or down. For example, if 
#   the time point was at y = - 0.2cm, all points within this time period would 
#   be shifted down 0.9cm such that the time point would be at y = DIST_TIMEDOTS
#   (in this trace it would be y = -1.1). This step was needed before reducing  
#   the noise of the trace at depth = 0. 
#
# 
#   Input: 
#       -   df          : a dataframe that contains x/y values of a dive trace 
#       -   time_dots   : a dataframe that contains the x and y values of the 
#                         time points. Another column should be added named 
#                         "y_val_corr", which is the abs(Y). This was needed due
#                         to the way ImageJ sets origins. 
#
#   Output: 
#       -  The output of this function is two cbinded vectors: 
#       -   center_y    : The smooth_y value in the trace data frame with the y 
#                         value correction.
#       -   center_y_   : column that is mainly there as a sanity check. Time 
#           time_point    dots should have a value of DIST_TIMEDOTS in this 
#                         column, and any value between time dots is the value 
#                         that was added to the smooth_y value in the trace. 
# 
################################################################################

# THIS FUNCTION IS BASED ON THE SMOOTH_Y VALUE IN THE DF  
center_scan <- function(df, time_dots){
  # finding the outliers that I made while image processing 
  outliers <- which(diff(time_dots$y_val_corr) > 0.1)
  # creating a numeric vector to append new y values 
  center_y <- numeric() 
  # numeric vector to append time point values to ensure that the function is 
  # working 
  center_y_time_point <- numeric()
  # looping through data frame 
  for(i in 1:nrow(df)){
    if(i == 1){
      # special case of first index 
      index <- 1
    } else{
      # index that I am working with for the time_dots dataframe is the time 
      # period value in the trace df 
      index <- df$time_period[i-1]
    }
    # special cases for outlier values that happened via image processing 
    if (index %in% outliers){
      center_y[i] <- df[i,]$smooth_y + time_dots[(index-1),]$y_val_corr 
      center_y_time_point[i] <- df[i,]$time_points_y + time_dots[(index-1),]$y_val_corr 
      
    }
    # center Y is the smooth_y value plus the time dots correction
    center_y[i] <- df[i,]$smooth_y + time_dots[index,]$y_val_corr 
    # center y time point is the time points value plus the y corrected 
    center_y_time_point[i] <- df[i,]$time_points_y + time_dots[index,]$y_val_corr 
    # center y time points SHOULD BE DIST_TIMEDOTS for time points, since I am 
    # shifting everything up/down so the time points are along  
    # y = DIST_TIMEDOTS to "center" the scan 
  }
  # returning the binded vectors 
  return(cbind(center_y, center_y_time_point))
}

# running the function above 
center_y <- center_scan(trace, time_dots)
# adding this as a new row to the data frame 
trace$center_y <- center_y[,1]

# great! so now that scan should be centered across the trace. 

################################################################################
# Author: EmmaLi Tsai
# Topic:  Removing noise at depth = 0 and depth scale
# Date:   2/24/2021
# MAJOR UPDATE: 3/23, ET
################################################################################

# depth scale: 

# THIS APPROACH WILL BE DIFFERENT FOR ALL TRACES BEFORE 1981 ! 1981 traces have 
# depth calibration at the end of the trace, previous ones do not. 

# the curve at the end corresponds to different pressures in psi. This needs to 
# be converted to depth. 

# calculating psi values: 
trace$psi <- ((trace$center_y * MAX_PSI) / max(trace$center_y))

# with each 1m increase in depth, there is a 1.4696psi increase in pressure in 
# saltwater: 
trace$depth <- trace$psi / (1.4696)

# this is a quick way to reduce noise at depth = 0, this can be done in the 
# diveMove package
trace[which(trace$depth < 0),]$depth <- 0

# plotting to look at the whole trace
# ggplot(trace, aes(x = time, y = depth)) + geom_line()

# looking at different bouts to assess how this method worked
# bout one 
# ggplot(trace[1000:9000,], aes(x = time, y = center_y)) + geom_line()
# 
# # bout two 
# ggplot(trace[35000:45000,], aes(x = time, y = center_y)) + geom_line() 
# 
# # bout three 
# ggplot(trace[73000:88800,], aes(x = time, y = center_y)) + geom_line() 
# 
# # bout four 
# ggplot(trace[138000:151000,], aes(x = time, y = center_y)) + geom_line()


# looking at different bouts 
# bout one 
# ggplot(trace[1000:9000,], aes(x = time, y = depth)) + geom_line()
# 
# # bout two 
# ggplot(trace[35000:45000,], aes(x = time, y = depth)) + geom_line() 
# 
# # bout three 
# ggplot(trace[73000:88800,], aes(x = time, y = depth)) + geom_line() 
# 
# # bout four 
# ggplot(trace[138000:151000,], aes(x = time, y = depth)) + geom_line()

# plotting again... this is close to what the final product should be. 
ggplot(trace, aes(x = time, y = depth)) + 
  geom_line(aes(time, y), color="gray", size=0.2) + 
  geom_line() +
  theme_bw() + 
  labs(x = "Time (min)", y = "Depth (m)", title = "WS_25_1981") + 
  scale_x_continuous(position = "top") + 
  scale_y_reverse()

# warning message is from last point in the trace, where I couldn't assign a
# time value. 

################################################################################
# Author: EmmaLi Tsai
# Topic:  Adding dates and times 
# Date:   3/7/2021
################################################################################

# removing last row which had NA time value-- I'm working currently working on 
# this issue. With the arc removal, the calibration curve at the end gets 
# moved over in the +x direction and past the last time point, so I couldn't 
# determine time for the last couple of rows in the trace.
trace <- trace[1:(nrow(trace)-1),]

# creating date_time column using the lubridate package 
trace$date_time <- ymd_hms(START_TIME, tz = "Antarctica/McMurdo") + 
  minutes(as.integer(trace$time)) + 
  seconds(as.integer((trace$time %% 1) * 60))

# plotting: 
# ggplot(trace, aes(x = date_time, y = depth)) + geom_line()

# currently working on comparing the dive statistics from this recovered trace 
# with the Castellini et al., 1992 bulletin using the diveMove() package
