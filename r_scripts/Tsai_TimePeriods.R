################################################################################
# Author: EmmaLi Tsai
# Topic: Time Periods, Depth, and Smoothing  
# Date: 2021-1-18
################################################################################
# 
#
# This document builds off the arc transformation .R file to also include work
# on the time periods, depth scale, centering, and spline smoothing. The final 
# graph is the closest fully recovered trace that I have made so far ET (2/24)
#
#
# The final output of this script produces a recovered trace, but some 
# small issues still need to be fixed: 
# 
# TODO: Currently, I am working on reducing the "noise" at depth = 0, and 
#       fixing the spline smoothing so that the smoothed line picks up when the 
#       seal resurfaces for ~10 minutes during a bout of dives. This is the most 
#       urgent task before I attempt the ones below. 
#  
#   UPDATE: somewhat resolved? Smoothing issues are resolved, but I am currently
#       checking the new method for removing noise at depth = 0. See last 
#       section of this document. ET 2/24
# 
# TODO: I need to double-check the depth transformation. The transformation 
#       itself is easy, but I had to "center" the scan and I'm not confident 
#       that I did it correctly. There is a software from NeruaScanner that can
#       center the scan for me, but I want to see if I can do it in code. 
# 
#   UPDATE: I have centered the scan by using the timing dots in this update. 
#       See code below. ET 2/15
# 
# TODO: some of the smoothed y_values seem to stray from the overall trend of
#       the trace (i.e. picking up the other edge of the line). Maybe this could 
#       be fixed by changing the spar value? 
# 
#   UPDATE: This issue is somewhat fixed by grouping the data by x value and 
#       the spline smoothing function that I created that breaks up the data 
#       into chunks and applies smoothing over smaller degrees of freedom. 
#       However, a new challenge emerges when trying to identify when the seal 
#       surfaces within a bout of dives-- see to-do task number 1. ET 2/15
# 
#   UPDATE: This has been resolved. I changed the image processing workflow to 
#       skeletonize the trace, which greatly improved smoothing.  ET 2/24
#         
# TODO: check the output of this code with the castellini (1992) bulletin. The 
#       max depth for this trace seems to align, but confirm other dive
#       parameters to ensure the quality of these methods. 
#
#   UPDATE: removed depth transformation section for now, will come back to this 
#       once I tackle the noise and smoothing issues above. ET 2/15
#
#   UPDATE: modified image processing by using "skeletonization" which helps the 
#       smoothing functions capture more of the behavior at depth. 
################################################################################
# Loading packages

library(ggplot2)
library(dplyr)

# this was added to add dates and times... We might not need to add this in the
# final package, but this information is necessary in order for the recovered 
# trace to work well in dive stats packages
library(lubridate)

################################################################################
# Global constants: 

# radius of the KBTDR arm when scaled up to the size of the physical traces 
RADIUS <- 20.6
# height of the KBTDR pivot point when scaled up to the size of the physical 
# trace
CENTER_Y <- 11.3


# Constants below change based on the trace: 

# defining the time period scale. THIS CHANGES ACROSS TRACES!
TIME_PERIOD <- 12

# defining max depth for depth scale. THIS ALSO CHANGES ACROSS TRACES! 
MAX_DEPTH <- 800

# distance from trace to timing dots in cm-- this is used for centering the scan
DIST_TIMEDOT <- 1.1

# start time and date for a trace, in ymd_hms format 
START_TIME <- "1981:01:16 15:10:00"

################################################################################
# Read data

# this is data from an actual trace from 1981. 

# UPDATE -- changed image processing workflow, so the file names changed ET 2/24
# see image processing methods in sample_data folder.
trace <- read.csv("./skele_trace.csv", header = TRUE, 
                  stringsAsFactors = FALSE)

# this csv contains the timing dots for the trace. I gathered these by using the 
# wand selection tool in ImageJ to grab the time dot, and automatically 
# calculating the centroid of this dot. 
time_dots <- read.csv("./skele_timedots.csv", header = TRUE, 
                      stringsAsFactors = FALSE)

# making this the new column value 
trace$y_corr <- trace$y_corr_p2

# some tidying for the time_dots file: 
time_dots <- select(time_dots, -c("X.1"))
names(time_dots) <- c("x_val", "Y")

################################################################################
# Transforming the arc using the new approach
# This code is from the Tsai_ArcFixing_NewApprach.R file 
################################################################################

# removing extra columns
trace <- select(trace, -c("Y", "y_corr"))
# changing the names to match the conventions I've been using 
names(trace) <- c("x_val", "y_val")

# applying my new equation, basically just the equation of a circle but takes 
# the original x/y and calculates where the center of the circle would be (h), 
# and uses this new center to find the x value when depth = 0. 
# I did some algebra to fit this math into one line of code 
trace$new_x <- -sqrt((RADIUS^2) - (CENTER_Y^2)) + (trace$x_val + sqrt(RADIUS^2 - (trace$y_val - CENTER_Y)^2))

# checking a slice to see if it works 
# ggplot(trace[1500:11000,], aes(x = new_x, y = y_val)) + geom_point()

# Rounding the data since some values are too specific with the kind of 
# precision that I'm working with
trace$new_x <- round(trace$new_x, 3)

# changing the time_dots y value to negative due to origin placement in ImageJ
time_dots$Y <- time_dots$Y * -1

################################################################################
# Author: EmmaLi Tsai 
# Topic: Spline Smoothing 
# Date: 1/21/2021
################################################################################

# I think there may be a way to clean up the code in this section, maybe with 
# some of the lapply functions? I'm trying to move away from my function-heavy 
# code... 

# mutating off the transformed depth now 
trace <- mutate(trace, new_x = new_x, y=y_val) %>%
  select(y, new_x) %>%
  arrange(new_x)

# grouping trace by x value and summarizing by median y for easier spline 
# smoothing
trace <- group_by(trace, new_x) %>% summarize(y = median(y))

################################################################################
#   Function: split_smoothing(df, n = 40, spar = 0.4)
#   Author: EmmaLi Tsai 
#   Date: 1/21/21
#
#   Function that breaks up the trace into different segments (default is set to
#   n = 40) so spline smoothing works a bit better. This function was necessary 
#   because smoothing at the scale of the whole trace resulted in poor 
#   resolution of the rest of the trace, regardless of the spar value defined in 
#   the smoothing function. 
#
#   Input: 
#       -   df          : a dataframe that contains x/y values of a dive trace 
#       -   n = 40      : the number of chunks the user would like to use to 
#                         divide the trace. Defualt is set to 40. 
#       -   spar = 0.4  : spar value used for smoothing. 0.4 seems to be the 
#                         resolution that I'd like, but this can always be 
#                         changed later and is likely trace-dependent. 
#
#   Output: 
#       -   trace_new    : data frame that contains the smoothed_y values after
#                          spline smoothing across the whole trace.
# 
################################################################################

split_smoothing <- function(df, n = 40, spar = 0.4) {
  # finding break points in the trace, can be modified using n 
  split_rows <- round(seq(1, nrow(df), length.out = n))
  # creating a data frame with the split points 
  # this just makes it a little bit easier to work with in the loop 
  split_rows_df <- data.frame(start_row = split_rows, 
                              end_row = lead(split_rows, 1))
  # had to remove the last row for looping purposes 
  split_rows_df <- split_rows_df[-nrow(split_rows_df),]
  
  # I will rbind data frame results from the loop here: 
  trace_new <- NULL

  # creating a for loop 
  for (i in 1:nrow(split_rows_df)){
    # defining the segment of the trace 
    trace <- df[(split_rows_df$start_row[i]:split_rows_df$end_row[i]),]
    # new spline mod, with editable spar value that can be defined at the 
    # function call 
    spline.mod <- smooth.spline(trace$new_x, trace$y, spar = spar)
    # the new segment of the trace 
    trace <- mutate(trace, smooth_y = predict(spline.mod, trace$new_x)$y,
                    deriv = predict(spline.mod, trace$new_x, deriv=1)$y,
                    ascent = deriv < 0,
                    deriv_diff=lag(sign(deriv)) - sign(deriv),
                    peak = case_when(deriv_diff < 0 ~ "TOP",
                                     deriv_diff > 0 ~ "BOTTOM"))
    # rbinding results from this loop
    trace_new <- rbind(trace_new, trace)
  }
  # returning new trace 
  return(trace_new)
}

# calling the function 
trace <- split_smoothing(trace)

# Graphing-- this is so cool!!! 
# very large y-values at the end of the trace when the scientists calibrated
# the TDR for depth. 
# also evident that the y-values at the beginning of the trace are off from the 
# way I scanned the physical trace - centering is needed: 
# ggplot(trace[1000:9000,], aes(x = new_x, y = smooth_y)) +
#   geom_line(aes(new_x, y), color="gray", size=0.2) +
#   geom_point(aes(color=deriv > 0)) +
#   geom_line()

# looking at a specific bout to see how well the smoothing performs... this is 
# greatly improved from the previous image processing methods I was using: 
# ggplot(trace[1000:9000,], aes(x = new_x, y = y)) + geom_point() + 
#   geom_line(aes(x = new_x, y = smooth_y), color = "red", size = 1.1)


################################################################################
# Author: EmmaLi Tsai
# Topic: Adding time dots to the trace data frame 
# Date: 1/18/2021
################################################################################

# This section merges the trace and time dots data frame. I had to create a 
# function here to merge the two files based on the closest x values. 

# rounding the x_value of the time dots since the measurements I made are
# not as precise as 3 decimal places 
time_dots$x_val <- round(time_dots$x_val, 2)

# connecting these time points to the trace data frame with the function I made 
# before: 

################################################################################
#   Function: add_timepoints_red(df, time_dots)
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

add_timepoints_red <- function(df, time_dots){
  # defining some empty numeric vectors to store values
  time_points <- numeric(length = nrow(df))
  time_points_y <- numeric(length = nrow(df))
  # stepping through each row of the time points 
  for (i in 1:nrow(time_dots)){
    # finding the index in the df where the x_val is closest to the 
    # time point 
    if (i == 1){
      # coding for the special case in the trace before the first time 
      # dot was made. This still counts as a first time period. 
      time_points[i] = df$new_x[i]
      time_points_y[i] = time_dots$Y[i]
    }
    
    min.index <- which.min(abs(df$new_x - time_dots$x_val[i]))
    # stepping through the data frame to connect these two 
    for (z in 1:nrow(df)){
      if (z == min.index){
        # if the index row is the minimum index that was identified, 
        # add these two values to our vectors 
        time_points_y[z] = time_dots$Y[i]
        time_points[z] = time_dots$x_val[i]
      }
    }
  }
  return(data.frame(time_points = time_points, time_points_y = time_points_y))
}

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
################################################################################

# this is the section I am currently working on! 

# this is a quick way-- i will build on this code but just wanted to assess this 
# method. 
trace[which(trace$center_y < 0),]$center_y <- 0

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

# depth scale: 

# scaling this to depth

# THIS APPROACH WILL BE DIFFERENT FOR ALL TRACES BEFORE 1981 ! 1981 traces have 
# depth calibration at the end of the trace, previous ones do not. 
trace$depth <- ((trace$center_y * MAX_DEPTH) / max(trace$center_y))

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

# removing last row which had NA time value 
trace <- trace[1:(nrow(trace)-1),]

# creating date_time column using the lubridate package 
trace$date_time <- ymd_hms(START_TIME, tz = "Antarctica/McMurdo") + 
  minutes(as.integer(trace$time)) + 
  seconds(as.integer((trace$time %% 1) * 60))

# currently working on comparing the dive statistics from this recovered trace 
# with the Castellini et al., 1992 bulletin using the diveMove() package