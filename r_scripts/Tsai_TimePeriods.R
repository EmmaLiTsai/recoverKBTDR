################################################################################
# Author: EmmaLi Tsai
# Topic: Time Periods, Depth, and Smoothing  
# Date: 2021-1-18
################################################################################

# This document builds off the arc transformation .R file to also include work
# on the time periods, depth scale, and spline smoothing. 
#
# Tracking this file via git 
#
# The final output of this script produces a fully recovered trace, but some 
# small issues still need to be fixed: 
# 
# TODO: I need to double-check the depth transformation. The transformation 
#       itself is easy, but I had to "center" the scan and I'm not confident 
#       that I did it correctly. There is a software from NeruaScanner that can
#       center the scan for me, but I want to see if I can do it in code. 
# 
# TODO: some of the smoothed y_values seem to stray from the overall trend of
#       the trace (i.e. picking up the other edge of the line). Maybe this could 
#       be fixed by changing the spar value? 
#
# TODO: check the output of this code with the castellini (1992) bulletin. The 
#       max depth for this trace seems to align, but confirm other dive
#       parameters to ensure the quality of these methods. 

################################################################################
# Loading packages

library(ggplot2)
library(dplyr)

################################################################################
# Constants 

# radius of the KBTDR arm when scaled up to the size of the physical traces 
RADIUS <- 19.17
# height of the KBTDR pivot point when scaled up to the size of the physical 
# trace
CENTER_Y <- 11.5

# both the RADIUS and CENTER_Y should be constant across all KBTDR devices. 

# defining the time period scale. THIS CHANGES ACROSS TRACES!
TIME_PERIOD <- 12

# defining max depth for depth scale. THIS ALSO CHANGES ACROSS TRACES! 
MAX_DEPTH <- 800

################################################################################
# Read data

# this is data from an actual trace from 1981. 

# I plan to push the workflow that I've been using to process the scanned
# images to GitHub in the future
trace <- read.csv("./WS_25_1981_fulltrace_xy.csv", header = TRUE, 
                  stringsAsFactors = FALSE)

# this csv contains the timing dots for the trace. I gathered these by maually 
# clicking each time point with the "point" tool in ImageJ, but am looking for a 
# macros that can do this with more accuracy. 
time_dots <- read.csv("./WS_25_1981_timedots_guess.csv", header = TRUE, 
                      stringsAsFactors = FALSE)

################################################################################
# Transforming the arc using the new approach
# This code is from the Tsai_ArcFixing_NewApprach.R file 
################################################################################

# removing an extra column
trace <- select(trace, -c(Y))
# changing the names to match the conventions I've been using 
names(trace) <- c("x_val", "y_val")

# applying my new equation, basically just the equation of a circle but takes 
# the original x/y and calculates where the center of the circle would be (h), 
# and uses this new center to find the x value when depth = 0. 
# I did some algebra to fit this math into one line of code 
trace$new_x <- -sqrt((RADIUS^2) - (CENTER_Y^2)) + (trace$x_val + sqrt(RADIUS^2 - (trace$y_val - CENTER_Y)^2))

# checking a slice to see if it works 
ggplot(trace[1500:11000,], aes(x = new_x, y = y_val)) + geom_point()

# Rounding the data since some values are too specific with the kind of 
# precision that I'm working with
trace$new_x <- round(trace$new_x, 2)

################################################################################
# Author: EmmaLi Tsai 
# Topic: Testing out possible depth transformation method 
# Date: 1/21/2021
################################################################################

# CAUTION -- THIS IS A WORK IN PROGRESS, STILL NEED TO ASSESS IF IT ACTUALLY 
# WORKS, SEE TODO LIST ABOVE 

# Transforming the y_val since the values at the beginning of the trace 
# are off from the way I scanned it. 
# There will always be noise at depth y = 0 from the way the device functions. 
# The led arm is less "stiff" at depth = 0 since there is no tension on the arm 
# at atmospheric pressure. 

trace$new_y <- abs(trace$y_val + (trace$y_val - trace$y_val[1]))

# scaling this to depth
# THIS APPROACH WILL BE DIFFERENT FOR ALL TRACES BEFORE 1981 ! 1981 Traces have 
# depth calibration at the end of the trace, previous ones do not. 
trace$depth <- ((trace$new_y * MAX_DEPTH) / max(trace$new_y))

# plotting 
ggplot(trace[1500:11000,], aes(x = new_x, y = depth)) + geom_point() + theme_bw()

################################################################################
# Author: EmmaLi Tsai 
# Topic: Spline Smoothing 
# Date: 1/21/2021
################################################################################

# I think there may be a way to clean up the code in this section, maybe with 
# some of the lapply functions? I'm trying to move away from my function-heavy 
# code... 

# mutating off the transformed depth now 
trace <- mutate(trace, new_x = new_x, y=depth) %>%
  select(y, new_x) %>%
  arrange(new_x)


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
# way I scanned the physical trace 
ggplot(trace[1500:11000,], aes(x = new_x, y = smooth_y)) +
  geom_line(aes(new_x, y), color="gray", size=0.2) +
  geom_point(aes(color=deriv > 0)) +
  geom_line()


################################################################################
# Author: EmmaLi Tsai
# Topic: Time periods-- New Approach 
# Date: 1/18/2021
################################################################################

# This script transforms the x-axis into time. 

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
      time_points_y[i] = -1
    }
    
    min.index <- which.min(abs(df$new_x - time_dots$x_val[i]))
    # stepping through the data frame to connect these two 
    for (z in 1:nrow(df)){
      if (z == min.index){
        # if the index row is the minimum index that was identified, 
        # add these two values to our vectors 
        time_points_y[z] = -1
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
add_timepoints_red(trace, time_dots)

# adding it to trace data frame
trace$time_points_x <- add_timepoints_red(trace, time_dots)$time_points
trace$time_points_y <- add_timepoints_red(trace, time_dots)$time_points_y

# just checking to see that all the time points were connected correctly
just_timepoints <- trace[(trace$time_points_x!=0),]

################################################################################
# Start of New Approach for x-axis Transformation
################################################################################

# This new approach transforms the x-axis into time, without worrying about the 
# original x positions. It basically takes the time period (12 minutes in this 
# case) and divides this by the number of rows between two time periods to find 
# a time scale for that time period. I then made a running sum where each row 
# time was created by adding the time scale value to the previous row. 

# To do this, I first needed to find the indices of the time points for the
# whole trace: 
time_points_index <- which(trace$time_points_x!=0)

# I then took these values found the difference between the indices, which gave
# me the number of rows between two time points. I divided the TIME_PERIOD by 
# this number, which is defined at the top of this code. 

# CAUTION- TIME PERIODS CHANGE BETWEEN TRACES 
time_scales <- TIME_PERIOD / (diff(time_points_index))

# I then created a flagging variable to identify the time period for each part 
# of the trace. This was needed to make the cumulative time a bit easier to 
# calculate. 

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


################################################################################
#   Function: running_time(df, time_scales)
#   Author: EmmaLi Tsai 
#   Date: 1/21/2021
#
#   Function that creates a numeric vector that identifies the time for each row
#   of a trace. This function was needed in order to make the time x-axis. 
# 
#   Input: 
#       -   df          : a dataframe that contains x/y values of a dive trace 
#       -   time_scales : a numeric vector that contains the time scale for a 
#                         specific time period. Mathematical process for these 
#                         values were described above. 
#
#   Output: 
#       -   time       : numeric vector that contains the running time for each
#                        row of the trace. This can be added to the original 
#                        trace data frame. 
# 
################################################################################

running_time <- function(df, time_scales){
  # creating a numeric time vector 
  time <- numeric()
  # stepping through each row in the data frame 
  for (i in 1:nrow(df)){
    # if the index is = 1, this is the start time 
    if (i == 1){
      # time for this index will be 0
      time[i] = 0 
    } else {
      # for all other indices, the time will be equal to the 
      # time before, plus the correct time scale value 
      time[i] = time[i-1] + time_scales[df$time_period[i-1]]
    }
  }
  # return the time numeric vector 
  return(time)
}

# adding this numeric vector to the trace df 
trace$time <- running_time(trace, time_scales)

# proving that it works: 
trace[which(trace$time_points_x!=0),]

# also proving with plotting 
ggplot(trace, aes(x = time, y = smooth_y)) +
  geom_line(aes(time, y), color="gray", size=0.2) +
  geom_point(aes(color=deriv > 0)) +
  geom_line() + 
  labs(x = "time (min)", y = "depth (m)", title = "WS_25_1981")


# Looking good! Still need to tweak some things
ggplot(trace, aes(x = time, y = smooth_y)) + 
  geom_line(aes(time, y), color="gray", size=0.2) + 
  geom_line() +
  theme_bw() + 
  labs(x = "Time (min)", y = "Depth (m)", title = "WS_25_1981") + 
  scale_x_continuous(position = "top") + 
  scale_y_reverse()





