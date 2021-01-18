################################################################################
# Author: EmmaLi Tsai
# Topic: Fixing the Arc-- New Transformation 
# Date: 2020-1-18
################################################################################

# This script tests out the new approach for fixing the arc using the geometry
# of the KBTDR device. 

# I also included some very preliminary code for the time periods on line 118

# TODO: spline smoothing works better when df is small-- thinking about creating 
# a function that finds the smooth_y values over smaller chunks of the trace?

# TODO: y-values at the beginning of the trace are slightly off from the way I 
# scanned the trace. Neurascanner has a software that can fix this but I'm sure
# I can figure it out in code 

# TODO: currently working on the scaling factor for the time periods. I tried 
# some things out but found that my method produced gaps or overlapping points 
# between time periods. 

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
# Step 1: Transforming the arc using the new approach
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

# applying group_by() and summarize to find median y-values
# ** unsure if this step is needed with the new smoothing approach 
trace <- group_by(trace, new_x) %>% summarize(y_val = median(y_val))

# Applying the smoothing code created by Dylan: 
trace <- mutate(trace, new_x = new_x, y=y_val) %>%
  select(y, new_x) %>%
  arrange(new_x)

# TODO: spline smoothing works better for the traces when the df is low-- so 
# maybe I need to create some sort of loop that does this for segments of the 
# trace?
# I find that running the smooth.spline on a full trace where df is high results 
# in poor resolution, regardless of the spar value I define

# creating a smooth spline 
spline.mod <- smooth.spline(trace$new_x, trace$y, spar = 0.1)

# mutating to find the predicted values after spline smoothing and take the 
# derivative to identify ascents/descents 
trace <- mutate(trace, smooth_y = predict(spline.mod, trace$new_x)$y,
                deriv = predict(spline.mod, trace$new_x, deriv=1)$y,
                ascent = deriv < 0,
                deriv_diff=lag(sign(deriv)) - sign(deriv),
                peak = case_when(deriv_diff < 0 ~ "TOP",
                                 deriv_diff > 0 ~ "BOTTOM")
)


# Graphing-- this is so cool!!! 
# very large y-values at the end of the trace when the scientists calibrated
# the TDR for depth. 
# also evident that the y-values at the beginning of the trace are off from the 
# way I scanned the physical trace 
ggplot(trace, aes(x = new_x, y = smooth_y)) +
  geom_line(aes(new_x, y), color="gray", size=0.2) +
  geom_point(aes(color=deriv > 0)) +
  geom_line()


################################################################################
# Author: EmmaLi Tsai
# Topic: Time periods 
# Date: 2020-1-18
################################################################################

# This preliminary script works on the time periods-- still a work in progress! 

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


# I need to create a data frame with: 
# start X, end X, start time, end time, and scaling factor such that each 
# time period has one row 

# adding the start and end positions of the time period 
time_periods <- data.frame(start_x = just_timepoints$time_points_x, 
                           end_x = lead(just_timepoints$time_points_x, 1))

# adding the start and end times of the time periods on the above data frame 
# time is added by taking the row number of the data which would represent the 
# time period, multiplying by 12, and then subtracting by 12 since we start at 
# 0 time. Everything is represented in minutes.
time_periods$start_t <- (as.integer(row.names(time_periods)) * 12) - 12 
# adding the end time using the lead() function, similar as above
time_periods$end_t <- lead(time_periods$start_t, 1)

# TODO: currently working on the scaling factor. I tried some things out but 
# found that my method produced gaps or overlapping points between time periods.  

