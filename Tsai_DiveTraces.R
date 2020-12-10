################################################################################
# Author:  EmmaLi Tsai 
# Date:    11/9/2020
################################################################################
# 
# This document compiles the code I've been working on to recover the 1970s - 
# 1980s dive traces. I've been using a sample dive trace that I made and 
# transformed into x/y values using ImageJ.
# 
# This R file is broken up into four topics: 
#
#   Topic 1: Transforming & Decomposing Dive Trace
#            Date: 11/9/2020
#            Line: 22
#   Topic 2: Investigating issues w/ time keeping dots & speed
#            Date: 12/2/2020
#            Line: 257
#   Topic 3: Transforming x-axis into time 
#            Date: 12/10/2020
#            Line: 396
#   Topic 4: Testing out the arc equation
#            Date: 12/8/2020
#            Line: 450
#
################################################################################
# Author:  EmmaLi Tsai 
# Date:    11/9/2020
# Topic 1: Transforming & decomposing dive trace 
################################################################################

# Loading Libraries 
library(ggplot2)
library(dplyr)

# Loading in the data
trace <- read.csv("./manual_trace.csv", header = TRUE, 
                  stringsAsFactors = FALSE)

# Moving all x-values over based on y-value to straighten the data 
# THIS IS NOT FULLY TRANSFORMED -- JUST MAKES IT EASIER TO CODE 
trace$new_x <- trace$x_val + trace$y_val*0.5

###############################################################################
# Flag Function 
#   Author: EmmaLi Tsai 
#   Date: 11/08/2020
#   
#   This is a function that creates a "flagging" variable that identifies the 
#   different portions of a dive. "AC" represents an ascent, "DC" represents the 
#   descent portions of a dive, "X" is a small variable that I produced to 
#   mark the end of the trace, "MD" identifies the max depth of a dive, "SP"
#   portrays no change in depth (possibly a plane or the seal is resting at the 
#   surface). 
# 
#   SMALL BUG -- switching between plane and max depth is sometimes off (11/9)
#       - resolved in catch_sd function below (11/9)
#
#   Input: 
#       -   df : a dataframe that contains x/y values of a dive trace 
#
#   Output: 
#       - flag : a vector of characters that identifies whether each point is 
#                part of an ascent (AC), descent (DC), plane (SP).  
################################################################################

flag <- function(df) {
  # Creating a vector with repeating "NA"- I did this so I could easily find 
  # errors in the function 
  flag <- rep("NA", nrow(df))
  
  # making sure the data frame is in correct order, or else this function won't 
  # run properly 
  df <- df[order(df$new_x),]
  
  # Stepping through each row in the data frame
  for (i in 1:nrow(df)) {
    # If the index is at the end of the data frame, mark this with an X
    if (i == (nrow(df) - 1)) {
      flag[i:nrow(df)] = "X"
      # break the loop-- I needed this so I can look at values beyond a
      # specific point
      break()
    }
    # Identifying a descent by calculating the slope 
    if (((df$y_val[i + 1] - df$y_val[i]) / (df$new_x[i + 1] - df$x_val[i])) > 0) {
      flag[i] = "DC"
    }
    # Identifying an ascent by calculating the slope 
    if (((df$y_val[i + 1] - df$y_val[i]) / (df$new_x[i + 1] - df$x_val[i])) < 0) {
      flag[i] = "AC"
    }
    # Identifying a plane-- meant to use this to find local max depths, but will 
    # have to find a different approach 
    if (((df$y_val[i + 1] - df$y_val[i]) / (df$new_x[i + 1] - df$x_val[i])) == 0) {
      flag[i] = "PL"
    }
  }
  return(flag)
}

# Organizing the data 
trace <- trace[order(trace$new_x),]
# calling the flag function to add new column, "flag"
trace$flag <- flag(trace)
# plotting the result 

ggplot(trace, aes(x=new_x, y = y_val)) + 
  geom_point(aes(color = flag)) + geom_line()
# From this plot, it is evident that we need to filter the data set a bit more. 

###############################################################################
# filter_trace function
#   Author: EmmaLi Tsai 
#   Date: 12/07/2020
#   
#   This function reduces the trace data frame and filters every 3rd data point 
#   based on the number of iterations the user defines. It is set to 3, since 
#   this was optimal for the specific trace I'm working with right now. This 
#   function was necessary to construct because ImageJ line detection picked up
#   both sides of a single line, which caused an extreme number of overlapping 
#   points. 
# 
#
#   Input: 
#       -   df          : a data frame that contains x/y values of a dive trace 
#       -   num_filter  : number of times user wants to filter the data set 
#
#   Output: 
#       -   filter_trace: filtered data frame  
################################################################################

filter_trace <- function(df, num_filter = 3){
  # keeping all distinct x_values
  filter_trace <- distinct(df, new_x, .keep_all = TRUE)
  # removing every 3rd data point, this process is repeated by 
  # the number of times the user defines. I kept it at 
  # 3 for this specific trace, since 105 obs was simple enough for
  # this data set. Will likely have to change this based on each trace. 
  for (i in 1:num_filter){
    filter_trace <- filter_trace[seq(1, nrow(filter_trace), by = 3),]
  }
  return(filter_trace)
}

# Let's see if it works
red_trace <- filter_trace(trace)

# calling the flag function on the reduced trace
# CAUTION: running this again will change the plot because you'd be overwriting 
# previous flag variables.
red_trace$flag <- flag(red_trace)

################################################################################
# add_type Function 
#   Author: EmmaLi Tsai 
#   Date: 11/08/2020
#   
#   This is a function looks at the previous variables produced by flag() and 
#   adds two new dive parameters: max depth, and start/end points of a dive. 
# 
#
#   Input: 
#       -   df : a dataframe that contains x/y values of a dive trace 
#
#   Output: 
#       -   df : with modified flag column with max depth (MD), and start/end 
#                points of a dive "S.DC".
# 
#   small bug - some start/end points of a dive are not being picked up by this 
#               function. Resolved by the catch_sd function below (12/05)
################################################################################

add_type <- function(df){
  # Coding for the start of the dive and max depth. I need to be able to 
  # identify these parts of the trace so I can apply my function and 
  # fix the arc. 
  for(v in 1:nrow(df)) {
    if (v == (nrow(df) - 1)) {
      # break the loop-- I needed this so I can look at values beyond a
      # specific point
      break()
    }
    # If the flagging variable changes, mark this is S.DC, meaning the
    # start or end of a dive. I limited this to a certain set of y_values to
    # make sure it doesn't happen at depth.
    # small bug here-- some are not being identified
    #  bug fixed in catch_sd function
    if (((df$flag[v] == "AC") && (df$flag[v + 1] == "DC")) && (df$y_val[v] <= 1)) {
      df$flag[v + 1] = "S.DC"
    }
    # Identifying max depth
    if ((df$flag[v] == "DC") && (df$flag[v + 1] == "AC")) {
      df$flag[v + 1] = "MD"
    }
    # Trying to code for the instance of a plane that counts as a max depth
    if ((df$flag[v] == "PL") && (df$flag[v + 1] == "AC") && (df$flag[v - 1] == "DC")) {
      df$flag[v + 1] = "MD"
    }
  }
  return(df)
}

# updating our data frame 
red_trace <- add_type(red_trace)

################################################################################
# catch_sd Function 
#   Author: EmmaLi Tsai 
#   Date: 11/09/2020
#   
#   This is a function takes the output from the add_type function and corrects 
#   a small bug. It makes sure that the start/end of each dive is accounted for. 
#   This had to be resolved with a separate function. I will probably be making 
#   many more of these to code for different kinds of odd cases. 
#
#   Input: 
#       -   df : a dataframe that contains x/y values of a dive trace and flag
#                variables produced from the flag() function 
#
#   Output: 
#       -   df : the same data frame with a small correction to fix the bug that 
#                provides reliable start/end dive positions 
################################################################################

catch_sd <- function(df){
  for(i in 1:nrow(df)){
    if (i == (nrow(df) - 1)) {
      # break the loop-- I needed this so I can look at values beyond a
      # specific point
      break()
    } # adding some conditions to fix the small bug that existed 
    # trying to catch instances of s.dc that were not captured in the flag() 
    # function. 
    if ((df$flag[i]=="DC") && (df$flag[i+1] == "DC") && ((df$flag[i-1] == "AC") | (df$flag[i-1] == "MD")) && (df$y_val <= 1)) {
      df$flag[i] = "S.DC" 
    }
  }
  return(df)
}

# refining the final data frame 
red_trace <- catch_sd(red_trace)

# plotting the final product 
ggplot(red_trace, aes(x=new_x, y =y_val)) + 
  geom_point(aes(color = flag), size = 2, alpha = 0.7) + 
  geom_line(alpha = 0.4, lty = "dashed") + 
  theme_bw() + 
  labs(x = "Transformed X Values (cm)", 
       y = "Y Values (cm)", color = "Type") +
  scale_color_discrete(labels = c("Ascent", "Descent", "Max Depth", "Plane", 
                                  "Start Dive", "End Trace")) 
#geom_point(data = trace, aes(x = x_val), alpha = 0.2, color = "grey") 



################################################################################
# Author:  EmmaLi Tsai 
# Date:    12/2/2020
# Topic 2: Investigating issues with the time-keeping dots & speed
################################################################################

# Ok-- so let's try to add some sample time keeping dots below the trace and 
# see what happens 
time_dots <- seq(from = red_trace$new_x[1], to = max(red_trace$new_x), by = 1.5)

# adding a y-value 
time_dots <- data.frame(x_val = time_dots, y_val = -1)

# ok cool, but let's move them over a bit to mimic what an actual trace would 
# look like 
time_dots$displace <- runif(nrow(time_dots), min = 0, max = 0.3)
time_dots$disp_x <- round(time_dots$x_val + time_dots$displace, 4)


################################################################################
# add_timepoints_red Function 
#   Author: EmmaLi Tsai 
#   Date: 12/02/2020
#
#   Function to add the random time points at the bottom of the trace, this was 
#   to link the time points with the data frame, similar to what I will actually 
#   be working with
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
    min.index <- which.min(abs(df$new_x - time_dots$disp_x[i]))
    # stepping through the data frame to connect these two 
    for (z in 1:nrow(df)){
      if (z == min.index){
        # if the index row is the minimum index that was identified, 
        # add these two values to our vectors 
        time_points_y[z] = -1
        time_points[z] = time_dots$disp_x[i]
      }
    }
  }
  return(data.frame(time_points = time_points, time_points_y = time_points_y))
}

# Nice, now let's find the speed that the scroll was moving between 
# these two points: 

################################################################################
# scroll_rate Function 
#   Author: EmmaLi Tsai 
#   Date: 12/05/2020
#
#   This function fills in a numeric vector with the distance between 
#   two time points divided by the time the distance would represent. 
#   Using this function, it would be possible to find the speed that the 
#   scroll was moving at different points, which is needed for my arc 
#   function. 
#
#   Input: 
#       -   df          : a dataframe that contains x/y values of a dive trace 
#                         and the x/y values of the time points 
#       -   minutes     : a number that shows the amount of time that passed 
#                         between these two time points
#
#   Output: 
#       -   speed       : a numeric vector with the speed the scroll was moving 
#                         when it made each point
################################################################################

scroll_rate <- function(df, minutes=12){
  # extracting the time points 
  time_points_extract <- df[which(df$time_points_x != 0),]$time_points_x
  # diving the difference of the time points by 12 minutes 
  # to find the speed the roll is moving between these two time points 
  scroll_speed <- diff(time_points_extract) / minutes
  # creating an empty speed numeric vector 
  speed <- rep(0, nrow(df))
  # stepping through the data frame 
  for (i in 1:nrow(df)){
    # stepping through the time points 
    for (v in 1:length(time_points_extract)){
      if (i == 1){
        speed[1] = scroll_speed[1]
      }
      # if the time point x value matches the corresponding 
      # row that we are looking at 
      if (df$time_points_x[i] == time_points_extract[v]){
        # the remaining 0s in the speed vector are overwritten by 
        # the speed the scroll was moving to draw these points 
        speed[(i+1):nrow(df)] = scroll_speed[v]
      }
    }
  }
  return(speed)
}


# adding time points and speeds to the reduced trace data frame: 
red_trace$time_points_x <- add_timepoints_red(red_trace, time_dots)$time_points
red_trace$time_points_y <- add_timepoints_red(red_trace, time_dots)$time_points_y
red_trace$speed <- scroll_rate(red_trace)

# plotting: 
ggplot(red_trace, aes(x = new_x, y = y_val, color = as.factor(speed))) + 
  geom_point() + geom_line(lty = "dashed")+
  theme_bw() + geom_point(aes(x = time_points_x, y = time_points_y), shape = 1) + 
  # to remove all time points at the origin
  xlim(0.00002, 15) + labs(color = "Rate")
# warning message is from the default 0's from the add_timepoints_red
# function:

sum(red_trace$time_points_x == 0) # it is the same as the warning message, but 
# is harmless for the kinds of calculations that I'm doing. 
# maybe transform all 0's to NAs instead to avoid this small issue.


# double check line add time points red function 
# -- should it be the old x or the new x value? 
# Thinking it should be on the new x value, since the scroll is always 
# moving in the +x direction. Using the old x value will produce negative 
# speeds due to the arc. Negative speeds wouldn't make sense 


################################################################################
# Author:  EmmaLi Tsai 
# Date:    12/7/2020
# Topic 3: Transforming x-axis into time  
################################################################################

# CAUTION: THIS SECTION IS STILL A WORK IN PROGRESS! THE APPROACH BELOW PRODUCES 
# OVERLAPPING VALUES SINCE I AM JUST MOVING POINTS IN THE +/- X DIRECTION
# CURRENTLY WORKING ON FIXING THIS SO THE ARC EQUATIONS (see next topic) WORK.

# finding the baseline speed that I want (normal distance between time points 
# divided by the minutes this represents). This is trace specific. 
# Here, 1.5 cm represented 12 minutes 
baseline_speed <- 1.5/12

# some 0 speeds, which won't happen in an actual trace 
red_trace <- red_trace[(which(red_trace$speed!=0)), ]

# just grabbing the time points for the graph below 
time_points_trace <- red_trace[(which(red_trace$time_points_x!=0)), ]

ggplot(red_trace, aes(x = x_val, y = y_val, color = speed)) + geom_point() + 
  geom_point(data = time_points_trace, aes(x = time_points_x, y =  time_points_y))

# taking the difference between the baseline speed and the trace speed to find 
# the error in speed. Taking this value and multiplying it by 12 to get how much 
# to move the x_val over so that the distance between time points is equal. 
# Stored this value as the scaled_x value
red_trace$scaled_x <- red_trace$x_val + ((baseline_speed - red_trace$speed) * 12)

# distance = rate * time; so time = distance / rate;
# adding time.min by taking the x_val and diving by the speed that the scroll 
# was moving to draw that point, which was based on the transformed x value. 
# I used the transformed x value to determine the speed because it wouldn't make 
# sense to use the speed of a preceding time point 

# Now I can use this equation, since the speed is now constant throughou the 
# trace: 
red_trace$time.min <- red_trace$scaled_x / red_trace$speed

# Just for the below graph:
time_points_trace <- red_trace[(which(red_trace$time_points_x!=0)), ]

# plotting: 
ggplot(red_trace, aes(x = time.min, y = y_val, color = speed)) + 
  geom_point() + # geom_point(data = time_points_trace, aes(x = time.min, y =  time_points_y))+
  theme_bw()

# CAUTION!! This process produces overlapping values since you're 
# moving chunks of data in +/- x direction... working on new approach that 
# can be found in the testing_xscale_transformation.xlsx worksheet. The math is 
# all there, but I'm working on putting it in code. 


################################################################################
# Author:  EmmaLi Tsai 
# Date:    12/7/2020
# Topic 4: Testing out the arc equation
################################################################################

# CAUTION -- THIS SECTION IS STILL A WORK IN PROGRESS, SEE ABOVE SECTION ABOUT 
# THE X AXIS 

# ok so now the time.min values depend on the original x_val, and now I can test 
# out my arc equation. The equation will return the amount of actual time it 
# took for the seal to ascend / descend, which will be the actual x_val 
# transformation. 

# just grabbing the start dive and max depth sections, since this is all the 
# information I need for the arc equation
sample_infp <- red_trace[which((red_trace$flag == "S.DC") | red_trace$flag == "MD"),]


# code below just contains sample calculations for the arc equation: 
#####
## some sample calculations before putting it in a function: 
sample_des <- sample_infp[6:7,]

x1 <- which.min(sample_des$x_val)
x2 <- which.max(sample_des$x_val)

# for descent 
(((sample_des$x_val[x2] + sqrt((325.08) - (sample_des$y_val[x2])^2)) - (sample_des$x_val[x1] + 18.03)) / (mean(sample_des$speed)))

# for ascent 
(((sample_des$x_val[x2] + 18.03) - (sample_des$x_val[x1] + sqrt((325.08) - (sample_des$y_val[x1]^2))))/ (mean(sample_des$speed)))
#####

################################################################################
# arc_equations Function 
#   Author: EmmaLi Tsai 
#   Date: 12/08/2020
#
#   This function practices applying the proposed arc equations to the data 
#   frame. It first filters the data set to only include start dives and max
#   depths, and uses the original x and y values of these points and the average
#   speed between them to determine the amount of time that actually passed in 
#   order to draw that section of the dive trace. 
# 
#   Error:    ascent time + descent time != total dive time... error in the 
#             equations? CHECK THIS! 
# 
#   Caution:  you WILL have to code for different kinds of conditions here, the 
#             current code only works on this specific trace, but you will have 
#             to add conditions to make this applicable to other sorts of data 
#
#   Input: 
#       -   df          : a dataframe that contains x/y values of a dive trace 
#                         and the x/y values of the time points 
#
#   Output: 
#       -   time_change : a numeric vector that contains the amount of time that
#                         actually passed for the ascent / descent 
################################################################################

arc_equations <- function(df){
  
  # small numeric vector for me to append my results
  time_change <- numeric()
  # wanting just the start dive and max depth flags 
  df <- df[which((df$flag == "S.DC") | df$flag == "MD"),]
  
  for(i in 1:nrow(df)){
    if (i == (nrow(df) - 1)) {
      # break the loop-- I needed this so I can look at values beyond a
      # specific point
      break()
    }
    # coding for the descent 
    if ((df$flag[i] == "S.DC") && (df$flag[i+1] == "MD")){
      # just grabbing a smaller data frame of this line 
      new.df <- df[i:(i+1),]
      # find the min and max indices -- had to do this to deal with the 
      # backtracking x-values 
      x1 <- which.min(new.df$x_val)
      x2 <- which.max(new.df$x_val)
      # the amount of time it took for the seal to descend
      time_change[i] = (((new.df$x_val[x2] + sqrt((325.08) - (new.df$y_val[x2]^2))) - (new.df$x_val[x1] + 18.03)) / (mean(new.df$speed)))
    } 
    # coding for the ascent
    if (((df$flag[i] == "MD") && (df$flag[i+1] == "S.DC")) | ((df$flag[i] == "MD") && (df$flag[i+1] == "MD"))){
      # grabbing smaller data frame of this line 
      new.df <- df[i:(i+1),]
      # find the min and max indices
      x1 <- which.min(new.df$x_val)
      x2 <- which.max(new.df$x_val)
      # the amount of time it took for the seal to ascend 
      time_change[i] = (((new.df$x_val[x2] + 18.03) - (new.df$x_val[x1] + sqrt((325.08) - (new.df$y_val[x1]^2))))/ (mean(new.df$speed)))
    }
  }
  return(time_change)
}

time_correct <- arc_equations(sample_infp)


################################################################################
# add_time Function 
#   Author: EmmaLi Tsai 
#   Date: 12/08/2020
#
#   This function takes the values from the arc_equation function and adds this 
#   correction to a new numeric vector that holds the corrected times. It 
#   basically takes the first original time value,  and adds the correction. It
#   then uses this previous corrected time value and adds the time correction to 
#   the numeric vector. This process was based off hand calculations and can be 
#   confirmed with the hand calculations I did. 
# 
#
#   Input: 
#       -   df          : a dataframe that contains x/y values of a dive trace 
#                         and the x/y values of the time points 
#       -   time_correct: numeric vector of the corrected ascent/descent times
#                         from the arc_equations function
#
#   Output: 
#       -   running_time: a numeric vector that contains the corrected times for
#                         the trace
################################################################################

add_time <- function(df, time_correct){
  
  # adding running time numeric vector to append the new times 
  running_time <- rep(df$time.min[1], length(time_correct))
  # altering the time_correct function since there is an offset in the indices
  # that I need to step through in order for this approach to work 
  time_correct <- append(0, time_correct, after = 1)
  
  # stepping through each row of the data frame 
  for (i in 1:nrow(df)){
    # next three if statements help with odd cases (end of trace, first and 
    # second rows of the data frame)
    if (i == (nrow(df) + 1)) {
      # break the loop-- I needed this so I can look at values beyond a
      # specific point
      break()
    } 
    # adding special cases for 1 and 2 for this sequence 
    if (i == 1){
      running_time[1] = df$time.min[1] 
    } 
    if (i == 2){
      # this would be a max dept point, and it takes the original time and adds
      # the first correction
      running_time[2] = df$time.min[2] + time_correct[2]
    } 
    if (!(i %in% (1:2))){
      # adding time to where it sums previous running_time value with 
      # corresponding time_correct value 
      running_time[i] = running_time[i-1] + time_correct[i]
    }
  }
  # returning the vector of all corrected times 
  return(running_time)
}

# checking to see if the function works 
add_time(sample_infp, time_correct)

# adding this as a new value to the data frame 
sample_infp$new.time <- add_time(sample_infp, time_correct)

# seems to check out-- did hand calculations in excel and the values match...
# but still won't work until I transform the x axis properly 
ggplot(sample_infp, aes(x=new.time, y =y_val)) + 
  geom_line() +
  geom_point(aes(color = flag), size = 2, alpha = 0.7) + 
  theme_bw() + 
  labs(x = "Time (min)", 
       y = "Y Values (cm)", color = "Type") 
