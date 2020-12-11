################################################################################
# Author:  EmmaLi Tsai 
# Date:    11/9/2020
################################################################################
# 
# This document compiles the code I've been working on to recover the 1970s - 
# 1980s dive traces. I've been using a sample dive trace that I made and 
# transformed into x/y values using ImageJ. Topics 3 & 4 are still a work in
# progress- code gets a bit sloppy here but I am working on making the code 
# here more efficient.
# 
# This R file is broken up into four topics: 
#
#   Topic 1: Transforming & Decomposing Dive Trace
#            Date: 11/9/2020
#            Line: 27
#   Topic 2: Investigating issues w/ time keeping dots & speed
#            Date: 12/2/2020
#            Line: 259
#   Topic 3: Transforming x-axis into time 
#            Date: 12/11/2020
#            Line: 399
#   Topic 4: Testing out the arc equation
#            Date: 12/8/2020
#            Line: 604
#
################################################################################
# Author:  EmmaLi Tsai 
# Date:    11/9/2020
# Topic 1: Transforming & decomposing dive trace 
################################################################################

# Loading Libraries-- tried to work in base R as much as possible 
library(ggplot2)
library(dplyr)

# Loading in the data
trace <- read.csv("./manual_trace.csv", header = TRUE, 
                  stringsAsFactors = FALSE)

# Moving all x-values over based on y-value to straighten the data 
# THIS IS NOT FULLY TRANSFORMED -- JUST MAKES IT EASIER TO CODE 
trace$new_x <- trace$x_val + trace$y_val*0.5

###############################################################################
#   Function: flag(df)
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
#   Function: filter_trace(df, num_filter = 3)
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
#   Function: add_type(df)
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
#   Function: catch_sd(df)
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
#   Function: add_timepoints_red(df, time_dots)
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
#   Function: scroll_rate(df, minutes=12)
#   Author: EmmaLi Tsai 
#   Date: 12/05/2020
#
#   This function fills in a numeric vector with the distance between 
#   two time points divided by the time the distance would represent. 
#   After working more on transforming the x axis into time, I believe this 
#   function is now obsolete. The graph that it produces is a nice visual 
#   though for future presentations. 
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
# I don't actually need this column, but it produces a neat graph! 
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
# maybe transform all 0's to NAs instead to avoid this small issue in the future 


# double check line add time points red function 
# -- should it be the old x or the new x value? 
# Thinking it should be on the new x value, since the scroll is always 
# moving in the +x direction. Using the old x value will produce negative 
# speeds due to the arc. Negative speeds wouldn't make sense 


################################################################################
# Author:  EmmaLi Tsai 
# Date:    12/11/2020
# Topic 3: Transforming x-axis into time  
################################################################################

# CODE FOR TOPICS 3 & 4 ARE STILL A WORK IN PROGRESS - code is still a bit 
# sloppy and I am working on tidying up these two sections to make it as 
# efficient as possible-  ET (12/11)

# grabbing all time points to bind with the output of the below function 
red_trace_tp <- red_trace[which(red_trace$time_points_x!=0),]


################################################################################
#   Function: new_timepoints(df, distance = 1.5, minutes = 12)
#   Author: EmmaLi Tsai 
#   Date: 12/10/2020
#
#   This function transforms all time points so that they are now the distance 
#   that the user defines. This also produces a scaling factor, which will be 
#   used to transform all x values into time using the x_prime function below. 
#   Output from this function is also binded to the original time points so that 
#   the x_prime function can match specific rows. The code for these two
#   functions are pretty inefficient and I am working on cleaning these up. 
#   Eventually, I believe that new_timepoints and x_prime could later become a 
#   single function. 
#
#   Input: 
#       -   df          : a dataframe that contains x/y values of a dive trace 
#                         and x/y values of the time points at the bottom of 
#                         the trace. 
#
#       -   distance    : a number indicating the standard distance between time 
#                         points. Default is 1.5cm. 
# 
#       -   minutes     : the minutes the standard distance represents. Default 
#                         is 12 minutes 
#
#   Output: 
#       -   df          : data frame contains the new.tp (all should be EXACTLY  
#                         the distance apart that the user defines), x.prime 
#                         (x values for this time point when transformed-- this 
#                         was to make sure my equation was working), and 
#                         scale_factor (amount the x prime values were altered 
#                         when scaling to the new distance between time points)
# 
################################################################################

new_timepoints <- function(df, distance = 1.5, minutes = 12){
  # just grabbing the time points 
  tp <- df[which(df$time_points_x!=0),]
  # making empty numeric vector for new time points 
  new_timepoints <- numeric() 
  # storing the scaling factors 
  scale_factor <- numeric()
  # this was to store the new time point x values-- just to make sure that 
  # they are all 1.5 cm apart 
  x_prime <- numeric()
  for (i in 1:nrow(tp)){
    if (i == (nrow(df) - 1)) {
      # break the loop-- I needed this so I can look at values beyond a
      # specific point
      break()
    }
    # special case for the first time point 
    if (i == 1){
      # grabbing the first and second time point 
      tp1 <- tp$time_points_x[1]
      tp2 <- tp$time_points_x[2]
      # finding the speed between them 
      r <- (tp2 - tp1) / minutes 
      # calculating the new time point 2 value 
      new_tp2 <- tp2 + ((distance / minutes) - r) * minutes
      # finding the scale factor 
      scale_factor[1:2] <- (tp2 - tp1) / (new_tp2 - tp1)
      # finding the new x values 
      x_prime[2] <- ((tp$x_val[2] - tp1) / scale_factor[1]) + tp1
      # adding new time point values to the numeric vector 
      new_timepoints[1] = tp1
      new_timepoints[2] = new_tp2
      # first time point doesn't change
      x_prime[1] <- tp$x_val[1]
    }
    else{
      # finding new tp1 (based on time point value from before), 
      # and the original tp2 
      tp1 <- new_timepoints[i]
      tp2 <- tp$time_points_x[i+1]
      # finding the new rate between points 
      r <- (tp2 - tp1) / minutes 
      # finding new time point 
      new_tp2 <- tp2 + ((distance / minutes) - r) * minutes
      # finding new scale factor 
      scale_factor[i+1] <- (tp2 - tp1) / (new_tp2 - tp1)
      # new x values 
      x_prime[i+1] <- ((tp$x_val[i+1] - tp1) / scale_factor[i+1]) + tp1
      # transferring new time points to new vector 
      new_timepoints[i+1] = new_tp2
    }
  }
  return(data.frame(new.tp = new_timepoints, x.prime = x_prime, scale_factor = scale_factor))
}

# calling the new time points function 
new_tp <- new_timepoints(red_trace)
# cutting of the extra NA at the end 
new_tp <- new_tp[1:nrow(new_tp)-1,]
# binding the new time points data frame with the time points data frame since 
# I plan to basically use time_points_x as a matching reference for the 
# scale transformation function (x_prime)
new_tp <- cbind(new_tp, red_trace_tp$time_points_x)


################################################################################
#   Function: x_prime(df, new_tp)
#   Author: EmmaLi Tsai 
#   Date: 12/10/2020
#
#   This function takes the scaling factor from the new_timepoints function and 
#   uses this scaling factor to find the new x_values when transformed to time. 
#   Details in the math behind this approach can be found in the 
#   testing_xscale_transformation.xlsx spreadsheet. It essentially takes the 
#   original x value, subtracts from the new time point, divides this by the 
#   scaling factor, and adds this value to the new time point. The code here is 
#   a bit tricky, and I plan to tidy it up in the future. I have double checked 
#   the values produced in this equation in the excel spreadsheet, to make sure 
#   that I was transforming everything correctly. 
# 
#   I am a bit unhappy with how this code is written and I'm working on making 
#   it better- ET (12/11)
#
#   Input: 
#       -   df          : a dataframe that contains x/y values of a dive trace 
#                         and x/y values of the time points at the bottom of 
#                         the trace. 
#
#       -   new_tp      : a smaller data frame that contains the output of the 
#                         new_timepoints() function when binded with the 
#                         original x values of the time points
#
#   Output: 
#       -   x_prime     : numeric vector that contains all transformed x_values. 
#                         When added as a new column to the original data frame, 
#                         the data frame should now be on set rate throughout 
#                         the trace, which would allow me to apply the equation
#                         distance / rate = time to find the time that each 
#                         time point represents. 
# 
################################################################################

x_prime <- function(df, new_tp){
  # creating empty numeric vector to append the transformed x_values 
  x_prime <- numeric(length = nrow(df))
  # stepping through each row of the new time points 
  for (v in 1:nrow(new_tp)){
    # stepping through each row of the data frame 
    for (i in 1:nrow(df)){
      if (i == (nrow(df) - 1)) {
        # break the loop-- I needed this so I can look at values beyond a
        # specific point
        break()
      }
      if (v == 1){
        # coding for special cases for v = 1 and v = 2, due to an offset from the 
        # new_timepoints function 
        x_prime[i:nrow(df)] <- ((df$x_val[i:nrow(df)]- new_tp$new.tp[1]) / new_tp$scale_factor[1]) + new_tp$new.tp[1]
      } 
      if (v == 2){ 
        # coding for special case v = 2
        x_prime[i:nrow(df)] <- ((df$x_val[i:nrow(df)]- new_tp$new.tp[1]) / new_tp$scale_factor[2]) + new_tp$new.tp[1]
      }
      # coding for special cases in v == 1 and v == 2 where the time points match
      # the time points from the new_timepoints function 
      if((df$time_points_x[i] == new_tp$`red_trace_tp$time_points_x`[v-1]) && (!(v %in% (1:2)))){
        # applying equation here 
        x_prime[i:(nrow(df))] <- ((df$x_val[i:(nrow(df))]- new_tp$new.tp[v-1]) / new_tp$scale_factor[v]) + new_tp$new.tp[v-1]
      } 
      if((df$time_points_x[i] == new_tp$`red_trace_tp$time_points_x`[v-1]) && (v >= 3)){
        # coding for another special case where the time points match but v >=3
        # applying the equation here
        x_prime[i] <- ((df$x_val[i] - new_tp$new.tp[v-2])) / new_tp$scale_factor[v-1] + new_tp$new.tp[v-2]
      }
    }
  }
  # returning all new x values 
  return(x_prime)
}

# calling the x_prime function to see that it works 
x_prime(red_trace, new_tp)

# adding a new column to red_trace with all transformed x_values 
red_trace$transformed_x <- x_prime(red_trace, new_tp)

# adding time by taking the distance / rate; since now the rate is constant 
# throughout the trace!
red_trace$new.time <- red_trace$transformed_x / (1.5/12)

# plotting to make sure nothing weird happened 
ggplot(red_trace, aes(x = new.time, y = y_val, color = flag)) + geom_point()

# I do see some negative times, but this would never happen in an actual trace. 
# I think this is just from the way I constructed this sample data set 

################################################################################
# Author:  EmmaLi Tsai 
# Date:    12/7/2020
# Topic 4: Testing out the arc equation
################################################################################

# ok so now the new.time values depend on the original x_val, and now I can test 
# out my arc equation. The equation will return the amount of actual time it 
# took for the seal to ascend / descend, which will be the actual x_val 
# transformation. 

# just grabbing the start dive and max depth sections, since this is all the 
# information I need for the arc equation. Infp stands for inflection points. 
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
#   Function: arc_equations (df, distance = 1.5, minutes = 12)  
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
#   CAUTION:  you WILL have to code for different kinds of conditions here, the 
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

arc_equations <- function(df, distance = 1.5, minutes = 12){
  
  # small numeric vector for me to append my results
  time_change <- numeric()
  # wanting just the start dive and max depth flags 
  df <- df[which((df$flag == "S.DC") | df$flag == "MD"),]
  
  rate <- distance / minutes 
  
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
      # 325.08 and 18.03 are constants for all taces-- they are the dimensions  
      # of the pressure-sensitive arm on the instrument, so I didn't store them 
      # as a variable 
      time_change[i] = (((new.df$x_val[x2] + sqrt((325.08) - (new.df$y_val[x2]^2))) - (new.df$x_val[x1] + 18.03)) / (rate))
    } 
    # coding for the ascent
    if (((df$flag[i] == "MD") && (df$flag[i+1] == "S.DC")) | ((df$flag[i] == "MD") && (df$flag[i+1] == "MD"))){
      # grabbing smaller data frame of this line 
      new.df <- df[i:(i+1),]
      # find the min and max indices
      x1 <- which.min(new.df$x_val)
      x2 <- which.max(new.df$x_val)
      # the amount of time it took for the seal to ascend 
      time_change[i] = (((new.df$x_val[x2] + 18.03) - (new.df$x_val[x1] + sqrt((325.08) - (new.df$y_val[x1]^2))))/ (rate))
    }
  }
  return(time_change)
}

time_correct <- arc_equations(sample_infp)


################################################################################
#   Function: add_time(df, time_correct) 
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
  running_time <- rep(df$new.time[1], length(time_correct))
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
      running_time[1] = df$new.time[1] 
    } 
    if (i == 2){
      # this would be a max dept point, and it takes the original time and adds
      # the first correction
      running_time[2] = df$new.time[2] + time_correct[2]
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
sample_infp$arc_time <- add_time(sample_infp, time_correct)

# seems to check out-- did hand calculations in excel and the values match...
# but still won't work until I transform the x axis properly 
ggplot(sample_infp, aes(x=arc_time, y =y_val)) + 
  geom_line() +
  geom_point(aes(color = flag), size = 2, alpha = 0.7) + 
  theme_bw() + 
  labs(x = "Time (min)", 
       y = "Y Values (cm)", color = "Type") 


