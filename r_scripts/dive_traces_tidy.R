################################################################################
# Authors: EmmaLi Tsai, Dylan W. Schwilk
# Creates scaled time by depth data from a Kooyman-Billups TDR dive trace
################################################################################

# STEPS

# 1. Recenter and fix misalignment (both data inputs)
# 2. Transform coordinates by radius arm eqn
# 3. Transform x axis according to time dots
# 4. Transform y axis to depth 
# 5. Smoothing
# 6. Dive statistics, direction flagging, etc.

# For all functions below, testing code can be found in the testing_code.R 
# file. 

###############################################################################
# Global constants: 

# radius of the KBTDR arm when scaled up to the size of the physical traces
RADIUS <- 20.87
# height of the KBTDR pivot point when scaled up to the size of the physical
# trace
# TODO: this is not constant across all traces! See Issue #13 in GitHub
CENTER_Y <- 11.1
# this was used for the psi to depth calculation, for every 1m increase in 
# depth, there is 1.4696 increase in PSI in saltwater
PSI_TO_DEPTH <- 1.4696

################################################################################
# STEP ONE: Recenter and fix misalignment (both data inputs) ###################
################################################################################

# Code here is absent because this is really more related to image processing 
# methods, but I created code that would fix this step and center the scan in
# the scan_tidying_functions.R file. This function tidies the trace and csv 
# files that were created from the ImageJ defaults using two functions: 
# tidy_trace(trace) and tidy_timedots(time_dots)
#
# There, I also centered the scan using the center_scan(trace, time_dots) 
# function. This function did a fuzzy distance full merge using the "fuzzyjoin" 
# package to use the y-values of the time dots to center the scan in the trace 
# file. 

# Functions to complete this step were tested in step one of the 
# testing_code.R file. 

################################################################################
## STEP TWO AND THREE: Apply radius arm transformation and transform to time ###
################################################################################

###############################################################################
# Function: transform_coordinates(trace, time_dots, time_period_min = 12)
# Author:   EmmaLi Tsai
# Date:     3/30/21
# 
# Function takes the tidy trace and time dots files to complete two steps: 
# 
#   (2) - apply radius arm transformation using the geometry of the KBTDR device 
#         which uses the globally defined constants above.
#
#   (3) - transform the x axis from time using the timing dots. To do this, I 
#         created a helper data frame with the start and end points of a 
#         time period and the corresponding scale value. This data frame is  
#         needed to help make the cut() function easier, so I can break 
#         the trace up into sections that would identify which time period a 
#         specific x value belongs to along a trace. Then, I do a merge() with 
#         the trace data frame. Using this merged data frame, I do a mutate() 
#         for some simple calculations that will use the distance a point is 
#         from the start point of a time period and the scale value I made 
#         earlier to estimate the time value of a specific point. 
# 
# Input: 
#   - time_dots   : tidy time_dots data frame, contains the x and y values of  
#                   the time dots for a trace 
# 
#   - trace       : tidy trace data frame, contains the x and y values of the 
#                   trace
#
#   - time_period_min : minutes between each time period. This is 12 minutes for 
#                   most traces. 
#   
# Output: 
#   - trace      : trace data frame complete with time periods, and time 
#                  of an x value in minutes from when the device started 
#                  gathering data. I kept all columns to ensure that the 
#                  function was working properly. 
###############################################################################
transform_coordinates <- function(trace, time_dots, time_period_min = 12) {
 ## Start Step Two: Transform Coordinates by Radius Arc Eqns ################# 
  
  # applying my new equation, basically just the equation of a circle but takes
  # the original x/y and calculates where the center of the circle would be
  # (h), and uses this new center to find the x value when depth = 0. I did
  # some algebra to fit this math into one line of code
  
  # TODO: CENTER_Y is not constant across traces, see issue 13 in GitHub. Will 
  # likely need extra calculations from r_scripts/find_center_y.R file. 
  trace$new_x <- -sqrt((RADIUS^2) - (CENTER_Y^2)) +
    (trace$x_val + sqrt(RADIUS^2 - (trace$y_val - CENTER_Y)^2))
 
  # ordering the file based on new_x value-- this is needed to create accurate 
  # time periods in step three below
  trace <- trace[order(trace$new_x),]

  ## Starting Step Three: scale X based on time dots ###########################
  
  # creating a data frame with time periods and start/end points for the time 
  # period... this will be used to cut the data 
  tp_df <- data.frame(time_period = seq(1:nrow(time_dots)),
                      start_x = time_dots$x_val, 
                      end_x = lead(time_dots$x_val), 
                      stringsAsFactors = FALSE)
  
  # adding the scale value 
  tp_df$scale = time_period_min / (tp_df$end_x - tp_df$start_x)
  
  # adding this as a time period variable to the trace using the cut() function
  trace$time_period <- cut(trace$new_x, 
                           breaks = tp_df$start_x, 
                           include.lowest = TRUE, 
                           labels = tp_df$time_period[1:(nrow(tp_df) - 1)])
  
  # merging the trace file with the time points data frame, which uses time 
  # periods as an ID variable: 
  trace <- merge(trace, tp_df, by = 'time_period', all.x = TRUE)

  # mutating to create the time scale. This mutate function first calculates 
  # the difference between the new x value and the start x value of the time 
  # period, and then multiplies this by the scale value. I needed this value 
  # to calculate time, which uses this scale value and relates this information 
  # to the time period. 
  trace <- dplyr::mutate(trace, 
                  diff = new_x - start_x, 
                  diff_with_scale = diff * scale, 
                  time = diff_with_scale + (as.numeric(time_period)-1) * time_period_min)
 
  # returning final trace 
   return(trace)
}

################################################################################
## STEP FOUR: Transform Y Axis to Depth ########################################
################################################################################

# THIS APPROACH WILL BE DIFFERENT FOR ALL TRACES BEFORE 1981 ! 1981 traces have 
# psi calibration at the end of the trace, previous ones do not. I have two 
# functions here to handle both. 

###############################################################################
# Function: transform_psitodepth(trace, psi_calibration)
# Author:   EmmaLi Tsai
# Date:     4/09/21
# 
# This function takes the tidy trace file (containing x and y values of the 
# trace) and a csv file (psi_calibration) containing the intervals and positions 
# of the psi calibration curve at the end of the trace to determine the psi 
# values of the trace. Essentially, this function creates breaks and labels out 
# of the psi_calibration csv file to cut() the trace into different categories 
# based on the y_val of a point. From these categories and the labels defined, 
# this function then uses proportions to calculate the psi value of a specific 
# point. These psi values can then be transformed to depth (in meters) using a 
# simple calculation.  
# 
# Input: 
# 
#   - trace       : tidy trace data frame, contains the x and y values of the 
#                   trace.
#
#   - psi_calibration : csv file that contains two columns for the cut(): 
#                           - psi_interval: the psi and y_val concatenated 
#                                           together (i.e., 100:1.43 for the 
#                                           100psi calibration that is 1.43 cm 
#                                           above y = 0). 
#                           - psi_position: the y_val to cut by. This should 
#                                           column should be one row longer than 
#                                           the psi_interval column, and start 
#                                           with a lower y_val to encompass 
#                                           points that may fall below y = 0. 
#                                           (i.e., start with y_val -5 to 
#                                           capture points with y_val = -0.01 so 
#                                           it can fall in the 100:1.43 
#                                           category)                                    
#   
# Output: 
#   - trace      : trace data frame complete with the psi_interval and 
#                  position a point fell into, the calculated psi value, and 
#                  the calculated depth. I kept all columns to ensure the 
#                  function was working properly. 
###############################################################################
transform_psitodepth <- function(trace, psi_calibration) {
  # defining the breaks 
  breaks <- psi_calibration$psi_position
  
  # defining the labels-- had to select a specific number of observations since 
  # the last observation is a "" since there is an unequal number of 
  # psi_interval and psi_position rows. I needed to specify an unequal number 
  # of rows since these columns will be used for the cut() function. 
  labels <- psi_calibration$psi_interval[1:6]
  
  # creating a psi and interval column together to make the calculations easier
  # the psi is in front of the corresponding y position with  ":" that will 
  # be split later. This was just a way to do a cut with two different labels, 
  # since I need both the psi and the position of the psi for these calculations
  trace$psi_interval_both <- cut(trace$y_val, breaks = breaks,
                                 include.lowest = TRUE, labels = labels)

  # splitting the label created by the cut function in to two separate columns 
  # since this made the calculations easier 
  trace <- tidyr::separate(trace, psi_interval_both, 
                           sep = ":", 
                           into = c("psi_interval", "psi_position"))

  # I needed numeric values to do calculations: 
  # is there a way to do this more efficiently? 
  trace$psi_interval <- as.numeric(paste(trace$psi_interval))
  trace$psi_position <- as.numeric(paste(trace$psi_position))
  
  # calculating psi position based on the interval it was categorized into
  trace$psi <- ((trace$psi_interval * trace$y_val) / trace$psi_position)
  
  # calculating depth by taking the psi value and dividing by the constant 
  # I defined above. 
  trace$depth <- trace$psi / PSI_TO_DEPTH

  # returning the trace 
  return(trace)
}

# testing code for this function can be found in the testing_code.R file 

# This is a simple function for the 1978 and 1979 traces without a psi 
# calibration curve at the end. I don't have the calibration records for these 
# traces, so I'll have to calibrate depth using the max depths from the 
# Castellini et al., 1992 bulletin. 
transform_todepth <- function(trace, max_depth){
  # calculating depth using the max depth the user defines and the max 
  # value of the trace: 
  trace$depth <- ((trace$y_val * max_depth) / max(trace$y_val))
  # returning trace 
  return(trace)
}

################################################################################
## STEP FIVE: Smoothing ########################################################
################################################################################
# currently a work in progress -- see options in testing_code.R


################################################################################
## STEP SIX:  Dive statistics, direction flagging, etc##########################
################################################################################
# creating date_time column using the lubridate package, this was needed in 
# order to read this file in as a TDR object in the diveMove package: 

# this could be tagged on to step 2 of this file? 
add_dates_times <- function(trace, start_time = "1981:01:16 15:10:00"){
  # adding dates and times from lubridate package 
  trace$date_time <- lubridate::ymd_hms(start_time, tz = "Antarctica/McMurdo") + 
    minutes(as.integer(trace$time)) + 
    seconds(as.integer((trace$time %% 1) * 60))
  # returning the trace 
  return(trace)
}

# currently working on comparing the dive statistics from this recovered trace 
# with the Castellini et al., 1992 bulletin using the diveMove() package

