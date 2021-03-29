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
RADIUS <- 20.6
# height of the KBTDR pivot point when scaled up to the size of the physical
# trace
CENTER_Y <- 11.3
# this was used for the psi to depth calculation, for every 1m increase in 
# depth, there is 1.4696 increase in PSI in saltwater
PSI_TO_DEPTH <- 1.4696

################################################################################
# STEP ONE: Recenter and fix misalignment (both data inputs) ###################
################################################################################

# Code here is absent because this is really more related to image processing 
# methods, but I created code that would fix this step and center the scan in
# the scan_tidying_functions.R file. This function tidys the trace and csv files 
# that were created from the ImageJ defaults using two functions: 
#   tidy_trace(trace) and tidy_timedots(time_dots)
#
# Here, I also centered the scan using the center_scan(trace, time_dots) 
# function. This function did a fuzzy distance full merge using the "fuzzyjoin" 
# package to use the y-values of the time dots to center the scan in the trace 
# file. I am currently working on improving the methods for centering. 

# Functions found to complete this step were tested in step one of the 
# testing_code.R file. 

################################################################################
## STEP TWO AND THREE: Apply radius arm transformation and transform to time ###
################################################################################

# TODO: invalid factor level generated after the mutate(). I am unsure why. 

transform_coordinates <- function(trace, time_dots, time_period_min = 12) {
  # applying my new equation, basically just the equation of a circle but takes
  # the original x/y and calculates where the center of the circle would be
  # (h), and uses this new center to find the x value when depth = 0. I did
  # some algebra to fit this math into one line of code
  trace$new_x <- -sqrt((RADIUS^2) - (CENTER_Y^2)) +
    (trace$x_val + sqrt(RADIUS^2 - (trace$y_val - CENTER_Y)^2))
 
   # ordering the file based on new_x value
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
  trace$time_period <- cut(trace$new_x, breaks = c(tp_df$start_x), include.lowest = TRUE, 
                           labels = tp_df$time_period[1:(nrow(tp_df) - 1)])
  
  # merging the trace file with the time points data frame, which uses time 
  # periods as an ID variable: 
  trace <- merge(trace, tp_df, all = TRUE)
  
  # removing NA's that were created-- this is the last bit of the trace that 
  # continued gathering data after the last time point was assigned, so I 
  # can't actually assign a time for them. 
  trace <- na.omit(trace)
  
  # mutating to create the time scale. This mutate function first calculates 
  # the difference between the new x value and the start x value of the time 
  # period, and then multiplies this by the scale value. I needed this value 
  # to calculate time, which uses this scale value and relates this information 
  # to the time period. 
  trace <- mutate(trace, 
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
# psi calibration at the end of the trace, previous ones do not. 

# This is a work in progress since I need to do a segmented calibration... the 
# function below is not perfect, but I think I'll have to cut() again based on 
# the psi interval

# depth interval values in cm and how they relate to psi: 

# 1.43 = 100psi
# 3.49 = 200psi
# 7.78 = 400psi
# 12.7 = 600psi
# 17.3 = 800psi

# TODO: the function call is way too verbose and too specific. I have defaults 
# in place for the cut function, but I don't like the way this is set up... 
# Is the a way in code to automatically create the breaks and labels in the 
# function call?

# Here I am just trying something out for the depth scale function... it is 
# not perfect. 
transform_psitodepth <- function(trace, 
                              breaks = c("-5","1.43", "3.49", "7.78", "12.7", "17.3", "22"), 
                              labels = c('100:1.43', '200:3.49', '400:7.78', '600:12.7', '800:17.3', '900:22')) {
  
  # creating a psi and interval column together to make the calculations easier
  # the psi is in front of the corresponding y position with  ":" that will 
  # be split later. This was just a way to do a cut with two different labels, 
  # since I need both the psi and the position of the psi for these calculations
  trace$psi_interval_both <- cut(trace$y_val, breaks = breaks,
                                 include.lowest = TRUE, labels = labels)
  
  # splitting the label created by the cut function in to two separate columns 
  # since this made the calculations easier 
  trace <- data.frame(trace, do.call(rbind, strsplit(as.character(trace$psi_interval_both), split = ":", fixed = TRUE)))
  # renaming the extra columns that were created into psi_interval (100, 200, etc)
  # and psi_position (y_value)
  trace <- trace %>% rename("psi_interval" = X1, "psi_position" = X2)
  
  trace$psi_interval <- as.numeric(paste(trace$psi_interval))
  trace$psi_position <- as.numeric(paste(trace$psi_position))
  
  # calculating psi position based on the interval it was categorized into
  trace$psi <- ((trace$psi_interval * trace$y_val) / trace$psi_position)
  
  # calculating depth by taking the psi value and dividing by the constant 
  # I defined above. 
  trace$depth <- trace$psi / PSI_TO_DEPTH
  
  # basic filtering method 
  trace[which(trace$depth < 0),]$depth <- 0
  
  # returning the trace 
  return(trace)
}

# testing code for this function can be found in the testing_code.R file 

################################################################################
## STEP FIVE: Smoothing ########################################################
################################################################################
# currently a work in progress 


################################################################################
## STEP SIX:  Dive statistics, direction flagging, etc##########################
################################################################################

# creating date_time column using the lubridate package, this was needed in 
# order to read this file in as a TDR object in the diveMove package: 

add_dates_times <- function(trace, start_time = "1981:01:16 15:10:00"){
  # adding dates and times from lubridate package 
  trace$date_time <- ymd_hms(start_time, tz = "Antarctica/McMurdo") + 
    minutes(as.integer(trace$time)) + 
    seconds(as.integer((trace$time %% 1) * 60))
  # returning the trace 
  return(trace)
}

# currently working on comparing the dive statistics from this recovered trace 
# with the Castellini et al., 1992 bulletin using the diveMove() package
