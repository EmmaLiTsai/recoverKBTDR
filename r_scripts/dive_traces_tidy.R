################################################################################
# Authors: EmmaLi Tsai, Dylan W. Schwilk
# Creates scaled time by depth data from a Kooyman-Billups TDR dive trace
################################################################################

# STEPS

# 1. Recenter and fix misalignment (both data inputs)
# 2. transform coordinates by radius arm eqn
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
# file. 

# Functions found to complete this step were tested in step one of the 
# testing_code.R file. 

################################################################################
## STEP TWO AND THREE: Apply radius arm transformation and transform to time ###
################################################################################

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
                      end_x = lead(time_dots$x_val))
  
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
# the depth interval and then scale using those values with a mutate()

# depth scale function: 
transform_todepth <- function(trace, max_psi = 800){
  # calculating psi values: 
  trace$psi <- ((trace$y_val * max_psi) / max(trace$y_val))
  
  # with each 1m increase in depth, there is a 1.4696psi increase in pressure in 
  # saltwater: 
  trace$depth <- trace$psi / (1.4696)
  
  # this is a quick way to reduce noise at depth = 0, this can be done in the 
  # diveMove package... not sure if it is needed here
  trace[which(trace$depth < 0),]$depth <- 0
  
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
trace$date_time <- ymd_hms(START_TIME, tz = "Antarctica/McMurdo") + 
  minutes(as.integer(trace$time)) + 
  seconds(as.integer((trace$time %% 1) * 60))

# currently working on comparing the dive statistics from this recovered trace 
# with the Castellini et al., 1992 bulletin using the diveMove() package
