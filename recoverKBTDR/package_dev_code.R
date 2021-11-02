# useful package development notes
#
# Some keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

library(usethis)
library(devtools)
library(Rcpp)
library(ggplot2)
################################################################################
# helpful lines of code for setting up the package #############################
################################################################################

# crating readme
# use_readme_md()
# pulling roxygen2 to help with documentation
# use_roxygen_md()
# making pipe available internally
# use_pipe()
# place to put cleaning/data tidying script
# guessing scan centering, file reading?
# use_data_raw()
# folders available to users when installed
# use_directory("inst")
# adding license -- we already have the GNU one

# creating a sample vignette
# use_vignette("recoverKBTDR")

# including data for package -- data are already tidy
# trace <- readr::read_csv("../sample_data/WS_25_1981/tidy_files/tidy_trace.csv")
# time_dots <- readr::read_csv("../sample_data/WS_25_1981/tidy_files/tidy_time_dots.csv")
# args <- readr::read_csv("../sample_data/WS_25_1981/WS_25_1981_args.csv")
#
# use_data(trace, compress = "xz", overwrite = TRUE)
# use_data(time_dots, compress = "xz", overwrite = TRUE)
# use_data(args, compress = "xz", overwrite = TRUE)
#
# adding documentation
# use_r("trace")
# use_r("time_dots")
# use_r("args")
################################################################################
# creating some sample tests for the functions so far: ########################
################################################################################

# v updating namespace
devtools::document()

# I think this loads the whole package... and should be used to load functions
# instead of the source() function.
devtools::load_all()

# data can be loaded into global environment using data() feature. This is tidy
# data (i.e., after cleaning the raw ImageJ csv files), but I have other
# functions below to illustrate the data cleaning process.
data(trace)
data(time_dots)

# scan centering -- will also produced centered psi calibration curve
?center_scan
centered_trace <- center_scan(trace, time_dots, center_along_y = 0.9)
# extract the psi calibration curve:
psi_calibration <- centered_psi_calibration(centered_trace, psi_interval = c(100, 200, 400, 600, 800))

# zero offset correction, if needed:
?zoc
# small drift
zoc(trace, 500, c(-1, 1))
# big drift
zoc(trace, 500, c(-1, 2))

# remove arc and transform x to minutes
?transform_x_vals
trace <- transform_x_vals(trace, time_dots, center_y = 11.18,
                          time_period_min = 12)

# add posixct date/times -- I kept these functions separate because it takes
# longer to run, but might be able to wrap this in the transform_x_vals function
# in future commits. It also just felt like too many arguments to add to one
# function, which seems like a common problem that I'm having.
?add_dates_times
trace <- add_dates_times(trace,
                         start_time = "1981:01:16 15:10:00",
                         on_seal = "1981:01:16 17:58:00",
                         off_seal = "1981:01:23 15:30:00")

# transform psi to depth
?transform_y_vals
# if psi calibration curve is present:
trace <- transform_y_vals(trace, psi_calibration = psi_calibration,
                          max_psi = 900, max_position = 22.45)
# if we just know max depth:
trace <- transform_y_vals(trace, maxdep = 319)

# spline smoothing
?smooth_trace_dive
trace <- smooth_trace_dive(trace, spar_h = 0.22, depth_thresh = 5)

# wow... it works!
ggplot(trace[500:20000,], aes(x = date_time, y = depth)) +
  geom_point(color = "grey") +
  geom_line(aes(y = smooth_depth), color = "blue")

################################################################################
# example and helper functions
################################################################################
# example of reading in raw csv files and tidying them:
filepath <- system.file("extdata", "WS_25_1981", package = "recoverKBTDR")
filepath_trace <- paste(filepath, "WS_25_1981_trace.csv", sep = "/")
filepath_timedots <- paste(filepath, "WS_25_1981_time_dots.csv", sep = "/")

tidy_raw_trace(filepath_trace)
tidy_raw_timedots(filepath_timedots)

# testing out the fast recovery function, which uses an arguments csv file
# (args) to pass arguments to all functions:
?fast_recovery
filepath_args <- paste(filepath, "WS_25_1981_args.csv", sep = "/")

recovered <- fast_recovery(filepath_trace, filepath_timedots, filepath_args)
# helper functions that help find the best arguments pass to the
# transform_x_vals() function (center_y), and smooth_trace_dive() function
# (spar_h).

# with psi calibration curve:
?find_center_y
find_center_y(beg_dive = c(1142.945, 0),
              depth_dive = c(1140.55, 9.3),
              rate = 0.16, psi_calibration)
# if only max depth known:
find_center_y(beg_dive = c(65.258, 0),
              depth_dive = c(63.442, 5.341),
              rate = 0.21, max_depth = 484, df = trace)

# v this one will take a longer time to run, but I believe this is the best
# method for objectively finding the right spar value.
?find_best_spar
find_best_spar(filepath_trace, filepath_timedots, filepath_args)
# ^ will return best spar value of 0.22

# if you want the dive stats for all spar iterations to see the raw data behind
# the best spar value that was found:
get_divestats("../results/")

################################################################################
# package checking!
################################################################################
# updating namespace and loading package:
devtools::document()
devtools::load_all()

# this checks and builds the package!
?check()
check("../recoverKBTDR")
# NOTES FROM PACKAGE CHECK -- no errors, but 1 warning and 1 note
# - no visible binding for ... global environment issue?
# - what is qpdf??? why do I need it????

# this will send the post package to CRAN:
# ?release()
