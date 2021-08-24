# useful package development notes
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

library(usethis)
library(devtools)
library(Rcpp)
library(ggplot2)

# helpful lines of code for setting up the package #############################
# crating readme
use_readme_md()
# pulling roxygen2 to help with documentation
use_roxygen_md()
# making pipe available internally
use_pipe()
# place to put cleaning/data tidying script
# guessing scan centering, file reading?
use_data_raw()
# folders available to users when installed
use_directory("inst")
# adding license -- we already have the GNU one

# creating a sample vignette
use_vignette("recoverKBTDR")

# including data for package -- data are already tidy
trace <- readr::read_csv("../sample_data/WS_25_1981/tidy_files/tidy_trace.csv")
time_dots <- readr::read_csv("../sample_data/WS_25_1981/tidy_files/tidy_time_dots.csv")
args <- readr::read_csv("../sample_data/WS_25_1981/WS_25_1981_args.csv")

use_data(trace, compress = "xz", overwrite = TRUE)
use_data(time_dots, compress = "xz", overwrite = TRUE)
use_data(args, compress = "xz", overwrite = TRUE)

# creating some sample tests for the functions so far: ########################

# v updating namespace
devtools::document()

# I think this loads the whole package... and should be used to load functions
# instead of the source() function.
devtools::load_all()

# data can be loaded into global environment using data() feature. This is tidy
# data.
data(trace)
data(time_dots)

# scan centering
?center_scan
trace <- center_scan(trace, time_dots, 0.9, psi_interval = c(100, 200, 400, 600, 800))

# zero offset correction, if needed:
?zoc
# small drift
zoc(trace, 500, c(-1, 1))
# big drift
zoc(trace, 500, c(-1, 2))

# remove arc and transform x to minutes
?transform_x_vals
trace <- transform_x_vals(trace, time_dots, center_y = 11.18, time_period_min = 12)

# add posixct date/times -- i kept these functions separate because it takes
# longer, but might be able to wrap this in the transform_x_vals function in
# the futture. It just felt like too many arguments to add to one function.
?add_dates_times
trace <- add_dates_times(trace, start_time = "1981:01:16 15:10:00", on_seal = "1981:01:16 17:58:00", off_seal = "1981:01:23 15:30:00")

# transform psi to depth
?transform_y_vals
# if psi calibration curve is present:
trace <- transform_y_vals(trace, psi_calibration = psi_calibration, max_psi = 900, max_position = 22.45)
# if we just know max depth:
trace <- transform_y_vals(trace, maxdep = 319)

# spline smoothing
?smooth_trace_dive
trace <- smooth_trace_dive(trace, spar_h = 0.3, depth_thresh = 5)

# wow... it works
ggplot(trace, aes(x = date_time, y = depth)) +
  geom_point(color = "grey") +
  geom_line(aes(y = smooth_depth), color = "blue")

# example of reading in raw csv files and tidying them:
?read_trace
filepath <- system.file("extdata", "WS_25_1981", package = "recoverKBTDR")
read_trace(filepath)
identical(trace_raw, trace)
identical(time_dots_raw, time_dots)
center_scan(trace_raw, time_dots_raw, 0.9)  # <- this also works
# ^ these should be identical, unsure why they aren't. When I cbind them
# and compare they are identical, but might be because one is a tibble and the
# other is a data frame.

# testing out the fast recovery function, which uses an arguments csv file to
# pass arguments to all functions:
?fast_recovery
filepath <- system.file("extdata", "WS_25_1981", package = "recoverKBTDR")
fast_recovery(filepath)

# helper functions
?find_center_y_psi
?find_center_y_nopsi
?spar_dive_stats
# ^ working on editing these now
