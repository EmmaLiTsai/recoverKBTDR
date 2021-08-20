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

# v updating namespace
devtools::document()

# creating some sample tests for the functions so far: ########################

# I think this loads the whole package... and should be used to load functions
# instead of the source() function.
devtools::load_all()

# data can be loaded into global environment using
# data() feature. This is tidy data.
data(trace)
data(time_dots)
?center_scan
center_scan(trace, time_dots, 0.9)
# wow... it works

# raw data files before tidying:
?read_trace
filepath <- system.file("extdata", "WS_25_1981", package = "recoverKBTDR")
read_trace(filepath)
identical(trace_raw, trace)
identical(time_dots_raw, time_dots)
# ^ these should be identical, unsure why they aren't. When I cbind them
# and compare they are identical, but might be because one is a tibble and the
# other is a data frame.

# zero offset correction:
?zoc()
# small drift
zoc(trace, 500, c(-1, 1))
# big drift
zoc(trace, 500, c(-1, 2))
