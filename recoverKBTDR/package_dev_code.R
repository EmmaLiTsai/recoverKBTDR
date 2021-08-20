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
# v updating namespace
devtools::document()
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


# including data for package -- data already tidy
trace <- readr::read_csv("../sample_data/WS_25_1981/tidy_files/tidy_trace.csv")
time_dots <- readr::read_csv("../sample_data/WS_25_1981/tidy_files/tidy_time_dots.csv")
args <- readr::read_csv("../sample_data/WS_25_1981/WS_25_1981_args.csv")

use_data(trace, compress = "xz", overwrite = TRUE)
use_data(time_dots, compress = "xz", overwrite = TRUE)
use_data(args, compress = "xz", overwrite = TRUE)
# ^ these can be loaded into global environment using
# data() feature
data(trace)
class(trace)
trace <- as.data.frame(trace)
time_dots <- as.data.frame(time_dots)

?use_data_raw()

# raw data files before tidying:
system.file("extdata", "WS_25_1981_trace.csv", package = "recoverKBTDR", mustwork = TRUE)
system.file("extdata", "WS_25_1981_time_dots.csv", package = "recoverKBTDR", mustwork = TRUE)

# I think this loads the whole package... and should be used to load functions
# instead of the source() function. BUT be careful running this, it creates an
# instead of the source() function. BUT be careful running this, it creates an
# infinite loop at the moment
devtools::load_all()

# creating a sample vignette
# creating a sample vignette
use_vignette("recoverKBTDR")

