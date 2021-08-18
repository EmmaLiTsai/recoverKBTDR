# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}


install.packages("usethis")
library(usethis)
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

# including data for package
trace <- readr::read_csv("../sample_data/WS_25_1981/WS_25_1981_trace.csv")
time_dots <- readr::read_csv("../sample_data/WS_25_1981/WS_25_1981_time_dots.csv")
use_data(trace, compress = "xz")
use_data(time_dots, compress = "xz")
