#' Correct time dots and trace csv files.
#' @param filepath the path to the folder containing both csv files, time dots and trace, for a single record.
#' @return Two data frames of the trace and timing dots after correcting ImageJ's default origin placement.
#' @export
#' @examples
#' \dontrun{
#' read_trace("data/folder")
#' }
#'

# Function below takes the file path that contains all files for a single record
# and reads in the trace, time dots, and psi_calibration csv files as data
# frames to the global environment. It also renames the columns from defaults
# in ImageJ from X, Y to x_val and y_val, and corrects default y-axis values to
# tidy the data for future functions.

# With this function, the user would direct the function to a folder that
# contains all files for a single record.

read_trace <- function(filepath = "data/folder"){
  # listing the files
  trace_list <- list.files(path = filepath, pattern = "*.csv", full.names = TRUE)
  # extracting the names of the files to read them in
  names <- sub('\\.csv', '', basename(trace_list))
  # splitting the file to break up the file name string -- this might be changed
  # later in package development depending on how people store and manage their
  # files
  names <- unlist(strsplit(names, "[1-9]_"))
  # picking out different files: trace, time dots, psi calibration, or argument
  # file
  names <- names[grep("^t|p|a", names)]
  # reading in all trace files
  trace_files <- lapply(trace_list, read.csv)
  # giving them appropriate names
  names(trace_files) <- paste(names, "raw", sep = "_")
  # reading them into the global environment... which seems dangerous...
  # maybe remove below for less specific code, and create a named list instead
  # for future package development:
  invisible(lapply(names(trace_files), function(x) assign(x, trace_files[[x]], envir = .GlobalEnv)))
  ### tidying trace data ##
  # accounting for default origin values in ImageJ
  trace_raw[,"Y"] <<- (-trace_raw[,"Y"])
  # selecting the correct columns and removing unnecessary ones
  # trace <<- dplyr::select(trace, c("X", "Y"))
  trace_raw <<- trace_raw[, which(names(trace_raw) %in% c("X", "Y"))]
  # changing the column names
  names(trace_raw) <<- c("x_val", "y_val")
  # ordering the trace by increasing x value
  trace_raw <<- trace_raw[order(trace_raw$x_val),]
  # removing duplicates
  trace_raw <<- trace_raw[!duplicated(trace_raw),]

  ## tidy time dot data##
  # changing y-values due to odd ImageJ origin placement
  time_dots_raw[,"Y"] <<- (-time_dots_raw[,"Y"])
  # selecting correct columns
  # time_dots <<- dplyr::select(time_dots, c("X", "Y"))
  time_dots_raw <<- time_dots_raw[, which(names(time_dots_raw) %in% c("X", "Y"))]
  # changing names
  names(time_dots_raw) <<- c("x_val", "y_val")
}

