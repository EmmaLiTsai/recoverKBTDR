#' Correct time dots and trace csv files for a record after image processing and read them in to the global environment.
#' @param filepath the path to the folder containing both csv files, time dots and trace, for a single record.
#' @param ... Optional. Columns in the data frame
#' @return Two data frames of the trace and timing dots after correcting ImageJ's default origin placement.
#' @import
#' @importFrom dplyr select
#' @export
#' @examples
#' \dontrun{
#' read_trace("data/")
#' }
#'
read_trace <- function(filepath = "data/"){
  # listing the files
  # package stores these as .rda files?
  trace_list <- list.files(path = filepath, pattern = "*.csv", full.names = TRUE)
  # extracting the names of the files to read them in
  names <- sub('\\.csv$', '', basename(trace_list))
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
  names(trace_files) <- names
  # reading them into the global environment... which seems dangerous...
  # maybe remove below for less specific code, and create a named list instead
  # for future package development:
  invisible(lapply(names(trace_files), function(x) assign(x, trace_files[[x]], envir = .GlobalEnv)))

  ### tidying trace data ##
  # accounting for default origin values in ImageJ
  trace$Y <<- -trace$Y
  # selecting the correct columns and removing unnecessary ones
  # trace <<- dplyr::select(trace, c("X", "Y"))
  trace <<- trace[, which(names(trace) %in% c("X", "Y"))]
  # changing the column names
  names(trace) <<- c("x_val", "y_val")
  # ordering the trace by increasing x value
  trace <<- trace[order(trace$x_val),]
  # removing duplicates
  trace <<- trace[!duplicated(trace),]

  ## tidy time dot data##
  # changing y-values due to odd ImageJ origin placement
  time_dots$Y <<- -time_dots$Y
  # selecting correct columns
  # time_dots <<- dplyr::select(time_dots, c("X", "Y"))
  time_dots <<- trace[, which(names(time_dots) %in% c("X", "Y"))]
  # changing names
  names(time_dots) <<- c("x_val", "y_val")
}

