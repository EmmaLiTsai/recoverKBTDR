#' Correct raw trace csv file from ImageJ
#'
#' This function takes the raw trace csv file from ImageJ and applies a series
#' of corrections to it such that it can be used in this package. This
#' involves correcting the default y-axis values, ordering, filtering, and
#' adding proper names to the data frame.
#'
#' @param filepath_trace the path to the csv file containing the raw trace data
#' from ImageJ
#' @param col_x column of the x values, default is set to first column.
#' @param col_y column of the y values, default is set to second column.
#' @return One trace data frame after correcting ImageJ's default origin
#' placement
#' @export
#' @examples
#' \dontrun{
#' filepath <- system.file("extdata", "WS_25_1981", package = "recoverKBTDR")
#' filepath_trace <- paste(filepath, "WS_25_1981_trace.csv", sep = "/")
#' tidy_raw_trace(filepath_trace, col_x = 1, col_y = 2)
#' }
#'
tidy_raw_trace <- function(filepath_trace, col_x = 1, col_y = 2){
  # gotta make this more general
  # reading in
  raw_trace <- read.csv(filepath_trace)
  # creating more flexibility based on data structure
  raw_trace <- raw_trace[,c(col_x, col_y)]
  names(raw_trace) <- c("X", "Y")
  # correcting default y-axis placement
  raw_trace[,"Y"] <- (-raw_trace[,"Y"])
  # grabbing the important columns
  raw_trace <- raw_trace[, which(names(raw_trace) %in% c("X", "Y"))]
  # changing the column names
  names(raw_trace) <- c("x_val", "y_val")
  # ordering the trace by increasing x value
  raw_trace <- raw_trace[order(raw_trace$x_val),]
  # removing duplicates
  trace_tidy <- raw_trace[!duplicated(raw_trace),]
  # return
  return(trace_tidy)
}

#' Correct raw time dots csv file from ImageJ
#'
#' This function takes the raw time dots csv file from ImageJ and applies a
#' series of corrections to it such that it can be used in this package. This
#' involves correcting the default y-axis values, ordering, filtering, and
#' adding proper names to the data frame.
#'
#' @param filepath_timedots the path to the csv file containing the raw time dots
#' data from ImageJ
#' @param col_x column number of the x values, default is set to first column.
#' @param col_y column number of the y values, default is set to second column.
#' @return One trace data frame after correcting ImageJ's default origin
#' placement
#' @export
#' @examples
#' \dontrun{
#' filepath <- system.file("extdata", "WS_25_1981", package = "recoverKBTDR")
#' filepath_timedots <- paste(filepath, "WS_25_1981_time_dots.csv", sep = "/")
#' tidy_raw_timedots(filepath_timedots, col_x = 1, col_y = 2)
#' }
#'
tidy_raw_timedots <- function(filepath_timedots, col_x = 1, col_y = 2){
  # reading in
  raw_timedots <- read.csv(filepath_timedots)
  # creating more flexibility based on data structure
  raw_timedots <- raw_timedots[,c(col_x, col_y)]
  names(raw_timedots) <- c("X", "Y")
  # changing y-values due to odd ImageJ origin placement
  raw_timedots[,"Y"] <- (-raw_timedots[,"Y"])
  # selecting correct columns
  time_dots_tidy <- raw_timedots[, which(names(raw_timedots) %in% c("X", "Y"))]
  # changing names
  names(time_dots_tidy) <- c("x_val", "y_val")
  # return
  return(time_dots_tidy)
}
