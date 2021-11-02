#' Correct raw trace csv file from ImageJ
#'
#' This function takes the raw trace csv file from ImageJ and applies a series
#' of corrections to it such that it can be used in this package. This
#' involves correcting the default y-axis values, ordering, filtering, and
#' adding proper names to the data frame.
#'
#' @param filepath_trace the path to the csv file containing the raw trace data
#' from ImageJ
#' @return One trace data frame after correcting ImageJ's default origin
#' placement
#' @export
#' @examples
#' \dontrun{
#' filepath <- system.file("extdata", "WS_25_1981", package = "recoverKBTDR")
#' filepath_trace <- paste(filepath, "WS_25_1981_trace.csv", sep = "/")
#' tidy_raw_trace(filepath_trace)
#' }
#'
tidy_raw_trace <- function(filepath_trace){
  # reading in
  raw_trace <- read.csv(filepath_trace)
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
#' @return One trace data frame after correcting ImageJ's default origin
#' placement
#' @export
#' @examples
#' \dontrun{
#' filepath <- system.file("extdata", "WS_25_1981", package = "recoverKBTDR")
#' filepath_timedots <- paste(filepath, "WS_25_1981_time_dots.csv", sep = "/")
#' tidy_raw_timedots(filepath_timedots)
#' }
#'
tidy_raw_timedots <- function(filepath_timedots){
  # reading in
  time_dots_tidy <- read.csv(filepath_timedots)
  # changing y-values due to odd ImageJ origin placement
  time_dots_tidy[,"Y"] <- (-time_dots_tidy[,"Y"])
  # selecting correct columns
  time_dots_tidy <- time_dots_tidy[, which(names(time_dots_tidy) %in% c("X", "Y"))]
  # changing names
  names(time_dots_tidy) <- c("x_val", "y_val")
  # return
  return(time_dots_tidy)
}
