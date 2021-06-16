# Sample code for data abstraction: 

# Function below takes the file path that contains all files for a single record 
# and reads in the trace, time dots, and psi_calibration csv files as data
# frames to the global environment. It also renames the columns from defaults 
# in ImageJ from X, Y to x_val and y_val, and corrects default y-axis values to
# tidy the data for future functions. 

# With this function, the user would direct the function to a folder that 
# contains all files for a single record. 
read_trace <- function(filepath = "../sample_data"){
  # listing the files 
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
  trace <<- dplyr::select(trace, c("X", "Y"))
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
  time_dots <<- dplyr::select(time_dots, c("X", "Y"))
  # changing names
  names(time_dots) <<- c("x_val", "y_val")

}

