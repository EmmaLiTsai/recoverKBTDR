# Sample code for data abstraction: 

# Function below takes the file path that contains all files for a single record 
# and reads in the trace, time dots, and psi_calibration csv files as data
# frames to the global environment. 

# With this function, the user would direct the function to a folder that 
# contains all files for a single record. 

read_traces <- function(filepath = "../sample_data"){
  # listing the files 
  trace_list <- list.files(path=filepath, pattern="*.csv", full.names=TRUE)
  # extracting the names of the files to read them in 
  names <- sub('\\.csv$', '', basename(trace_list))
  # removing the beginning of the file name so it works better for future 
  # functions
  names <- substring(names, 12)
  # reading in all trace files 
  trace_files <- lapply(trace_list, read.csv)
  # giving them appropriate names
  names(trace_files) <- names
  # reading them into the global environment... which seems dangerous...
  invisible(lapply(names(trace_files), function(x) assign(x, trace_files[[x]], envir=.GlobalEnv)))
}

