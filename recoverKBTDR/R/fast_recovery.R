#' Fast recovery of record using argument file
#'
#' Recover a single record fast by using an argument csv file that passes
#' arguments to different functions for recovery.
#'
#' @param filepath file path of folder containing at least 3 files (trace, time dots, args). The argument file should contain columns for:
#'
#'   - radius  : the length of the radius arm in cm. This is constant across all
#'               records, but some of the earlier records that I have were
#'              magnified by a slightly larger amount, and therefore the scale
#'               is slightly different.
#'
#'   - center_y: height of the pivot point of the transducer arm from depth = 0
#'               in cm. In other words, the y-value of the center of the circle
#'               that draws the dive record. This value may change across
#'               records, but helper functions are available to provide
#'               estimates (?find_center_y).
#'
#'   - center_along_y: distance in cm used for centering the record. All time
#'                    dots will be centered along y = -center_along_y
#'
#'   - time_period_min: time elapsed between two time dots.
#'
#'   - spar_h  : spar value used for high depths of the records to increase
#'               resolution for the smooth_trace_dive() function.
#'
#'   - depth_thresh: depth threshold to use for the rolling mean function
#'                   to determine what depths should be considered diving
#'                   behavior.
#'
#'   - date_start: start of the record in y:m:d h:m:s format.
#'
#'   - max_depth: maximum depth to use for depth transformation. Only for records
#'                without a psi calibration curve.
#'
#'   - k_h     : larger window to use for zoc() function, if needed.
#'
#'   - depth_bounds_l, depth_bounds_h: low and high depth bounds that encapsulate
#'               where depth = 0 is likely to be. Used for zoc() function.
#'
#'  - on_seal : time the tdr was placed on seal. In y:m:d h:m:s format.
#'
#'  - off_seal: time the tdr was taken off seal. In y:m:d h:m:s format.
#'
#' @return trace data frame after full recovery, added to global environment.
#' @export
#' @examples
#' \dontrun{
#' filepath <- system.file("extdata", "WS_25_1981", package = "recoverKBTDR")
#' fast_recovery(filepath)
#' }
#'
# TODO: the radius MIGHT change across records if they xerographed by a
# different scale... and the max_psi value might needed to be added to the args
# file (one TDR had a max_psi value of 1000, not 900)

###############################################################################
# Function: fast_recovery(filepath = "../sample_data/WS_25_1981")
# Author:   EmmaLi Tsai
# Date:     6/29/21
#
# Wrapper function that allows for fast recovery of a trace using an argument
# file that passes all function arguments to the functions in this repository.
# This was mainly created to start thinking about user interface, but also so I
# could quickly read in traces with the appropriate arguments. This was more
# important for my private repo with all the records.
#
# Input:
#   - filepath   : file path that points to all the csv files needed for
#                  recovery (trace, time_dots, and args file)
# Output:
#   - trace      : fully recovered record (centered, zoc (if needed), time and
#                  depth transformation, smoothed, with POSIXct dates and times
#                  added)
###############################################################################
fast_recovery <- function(filepath = "../data/WS_folder"){

  # read in the record
  read_trace(filepath = filepath)

  # center the scan
  trace_tidy <- center_scan(trace_tidy, time_dots_tidy,
                            center_along_y = args_tidy$center_along_y)

  # getting the centered psi calibration curve, if the record has one
  if (is.na(args_tidy$max_depth)){
    psi_calibration <- .centered_psi_calibration(trace_tidy)
  }

  # zoc, if needed
  if (!is.na(args_tidy$k_h)){
    trace_tidy <- zoc(trace_tidy,
                      k_h = args_tidy$k_h,
                      depth_bounds = c(args_tidy$depth_bounds_l, args_tidy$depth_bounds_h))
  }

  # transforming x-axis to time (minutes from the start)
  trace_tidy <- transform_x_vals(trace_tidy, time_dots_tidy,
                                 center_y = args_tidy$center_y,
                                 time_period_min = args_tidy$time_period_min)

  # dates and times with interpolated points
  trace_tidy <- add_dates_times(trace_tidy,
                                start_time = args_tidy$date_start,
                                on_seal = args_tidy$on_seal,
                                off_seal = args_tidy$off_seal)

  if(!is.na(args_tidy$max_depth)){
    trace_tidy <- transform_y_vals(trace_tidy, args_tidy$max_depth)
  } else {
    # v max_psi doesn't usually change... but this might have to be included in
    # future arguments
    trace_tidy <- transform_y_vals(trace_tidy,
                                  psi_calibration = args_tidy$psi_calibration,
                                  max_psi = 900,
                                  max_position = 22.45)
  }

  # smooth
  trace_tidy_recovered <<- smooth_trace_dive(trace_tidy,
                                            spar_h = args_tidy$spar_h,
                                            depth_thresh = args_tidy$depth_thresh)

  # writing to csv file in results folder:
  # base_name <- unlist(strsplit(filepath, "/"))[3]
  # results_filepath <- paste("../results/", base_name, ".csv", sep = "")
  # write.csv(trace, results_filepath)
  # ^ unsure if this will be added to the package? Might be better to just
  # add it to the global environment
  #
}
