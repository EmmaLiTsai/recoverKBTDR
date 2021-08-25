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
#'               in cm. Changes across all records, but I have helper functions
#'               to provide estimates (see find_center_y_functions.R).
#'
#'   - dist_timedot: distance in cm used for centering the record. All time dots
#'               will be centered along y = dist_timedot.
#'
#'   - time_period_min: time elapsed between two time dots.
#'
#'   - spar_h  : spar value used for high depths of the records to increase
#'               resolution for the smooth_trace_dive() function.
#'
#'   - depth_bounds_smooth: depth threshold to use for the rolling mean function
#'               to determine what depths should be considered diving behavior.
#'
#'   - date_start: start of the record in ymd_hms format
#'
#'   - max_depth: maximum depth to use for depth transformation. Only for records
#'                older than 1981 without a psi calibration curve
#'
#'   - k_h     : larger window to use for zoc() function, if needed
#'
#'   - depth_bounds_l, depth_bounds_h: low and high depth bounds that encapsulate
#'               where depth = 0 is likely to be. Used for zoc() function.
#'
#'  - on_seal : time the tdr was placed on seal. In ymd_hms format.
#'
#'  - off_seal: time the tdr was taken off seal. In ymd_hms format.
#'
#' @return trace data frame after full recovery, added to global environment
#' @export
#' @examples
#' \dontrun{
#' filepath <- system.file("extdata", "WS_25_1981", package = "recoverKBTDR")
#' fast_recovery(filepath)
#' }

###############################################################################
# Function: fast_recovery(filepath = "../sample_data/WS_25_1981")
# Author:   EmmaLi Tsai
# Date:     6/29/21
#
# Wrapper function that allows for fast recovery of a trace using an argument
# file that passes all function arguments to the functions in this repository.
# The argument file contains 14 different arguments:
#
#   - radius  : the length of the radius arm in cm. This is constant across all
#               records, but some of the earlier records that I have were
#               magnified by a slightly larger amount, and therefore the scale
#               is slightly different.
#
#   - center_y: height of the pivot point of the transducer arm from depth = 0
#               in cm. Changes across all records, but I have helper functions
#               to provide estimates (see find_center_y_functions.R).
#
#   - dist_timedot: distance in cm used for centering the record. All time dots
#               will be centered along y = dist_timedot.
#
#   - time_period_min: time elapsed between two time dots.
#
#   - spar_h  : spar value used for high depths of the records to increase
#               resolution for the smooth_trace_dive() function.
#
#   - depth_bounds_smooth: depth threshold to use for the rolling mean function
#               to determine what depths should be considered diving behavior.
#
#   - date_start: start of the record in ymd_hms format
#
#   - max_depth: maximum depth to use for depth transformation. Only for records
#                older than 1981 without a psi calibration curve
#
#   - k_h     : larger window to use for zoc() function, if needed
#
#   - depth_bounds_l, depth_bounds_h: low and high depth bounds that encapsulate
#               where depth = 0 is likely to be. Used for zoc() function.
#
#   - on_seal : time the tdr was placed on seal. In ymd_hms format.
#
#   - off_seal: time the tdr was taken off seal. In ymd_hms format.
#
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
fast_recovery <- function(filepath = "../sample_data/WS_25_1981"){

  # read in the record
  read_trace(filepath = filepath)

  # center the scan
  trace_raw <- center_scan(trace_raw, time_dots_raw,
                           center_along_y = args_raw$dist_timedot)

  # getting the centered psi calibration curve, if the record has one
  if (is.na(args_raw$max_depth)){
    psi_calibration <- .centered_psi_calibration(trace_raw)
  }

  # zoc, if needed
  if (!is.na(args_raw$k_h)){
  zoc(trace_raw,
      k_h = args_raw$k_h,
      depth_bounds = c(args_raw$depth_bounds_l, args_raw$depth_bounds_h))
  }

  # transforming x-axis to time (minutes from the start)
  trace_raw <- transform_x_vals(trace_raw, time_dots_raw,
                                center_y = args_raw$center_y,
                                time_period_min = args_raw$time_period_min)

  # dates and times with interpolated points
  trace_raw <- add_dates_times(trace_raw,
                           start_time = args_raw$date_start,
                           on_seal = args_raw$on_seal,
                           off_seal = args_raw$off_seal)

  if(!is.na(args_raw$max_depth)){
    trace_raw <- transform_y_vals(trace_raw, args_raw$max_depth)
  } else {
    trace_raw <- transform_y_vals(trace_raw,
                                  psi_calibration = args_raw$psi_calibration,
                                  max_psi = 900,
                                  max_position = 22.45)
  }


  # smooth
  trace_raw_recovered <<- smooth_trace_dive(trace_raw,
                                            spar_h = args_raw$spar_h,
                                            depth_thresh = args_raw$depth_bounds_smooth)

  # writing to csv file in results folder:
  # base_name <- unlist(strsplit(filepath, "/"))[3]
  # results_filepath <- paste("../results/", base_name, ".csv", sep = "")
  # write.csv(trace, results_filepath)
  #
}
