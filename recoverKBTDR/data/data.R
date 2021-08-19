# description of data

#' X and Y values of a sample record
#'
#' Source: Image Processing record WS_25 from Castellini et al., 1992
#'
#' @format A data frame with 2 variables: \code{X}, \code{Y}
#'
#' \describe{
#' \item{X}{x-value position of the point, in centimeters from the origin}
#' \item{Y}{y-value position of the point, in centimeters from the origin}
#' }
"trace"

#' X and Y values of timing dots for a sample record
#'
#' Source: Image Processing record WS_25 from Castellini et al., 1992
#'
#' @format A data frame with 2 variables: \code{X}, \code{Y},
#'
#' \describe{
#' \item{X}{x-value position of the point, in centimeters from the origin}
#' \item{Y}{y-value position of the point, in centimeters from the origin}
#' }
"time_dots"

#' Arguments to use for example record
#'
#' Source: Image Processing record WS_25 from Castellini et al., 1992
#'
#' @format A data frame with 2 variables: \code{X}, \code{Y},
#'
#' \describe{
#' \item{radius}{radius of transducer arm, scaled to paper records}
#' \item{center_y}{height of pivot point transducer arm from depth = 0}
#' \item{dist_timedot} y-value to center timing dots along
#' \item{time_period_min} minutes each time dot represents
#' \item{spar_h} high resolution spar value for spline smoothing
#' \item{depth_bounds_smooth} shallowest depth value for dive detection
#' \item{date_start} time TDR was turned on, in ymd_hms
#' \item{max_depth} maximum depth of paper record, if psi calibration curve is absent
#' \item{k_h} window to use for zero-offset correction
#' \item{depth_bounds_l} lower value of depth = 0, for zero-offset correction
#' \item{depth_bounds_h} higher value of depth = 0, for zero-offset correction
#' \item{on_seal} time TDR was placed on the seal
#' \item{off_seal} time TDR was taken off the seal
#' }
"args"
