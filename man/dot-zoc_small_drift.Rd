% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zoc_functions.R
\name{.zoc_small_drift}
\alias{.zoc_small_drift}
\title{Zero-offset correct tidy trace data frame with small drift}
\usage{
.zoc_small_drift(trace, k_h = 500, depth_bounds = c(-1, 1))
}
\arguments{
\item{trace}{tidy trace data frame, contains the x and y values of the trace}

\item{k_h}{numeric value, size of the larger window used for the second filter. This default is set to 500, but should be smaller for records with extreme drift.}

\item{depth_bounds}{two concatenated numbers restricted search window for where y = 0 should likely be. For the records, this should be in cm. Default set c(-1, 1).}
}
\value{
trace data frame after it has been zero-offset corrected
}
\description{
Zero-offset correct tidy trace data frame with small drift
}
\examples{
\dontrun{
zoc(trace, k_h = 500, depth_bounds = c(-1, 1))
}
}
