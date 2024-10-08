% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dive_tidy_functions.R
\name{transform_x_vals}
\alias{transform_x_vals}
\title{Transform x-axis to time}
\usage{
transform_x_vals(trace, time_dots, time_period_min = 12)
}
\arguments{
\item{trace}{tidy trace data frame after centering, contains the x and y
values of the trace.}

\item{time_dots}{tidy time dots data frame, contains the x and y positions
of the timing dots.}

\item{time_period_min}{minutes elapsed between two time periods.}
}
\value{
trace data frame after arc removal.
}
\description{
This function uses the timing dots the transform the x-axis to time, in
minutes from the start. This can later be transformed to POSIXct date times
and transformed into a regular time series in the add_dates_times function.
}
\examples{
\dontrun{
transform_x_vals(trace, time_dots, time_period_min = 12)
}

}
