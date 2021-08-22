
# recoverKBTDR

<!-- badges: start -->
<!-- badges: end -->

## Overview

This package contains a suite of functions for recovering dive records from 1960's film-based Kooyman-Billups Time Depth Records (KBTDRs). This TDR was among the first placed on a free-ranging marine organism, but the film-based format of the behavioral data makes long-term comparisons exceptionally challenging. This package returns a corrected, continuous, and digitized file of a KBTDR record complete with dates, times, and depth that can be easily read into dive analysis software. 

There are six main recovery steps that are achieved in this package: 

1. Scan centering and zero-offset correction 
2. Arc removal 
3. x-axis transformation to dates & times 
4. Interpolation between missing points 
5. y-axis transformation to depth 
6. spline smoothing 

## Installation

You can install the released version of recoverKBTDR from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("recoverKBTDR")
```

## Example

This is a basic example of the intended workflow: 

First, read in data:
data(trace)
data(time_dots)

Center the scan using the timing dots, such that all timing dots will be centered along  y = -center_along_y. Also return the centered psi calibration curve, if present: 
trace <- center_scan(trace, time_dots, center_long_y = 0.9, psi_interval = c(100, 200, 400, 600, 800))

Transform the x-axis into minutes using the timing dots, and remove the arc in the data by defining the height of the pivot point above y = 0 (center_y, in cm). 
trace <- transform_x_vals(trace, time_dots, center_y = 11.18, time_period_min = 12)

Add POSIXct date times and create a regular time series using the time the TDR was turned on (start_time), and the times it was placed on and off the seal: 
trace <- add_dates_times(trace, start_time = "1981:01:16 15:10:00", on_seal = "1981:01:16 17:58:00", off_seal = "1981:01:23 15:30:00")

Transform y-axis to depth either using the maximum depth, or psi calibration curve: 
If psi calibration curve is present:
trace <- transform_y_vals(trace, psi_calibration = psi_calibration, max_psi = 900, max_position = 22.45)
If we just know max depth:
trace <- transform_y_vals(trace, maxdep = 319)

Spline smoothing to reduce noise in the data by passing the spar value and depth threshold (in meters) to use when a dive is detected: 
trace <- smooth_trace_dive(trace, spar_h = 0.3, depth_thresh = 5)

Then, final data frame can be exported and read into dive analysis software. 

``` r
library(recoverKBTDR)
## basic example code
data(trace) 

# or, reading in raw csv file: 
filepath <- system.file("extdata", "WS_25_1981", package = "recoverKBTDR")
read_trace(filepath)

# center
trace <- center_scan(trace, time_dots, 0.9, psi_interval = c(100, 200, 400, 600, 800))

# arc removal and time assignment 
trace <- transform_x_vals(trace, time_dots, center_y = 11.18, time_period_min = 12)

# POSIXct date times 
trace <- add_dates_times(trace, start_time = "1981:01:16 15:10:00", on_seal = "1981:01:16 17:58:00", off_seal = "1981:01:23 15:30:00")

# add depths
trace <- transform_y_vals(trace, psi_calibration = psi_calibration, max_psi = 900, max_position = 22.45)

# spline smoothing 
trace <- smooth_trace_dive(trace, spar_h = 0.3, depth_thresh = 5)

# export 

```
