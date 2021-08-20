
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

This is a basic example which shows you how to solve a common problem: 

Maybe here show general recovery workflow?

``` r
library(recoverKBTDR)
## basic example code
```
