################################################################################
## Authors:  EmmaLi Tsai, Dylan Schwilk
## Date: 2020-12-15
################################################################################

## This script provides the digitization steps for a dive trace. Currently this
## is all top level code; later, we will wrap this as exportable functions. see
## github issue #3 for discussion of steps.

###############################################################################
# Loading packages. TODO: for package development, we will required packages
# explicitly or eliminate them.
library(ggplot2)
library(dplyr)
###############################################################################

## Constants
## SPLINE_SPAR = ??? ## For now, use defaults in smooth.spline
## From smooth.spline documentation: "If spar and lambda are missing or NULL,
## the value of df is used to determine the degree of smoothing. If df is
## missing as well, leave-one-out cross-validation (ordinary or ‘generalized’
## as determined by cv) is used to determine λ."

###############################################################################
## Read data.
## TODO: move this code to an examples section eventually.
trace <- read.csv("./manual_trace.csv", header = TRUE, 
                  stringsAsFactors = FALSE)

###############################################################################
## Step 1. Convert x units to time using the dots.
###############################################################################

## TODO. See issue #2, https://github.com/EmmaLiTsai/MS_DiveTraces/issues/2

###############################################################################
## Step 2. Initial transformation to allow smoothing
###############################################################################

## Dive traces can have multiple y values at the same X representing separate
## dives that appear to overlap because of the arc of the depth recording arm.
## This is solved in step 5, but we must temporarilly transform to avoid this
## overlap so we can use smoothing functions.

## DWS: Note, y axis is inverted. I'd prefer fixing that first, then mvoing on,
## ut I think all of EmmaLi's code depends on this inversion so will leave for
## now.

trace <- mutate(trace, x=x_val, y=y_val, new_x = x + y*0.5) %>%
  select(x, y, new_x) %>%
  arrange(new_x)
# TODO: Check that this is a general solution.


###############################################################################
## Steps 3-4. Smooth the dive curves and assign named dive components
###############################################################################

## Note: we might consider averaging the curve first, then fitting the spline.
## But for now, we fit the spline despite having duplicated x values. In the
## test scan we re using, averaging first avoids the slight offset at the
## beginning of the first big dive. This is an easy change if needed.

## Fit spline with generalized cross validation. TODO: specify parameters in
## case defaults change in smooth.spline.
spline.mod <- smooth.spline(trace$new_x, trace$y)

trace <- mutate(trace, smooth_y = predict(spline.mod, trace$new_x)$y,
                deriv = predict(spline.mod, trace$new_x, deriv=1)$y,
                ascent = deriv < 0,
                deriv_diff=lag(sign(deriv)) - sign(deriv),
                peak = case_when(deriv_diff < 0 ~ "TOP",
                                 deriv_diff > 0 ~ "BOTTOM")
                )


## DWS: TODO rename factor levels and rename columns as needed. I am keeping
## "ascent" as a boolean and "peak" as a factor with two levels and many NA
## values. I have chosen names poorly, please fix!


###############################################################################
## Debug plots to show this all works
###############################################################################

## TODO: REMOVE, the below is debug only.

p1 <- ggplot(trace, aes(x=new_x, y = smooth_y)) +
  geom_line(aes(new_x, y), color="gray", size=0.2) +
  geom_point(aes(color=deriv > 0)) +
  geom_line()
p1
ggsave("./results/spline_smooth_example.png", plot=p1)


p2 <- ggplot(trace, aes(new_x, smooth_y)) +
  geom_line(aes(new_x, y), color="gray", size=0.2) +
  geom_point() +
  geom_line() +
  geom_point(aes(color = peak),
             data=filter(trace, !is.na(peak)),
             size=4, alpha=0.8)
p2
ggsave("./results/peaks_and_troughs.png", plot=p2)


###############################################################################
## Step 5: Transform ordered data using arc equation.
###############################################################################

## TODO. Working code in Tsai_DiveTraces_Dec11.R but it is pretty verbose.


