# Topic: zero offset correcting (zoc) the data before arc removal ##############
# Date: 5/20/2021 
# 
# Still a work in progress! 
# 
# Zoc would make arc removal more reliable since the arc equation assumes 
# that depth = 0 is the same as y = 0, but there is (slight) drift in y = 0 
# for some of the records. This issue also happens with modern TDRs, but zero
# offset correction is needed before the arc in the data is removed. While the 
# sample data in this repo is somewhat free of this drift, there are a few older 
# records with extreme level shifts and drift in depth = 0 within a bout of 
# dives. Therefore, I thought it might be helpful to include this (still 
# preliminary) file in the github repo.  
# 
# Here, I modified some code from the diveMove package on GitHub to work with 
# the tidy trace data. This code relies on two functions that I pulled from the 
# page (.runquantile, and .EndRule-- both originally from the caTools package 
# that specializes in moving window statistics. Both present at the end of this 
# file), and the code I wrote outside the function is modeled after the code you 
# can find in the .depthFilter function on the GitHub page. The .depthFilter
# function runs on S4 objects, so I had to modify the code here for the simple 
# trace data and so I could continue working with the data (i.e., remove arc, 
# add times, depth calibration, etc.) before final dive analysis. 
# 
# This filtering method for zero offset correction can be found in: 
# Luque & Fried, 2011: Recursive filtering for zero offset correction of diving 
# depth time series with GNU R package diveMove 
#
################################################################################

# defining window and quantiles for filtering method of zoc -- this will be 
# unique for each trace: 
#   depth bounds = restricted search for where depth = 0 should be (in cm)
#              k = size of windows used for first and second filters 
#          probs = quantiles to extract for each k step 

zoc <- function(trace, k = c(3, 500), probs = c(0.5, 0.02), depth.bounds = c(-5, 1)){
  
  # logical vector if there is an NA depth
  d.na <- is.na(trace$y_val)
  # seeing if the depth is in the bounds of where y = 0 should likely be:
  d.in.bounds <- trace$y_val > depth.bounds[1] & trace$y_val < depth.bounds[2]
  # grabbing the depths that fall into the category above or is NA:
  d.ok <- which(d.in.bounds | is.na(trace$y_val))
  
  # creating a matrix of y_values for each filter: 
  filters <- matrix(trace$y_val, ncol = 1)
  
  # for each k window of smoothing: 
  for (i in seq(length(k))) {
    # add a column of y values from the trace df
    filters <- cbind(filters, trace$y_val)
    # grab the depths that are near depth = 0 for the window of smoothing:
    dd <- filters[d.ok, i]
    # calls the runquantile function, which creates a rolling quantile across the 
    # window. This function also relies of the EndRule function which takes the 
    # result of runquantile and transforms it into a vector.
    filters[d.ok, i + 1] <- .runquantile(dd, k=k[i], probs=probs[i])
    
    ## Linear interpolation for depths out of bounds-- in other words,
    # approximate the position of depth = 0 when the seal is diving at greater 
    # depths 
    offbounds <- which(!d.in.bounds)
    offbounds.fun <- approxfun(seq(length(trace$y_val))[d.in.bounds],
                               filters[d.in.bounds, i + 1], rule=2)
    filters[offbounds, i + 1] <- offbounds.fun(offbounds)
    
    ## NA input should be NA output regardless of na.rm
    filters[d.na, i + 1] <- NA
  }
  
  # finding the depth adjustment by taking the y value and subtracting by the 
  # filters
  depth.adj <- trace$y_val - filters[, ncol(filters)]
  # binding new depth adjustment with original trace data 
  # I subtracted the depth adjustment by a small amount to account for the  
  # thickness of the trace.
  new <- cbind(trace$x_val, (depth.adj - 0.15))
  # transforming to df 
  new <- as.data.frame(new)
  # changing names for future functions: 
  names(new) <- c("x_val", "y_val")
  
  # returning the final output 
  return(new)
}

###############################################################################
## other functions needed below-- copied from diveMove but originally from 
## from caTools package. Specialize in moving window statistics. 
###############################################################################
.runquantile <- function(x, k, probs, type=7,
                         endrule=c("quantile", "NA", "trim", "keep",
                                   "constant", "func"),
                         align=c("center", "left", "right")) {
  ## see http://mathworld.wolfram.com/Quantile.html for very clear
  ## definition of different quantile types
  endrule <- match.arg(endrule)
  align <- match.arg(align)
  ## Capture dimension of input array - to be used for formating y
  dimx <- dim(x)
  yIsVec <- is.null(dimx)             # original x was a vector
  x <- as.vector(x)
  n <- length(x)
  np <- length(probs)
  k <- as.integer(k)
  type <- as.integer(type)
  if (k <= 1) return (rep(x, n, np))
  if (k > n) k <- n
  if (is.na(type) || (type < 1 | type > 9))
    warning("'type' outside allowed range [1,9]; changing 'type' to ",
            type <- 7)
  
  y <- double(n * np)
  y <- .C("run_quantile", as.double(x), y=y, as.integer(n),
          as.integer(k), as.double(probs), as.integer(np),
          as.integer(type), NAOK=TRUE)$y
  dim(y) <- c(n, np)
  
  for (i in 1:np) {                   # for each percentile
    yTmp <- .EndRule(x, y[, i], k, dimx, endrule, align, quantile,
                     probs=probs[i], type=type, na.rm=TRUE)
    if (i == 1) {
      if (is.null(dimx))
        dimy <- length(yTmp)
      else
        dimy <- dim(yTmp)
      yy <- matrix(0, length(yTmp), np) # initialize output array
    }
    yy[, i] <- as.vector(yTmp)
  }
  if (np > 1)
    dim(yy) <- c(dimy, np)
  else
    dim(yy) <- dimy
  return(yy)
}

.EndRule <- function(x, y, k, dimx,
                     endrule=c("NA", "trim", "keep", "constant", "func"),
                     align=c("center", "left", "right"), Func, ...) {
  ## Function which postprocess results of running windows functions and
  ## cast them in to specified format. On input y is equivalent to y =
  ## runFUNC(as.vector(x), k, endrule="func", align="center")
  
  ## === Step 1: inspects inputs and unify format ===
  align <- match.arg(align)
  k <- as.integer(k)
  k2 <- k %/% 2
  if (k2 < 1) k2 <- 1
  ## original x was a vector -> returned y will be a vector
  yIsVec <- is.null(dimx)
  if (yIsVec) dimx <- c(length(y), 1) # x & y will become 2D arrays
  dim(x) <- dimx
  dim(y) <- dimx
  n <- nrow(x)
  m <- ncol(x)
  if (k > n) k2 <- (n - 1) %/% 2
  k1 <- k - k2 - 1
  if (align == "center" && k == 2) align <- 'right'
  
  ## === Step 2: Apply different endrules ===
  if (endrule == "trim") {
    y <- y[(k1 + 1):(n - k2), ] # change y dimensions
  } else if (align == "center") {
    idx1 <- 1:k1
    idx2 <- (n - k2 + 1):n
    ## endrule calculation in R will be skipped for most common case
    ## when endrule is default and array was a vector not a matrix
    if (endrule == "NA") {
      y[idx1, ] <- NA
      y[idx2, ] <- NA
    } else if (endrule == "keep") {
      y[idx1, ] <- x[idx1, ]
      y[idx2, ] <- x[idx2, ]
    } else if (endrule == "constant") {
      y[idx1, ] <- y[k1 + 1 + integer(m), ]
      y[idx2, ] <- y[n - k2 + integer(m), ]
    } else if (endrule == "func" || !yIsVec) {
      for (j in 1:m) {
        for (i in idx1) y[i, j] <- Func(x[1:(i + k2), j], ...)
        for (i in idx2) y[i, j] <- Func(x[(i - k1):n, j], ...)
      }
    }
  } else if (align == "left") {
    y[1:(n - k1), ] <- y[(k1 + 1):n, ]
    idx <- (n - k + 2):n
    if (endrule == "NA") {
      y[idx, ] <- NA
    } else if (endrule == "keep") {
      y[idx, ] <- x[idx, ]
    } else if (endrule == "constant") {
      y[idx, ] <- y[n - k + integer(m) + 1, ]
    } else {
      for (j in 1:m) for (i in idx) y[i, j] <- Func(x[i:n, j], ...)
    }
  } else if (align == "right") {
    y[(k2 + 1):n, ] <- y[1:(n - k2), ]
    idx <- 1:(k - 1)
    if (endrule == "NA") {
      y[idx, ] <- NA
    } else if (endrule == "keep") {
      y[idx, ] <- x[idx, ]
    } else if (endrule == "constant") {
      y[idx, ] <- y[k + integer(m), ]
    } else {
      for (j in 1:m) for (i in idx) y[i, j] <- Func(x[1:i, j], ...)
    }
  }
  
  ## === Step 4: final casting and return results ===
  if (yIsVec) y <- as.vector(y)
  return(y)
}
