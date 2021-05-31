# This is a work in progress, but here is what I have so far to address Issue 
# #13 in GitHub

# This function finds the center_y value of the arm given two points along the 
# descent of a dive (x1, y1; x2, y2). This dive should be rapid with little to 
# no bottom time, since this would imply that the seal was moving at a constant 
# rate. To do this, the function calculates the amount of time it would've taken
# the seal to descend to depth, assuming the seal is descending at 1.1 m/s 
# (from Williams et al., 2015). The time of this descent was then used to 
# transform x2 over in the -x direction, such that the two points would be along 
# the same circle with a center of (h, k). Then, the center of the circle was 
# calculated using methods described in: 
# 
# https://math.stackexchange.com/questions/1781438/finding-the-center-of-a-
# circle-given-two-points-and-a-radius-algebraically
# 
# because this math is easier to put in code rather than solving a system of 
# equations. The returned K value should be the center_y value for the record.

# This function also takes: r = 20.87, which is the constant length of the 
# traducer arm; the rate that the record was moving at the time these two points 
# were taken ((time point 2 - time point 1) / 12 minutes); and the file used 
# for psi calibration (only available for 1981 traces). 

find_center_y <- function(x1, y1, x2, y2, r = 21.14, rate, psi_calibration = psi_calibration){
  # First, I am transforming y2 to depth in meters. This was needed to estimate 
  # the amount of time it would've taken for the seal to descend to this depth. 
  
  # defining the breaks: 
  breaks <- psi_calibration$psi_position
  
  # defining the labels: 
  labels <- psi_calibration$psi_interval[1:6]
  
  # creating a psi and interval column together to make the calculations easier
  # the psi is in front of the corresponding y position with  ":" that will 
  # be split later. This was just a way to do a cut with two different labels, 
  # since I need both the psi and the position of the psi for these calculations
  psi_interval_both <- cut(y2, breaks = breaks, labels = labels)
  
  # using psi and the position of the psi calibration to calculate depth 
  psi <- as.numeric(strsplit(as.character(psi_interval_both), ":")[[1]][1])
  cm_psi <- as.numeric(strsplit(as.character(psi_interval_both), ":")[[1]][2])
  depth <- ((y2 * psi) / cm_psi) / PSI_TO_DEPTH
  
  # finding time it took for seal to descend to that depth assuming it is 
  # descending at 1.1 m/s (Williams et al., 2015) and transforming it to 
  # minutes 
  t <- (depth / 1.1) / 60
  
  # transforming x2 over in the -x direction such that x1 and x2 would be along
  # the same circle with center (h,k)
  x2 = x2 - (rate * t)
  
  # geometry from stack exchange (link above-- I also have sample calculations 
  # to confirm that this method of using the geometry of a rhombus works): 
  xa =  0.5 * (x2 - x1)
  ya =  0.5 * (y2 - y1)
  
  a = sqrt(xa^2 + ya^2)
  b = sqrt(r^2 - a^2)
  
  x0 = x1 + xa
  y0 = y1 + ya
  
  h = x0 + ((b * ya)/a)
  k = y0 - ((b * xa)/a)
  
  return(rbind(h, k))
}

# running with sample values from WS_25:
# find_center_y(1142.9, 0, 1140.5, 9.3, 21.14, 0.16, psi_calibration)

## Keep global level code out of the function files
