
--------- Workflow for Processing Scanned WS Dive Traces (1978 - 1981) in ImageJ: ----------------------------------------------------------------------------------

--- FOR TIMING DOTS --- 
- do this before the dive trace, because skeletonizing will remove faint timing dots 
- make scale by drawing a line of a set distance (i.e. timing dots) and set scale to that distance on the actual trace. 
- set origin by image --> properties; set the origin to the bottom left of the trace, right before the LED light begins 
          taking data and slightly below the trace
        - this is the same origin that scientists previously used when analyzing these data 
	- this also produces negative y values in the +y direction since ImageJ is weird with origin placement, but this 
          can be fixed easily in later processing
	- the origin you will set will be the pixel values 
- go to analyze --> set measurements and make sure centroid is the only one checked 
        - this will provide a more accurate estimation of the x_value of the time dot, rather than just clicking the 
          points. 
- use the wand tool to select the first timing dot
- press "m" to measure the centroid position of this timing dot 
- continue this through the whole trace
- at the end of the trace, go to file --> save as to save the positions of the timing dots as a .csv file 


--- FOR DIVE TRACE --- 
- make sure the trace is binary 8-bit-- all the BW scanned traces are already this format, but it is important 
- tidy trace with paintbrush and fill tool
	- use the fill tool to fill in parts of the trace that might get left out with the wand tool 
	- use the paintbrush tool to cut off any parts of the background that aren't part of the trace (i.e. tape that 
          connected two parts of film)
- process --> binary --> skeletonize 
        - this helps reduce the trace to allow for better smoothing
- select what you want with wand tool
	- holding down shift allows you to select more than one segments of a trace
	- select WHOLE trace, including the calibration lines at the end of the 1981 traces
- edit --> selection --> properties --> extract coordinates
	- this will produce a popup box that contains all XY coordinates of the selected trace relative to the origin 
          you set earlier 
	- go to the file tab in the popup box and extract these values to a CSV file 
- in excel: 
	- first sort by ascending by x values, or else the plot you make won't make sense 
	- create a new column called y_corr that takes the abs(y_value) since where we put the orgin made -y values in 
          the +y direction 
        - since this introduces error with points crossing origin, I modified this by creating a new column called 
          y_corr_p2 which is the excel code: IF(B2>0, -B2, ABS(B2)), which accounts for values crossing the origin 
          (2/19 ET)
	- This last excel step has been automated in the scan_tidying_functions.R file

