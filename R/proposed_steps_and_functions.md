Outline of Steps and Major Functions: 

(1) File tidying and centering of the scan 

	Functions here will be added to a separate R script (scan_tidying_functions.R) because 
	these items are more related to image processing rather than fixing the trace. I run 
	this script using the source() function in the testing_code.R file. 	

	Here, I imagine that I will have a file tidying function to deal with default column 
	names from ImageJ image processing. I clearly see two functions to complete this 
	objective, but I could eventually wrap these into one function in the future: 

	    tidy_trace(trace): 
		I think this function will tidy the trace csv file to have the correct column
		names, and to remove extra or unnessary columns from the data. 
			
	     tidy_timedots(time_dots): 
		This function should tidy the time dots csv file by fixing the column names. 
		This function will also add an extra time dot at the beginning of the file, 
		which would be when the device started gathering data but before the first 
		time dot. 

	I also imagine another function that will center the scan using the y values of the 
	timing dots. This includes creating a brief fuzzy merge with the trace and timing 
	dots csv files. After this fuzzy merge, I plan to create y value correction 
	values to shift the above trace up or down such that all time dots are along the 
	same horizontal line. After this step, the trace file should be in good shape to 
	continue with recovery. This is an extra processing step, and might not be included  
	in the final code package. 

	    center_scan(trace, time_dots, dist_timedot = 1.1): 
		This function will center the trace csv file by using the y-values of the 
		timing dots. Here, I think I will do a fuzzy merge to connect the trace csv 
		file with the time dots file. I think this should be a full merge based on 
		distance between the x values. After the fuzzy merge, I will do some small 
		calculations to move the trace data up or down such that the time dots are 
		all along the same horizontal line that the user defines in dist_timedot.  
		The default is set to -1.1 cm below the trace. 

(2-3) Transform coordinates by radius arm equation, and create time scale
	
	Functions from steps 2-6 will be in the dive_traces_tidy.R file, which contains all 
	production code

	Here, I imagine a function that will apply the arc equation to the data, and also
	transform the x-axis from timing dots to time in minutes. 

      	    transform_coordinates(trace, time_dots, time_period_min = 12): 
		This function will create a new_x column that remove the arc from these 
		data. It should be a very straightforward and simple equation based on 
		the global RADIUS and CENTER_Y variables, which are measurements that 
		should be the same across all KBTDR devices. I think I will also create 
		a simple data frame of just the time points that identify the start x 
		value and end x value for all time periods for these data. Using this 
		constructed data frame, I will calculate the corresponding scale for this 
		time period by taking the time_period_min value and dividing this by the 
		distance between two time points. I will use this scale value later to 
		calculate the time for each point within a time period. Then, I think I 
		should use this time periods data frame to cut() the trace data frame 
		to categorize the new_x values of the trace into different time periods, 
		and label these categories based on the time period. Then, I could do a 
		full merge based on time period to connect the trace and simplified 
		time dots data frame I created earlier in this function, which will make 
		calculations easier. Then I could do a final mutate that would calculate
		the distance a point would be from the beginnig x value of the time
		period it belongs to and use the scale I made earlier to calculate time. 

	This function should remove the arc from the trace data and also create a reliable 
	time scale. 
 
(4) Transform y-axis from PSI to depth 

	Here, I expect to create a function that will use the psi calibration intervals 
	at the end of the 1981 traces to create a depth scale. This function will be 
	tricky! 

	   transform_psitodepth(trace, breaks, labels): 
		I think this function should first cut() the trace data frame into different
		psi categories based on the y value of a point. The categories might contain 
		psi category (100, 200, 400, 600, 800, 900), and the y value for this psi 
		category separated by a colon (i.e., 100:1.43). However, there might be a 
		better way to do this. I could then do a strsplit() to break these labels up 
		in to two different columns which will make calculations easier: the psi_interval 
		column (100, 200, 400, etc.), and the psi_position column (1.43cm, 3.49cm, 
		7.78cm, etc.). From these two columns, I could then calculate psi by: 
		(psi_interval * y_val) / psi_position. Then, I could take this psi value and 
		divide this value by PSI_TO_DEPTH constant, which is the increase in psi every 
		1 meter increase in depth for saltwater. 
		
		This function could also do a first pass at filtering the data, which would 
		take any value less than a certain depth and make it = 0. 

	This function should add depth to the data. 

(5) Smoothing 

 	This process should take the depth values from step 4 to simplify the trace using 
	a form of smoothing method. Currently this is spline smoothing, but is subject to 
 	change... Ideally I can use a smoothing method that retains the wiggles in the trace
	at depth but remove any noise at depth = 0 and performs well during the descent and 
	ascent portions of a dive. 

(6) Adding Dates and Times 

	This function adds dates and times to the data using the lubridate() package. This 
	function has the potential to be called within the transform_coordinates() function, 
	so I could produce a better date/time scale during step 2-3 of this recovery process. 
	I'll have this at the end of the file for now, since this is final cleaning before 
	this file can be read in using the diveMove package. 

	  add_dates_times(trace, start_time): 
		This function should add dates and times in ymd_hms format using the 
		Antarctica/McMurdo time zone. It should take the time in minutes that I 
		calculated in step 2-3 and transform this to hours, minutes, and seconds 
		since the TDR was attached to the seal. 
		
	