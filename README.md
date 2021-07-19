# MS_DiveTraces
Repo for recovering 1970s - 1980s Weddell seal paper dive records from Kooyman-Billups TDRs


Contains: 

 (1) r_scripts     - all .R script files with code that fixes various issues with the KBTDR traces. 
 		  	Within this folder, you can find eight files:

			  - dive_trace_tidy_functions.R -- contains organized production code that can be broken up into six main steps: 

				(1) Re-centering and fixing misalignment: this mainly focuses on tidying the trace and time_dots csv files after
				    image processing steps in ImageJ. This code also aims to center the scan. Since this code is mainly related 
				    to image processing, I moved code for this step to the read_trace.R file, and fixing misalignment code to 
				    centering_functions.R file. 
			      (2-3) Transform coordinates: this aims to remove the arc in these data and also transform the x axis into time
				    using the timing dots. 
				(4) Creating a regular time series using linear interpolation. This helps with records that are discontinuous 
				    which makes future dive analysis challenging.
				(5) Transform y axis to depth: this step will transform interpolated y values to psi using the psi calibration 
				    at the end of the trace, and transform these psi values to depth. 
				(6) Smoothing: this step smooths these data to simplify the trace. This is needed to help remove noise 
				    and extract the main trace from the thickness of the line, and for future dive analysis. This step also 
				    contains some sample cross validation methods for choosing the best smoothing parameters (in development). 
			
			  - testing_code.R      -- contains testing code for functions created in all R files in this project. This was needed to 
						   separate testing and production code, and uses the trace and time_dots csv files in the 
						   sample data folder in this repository. 
			  
			  - find_center_y_functions.R -- contains two functions with math to estimate the height of the transducer arm pivot point 
						 	 given two points along the descent of a dive. This function was intended to provide a tool 
							 for estimating the height of the pivot point of the transducer arm, but this value should
							 be visually confirmed to ensure that it does not introduce any abnormal skew across the 
							 record. 	
						   
			  - centering_functions.R -- contains methods for centering records and another function, centered_psi_calibration, that 
						     extracts the psi calibration curve after centering. It creates a data frame and was intended 
						     to make future depth calculations more accurate after centering. 

			  - zoc_functions.R 	-- contains functions for zero offset correction (zoc) of the trace before arc removal. This 
						   file is modeled after code that can be found in the diveMove package. The sample trace
						   in this repo doesn't require zoc before arc removal, but some of the other records have
						   extreme level shifts and/or drift in depth = 0 within a bout, so I thought this file may be 
						   useful to include. This file also includes an extra function (zoc_big_drift) to help zoc 
						   records that have extreme drift in depth = 0 such that zoc was difficult. 

			  - read_trace.R 	-- contains a function to read in all sample data (trace, time dots, and argument files) into the 
						   global environment. First attempt at data abstraction, and contains code to tidy the trace and time 
						   dot csv files.   

			  - fast_recovery.R     -- contains a wrapper function that uses an args.csv file to pass trace-specific arguments to all 
					           functions in this repository. Intended to make recovery faster instead of having to tab through the 
						   whole testing_code.R file, and to begin brainstorming user interface. This function is more important 
						   for my private repository with the whole dataset for this project. 
 
 (2) sample_data   - contains XY coordinates of trace and timing dots data (from Weddell seal #25) as .csv files, and a .txt file that outlines the image 
                     processing methods used to create this record. This folder also contains an args.csv file that is only used in the fast_recovery.R 
		     file. Description of the contents in this argument file can be found in the function header of the fast_recovery() function.

 (3) results       - folder that contains results for this project.
 

