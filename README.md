# MS_DiveTraces
Repo for recovering 1970s - 1980s Weddell seal paper dive traces from Kooyman-Billups TDRs

Contains: 

 (1) R Scripts     - all .R script files with code that fixes various issues with the KBTDR traces. Many of these .R files are preliminary code. 
 		  	Within this folder, you can find two files:

			  - example_trace.R    -- contains example code that completes six main objective: removing the arc, spline smoothing, 
						  adding the time dots, transforming the x-axis to time, scan centering, and adding dates and 
						  times to the data. This file works within the sample_data working directory. However, this  
						  code is old and unorganized.

			  - dive_trace.R       -- contains more finalized code

			  - dive_traces_tidy.R -- contains organized production code that can be broken up into six main steps: 

				(1) Re-centering and fixing misalignment: this mainly focuses on tidying the trace and time_dots csv files after
				    image processing steps in ImageJ. This code also aims to center the scan. Since this code is mainly related 
				    to image processing, I moved code for this step to the scan_tidying_functions.R file. 
			      (2-3) Transform coordinates: this aims to remove the arc in these data and also transform the x axis into time
				    using the timing dots. 
				(4) Transform y axis to depth: this step will transform y values to psi using the psi calibration at the end 
				    of the trace, and transform these psi values to depth. 
				(5) Smoothing: this step will smooth these data to simplify the trace. This is needed to help remove noise 
				    and extract the main trace from the thickness of the line. 
				(6) Final tidying: this will add dive statistics, direction flagging, and also add dates and times to these data. 
			
			  - scan_tidying_functions.R -- contains code to complete step (1) in the dive_traces_tidy.R file. 
				
			  - testing_code.R      -- contains testing code for functions created in the dives_traces_tidy.R file and 
						   scan_tidying_functions.R file. This was needed to separate testing and produciton code, and 
						   uses the trace and time_dots csv files in the sample data folder in this repository. 		  			
 
 (2) Sample Data   - contain XY coordinates of sample trace data as .csv files, along with the corresponding positions of the timing dots. This 
		     file also contains a .txt file that outlines the image processing methods used to create these sample files.

 (3) Results       - folder that contains results for this project. 
 

