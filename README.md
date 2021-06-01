# MS_DiveTraces
Repo for recovering 1970s - 1980s Weddell seal paper dive records from Kooyman-Billups TDRs


Contains: 

 (1) R Scripts     - all .R script files with code that fixes various issues with the KBTDR traces. 
 		  	Within this folder, you can find six files:

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
			  
			  - find_center_y.R  	-- contains a draft function with math to calculate the height of the transducer arm given two 
						   points along the descent of a dive. 	  	
						   
			  - center_scan_td_isses.R -- contains draft methods for centering records with unique time dot issues (i.e., time dots 
			  			      may be 60 minutes and ~8cm apart instead of 12 minutes apart and ~1.5cm apart). 
						      There are only 2 records with this issue, but I thought these methods might be helpful 
						      to include in the future code package. 

			  - zoc.R    		-- contains preliminary code for zero offset correction (zoc) of the trace before arc removal. 
						   This file is modeled after code that can be found in the diveMove package. The sample trace
						   in this repo doesn't require zoc before arc removal, but some of the other records have
						   extreme level shifts and drift in depth = 0 within a bout, so I thought this file may be 
						   useful to include. 
 
 (2) Sample Data   - contain XY coordinates of sample trace data as .csv files, along with the corresponding positions of the timing dots and 
		     psi calibration curve at the end of the record. This file also contains a .txt file that outlines the image processing 
		     methods used to create this sample record. 

 (3) Results       - folder that contains results for this project. 
 

