# MS_DiveTraces
Repo for recovering 1970s - 1980s Weddell seal paper dive traces from Kooyman-Billups TDRs

Contains: 

 (1) R Scripts     - all .R script files with code that fixes various issues with the KBTDR traces. Many of these .R files are preliminary code. 
 		  	Within this folder, you can find two files:

			  - Tsai_TimePeriods.R -- contains all preliminary code that completes six main objective: removing the arc, spline smoothing, 
						  adding the time dots, transforming the x-axis to time, scan centering, and adding dates and 
						  times to the data. This file works within the sample_data working directory. 
			  - dive_trace.R       -- contains more finalized code
 
 (2) Sample Data   - contain XY coordinates of sample trace data as .csv files, along with the corresponding positions of the timing dots. This 
		     file also contains a .txt file that outlines the image processing methods used to create these sample files. 
 
 (3) Results       - folder the contains current results 
 
 
