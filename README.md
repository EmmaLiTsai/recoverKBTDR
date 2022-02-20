# MS_DiveTraces
Repo for recovering 1970s - 1980s Weddell seal paper dive records from Kooyman-Billups TDRs


Contains: 

recoverKBTDR - R package development folder. Contains all sample data (/inst), documentation(/vignettes), package development 
	         code (/R), and function documentation (/man). Package checks and development workflow can be found within 
		   package_dev_code.R. The package passes check, aside from the one 'qpdf' warning for large file sizes. 
               No errors or notes. The /vignettes/recoverKBTDR.Rmd file and the README walks through the whole dive record 
		   recovery process with plots to illustrate the output of each recovery function. 
 
Within the /R folder, you can a series of .R scripts that fixes various issues with the KBTDR traces:

 1. read_trace.R 	-- contains two functions to read and tidy the sample trace data (time dots and dive trace) into the 
			   global environment. 

 2. centering_functions.R -- contains methods for centering records and another function, centered_psi_calibration, that 
			   extracts the psi calibration curve after centering. It creates a data frame and was intended 
			   to make future depth calculations more accurate after centering. 

 3. zoc_functions.R -- contains functions for zero offset correction (zoc) of the trace before arc removal. This 
		         file is modeled after code that can be found in the diveMove package. The sample trace
			   in this repo doesn't require zoc before arc removal, but some of the other records have
			   extreme level shifts and/or drift in depth = 0 within a bout, so I thought this file may be 
			   useful to include. This file also includes an extra function (zoc_big_drift) to help zoc 
			   records that have extreme drift in depth = 0 such that zoc was usually difficult. 

 4. dive_tidy_functions.R -- contains organized production code that can be broken up into five main steps: 

    -(1-2) Transform coordinates: this aims to remove the arc in these data and also transform the x axis into time
	  using the timing dots. 
    -(3) Creating a regular time series using linear interpolation. This helps with records that are discontinuous 
	  which makes future dive analysis challenging.
    -(4) Transform y axis to depth: this step will transform interpolated y values to psi using the psi calibration 
	  at the end of the trace, and transform these psi values to depth. 
    -(5) Smoothing: this step smooths these data to simplify the trace. This is needed to help remove noise 
	  and extract the main trace from the thickness of the line, and for future dive analysis. 

  5. fast_recovery.R  -- contains a non-essentail wrapper function that uses an argument file to pass trace-specific 
			   arguments to all functions in this repository. Intended to make recovery faster instead of having to  
			   run each function individually, but it is up to the user. This is likely more helpful if you have
			   many KBTDR records to recover. 

  6. helper_functions.R -- non-essential functions intended to help with finding the best spar smoothing penalty and 
		         the height of the transducer arm for arc removal. 

			  

All other files within this folder are for documentation purposes. 