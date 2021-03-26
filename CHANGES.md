# changes.md

This document builds off the arc transformation .R file to also include work
on the time periods, depth scale, centering, and spline smoothing. The final 
graph is the closest fully recovered trace that I have made so far ET (2/24)

## TODO list

  - TODO: Currently, I am working on reducing the "noise" at depth = 0, and 
       fixing the spline smoothing so that the smoothed line picks up when the 
       seal resurfaces for ~10 minutes during a bout of dives. This is the most 
       urgent task before I attempt the ones below. 
  
  - UPDATE: somewhat resolved? Smoothing issues are resolved, but I am currently
       checking the new method for removing noise at depth = 0. See last 
       section of this document. ET 2/24
 
   - TODO: I need to double-check the depth transformation. The transformation 
       itself is easy, but I had to "center" the scan and I'm not confident 
       that I did it correctly. There is a software from NeruaScanner that can
       center the scan for me, but I want to see if I can do it in code. 
 
   - UPDATE: I have centered the scan by using the timing dots in this update. 
       See code below. ET 2/15
 
   - TODO: some of the smoothed y_values seem to stray from the overall trend of
       the trace (i.e. picking up the other edge of the line). Maybe this could 
       be fixed by changing the spar value? 
 
   - UPDATE: This issue is somewhat fixed by grouping the data by x value and 
       the spline smoothing function that I created that breaks up the data 
       into chunks and applies smoothing over smaller degrees of freedom. 
       However, a new challenge emerges when trying to identify when the seal 
       surfaces within a bout of dives-- see to-do task number 1. ET 2/15
 
   - UPDATE: This has been resolved. I changed the image processing workflow to 
       skeletonize the trace, which greatly improved smoothing.  ET 2/24
         
   - TODO: check the output of this code with the castellini (1992) bulletin. The 
       max depth for this trace seems to align, but confirm other dive
       parameters to ensure the quality of these methods. 

   - UPDATE: removed depth transformation section for now, will come back to this 
       once I tackle the noise and smoothing issues above. ET 2/15

   - UPDATE: modified image processing by using "skeletonization" which helps the 
       smoothing functions capture more of the behavior at depth. 
