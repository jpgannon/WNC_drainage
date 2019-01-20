# WNC_watershed
This repository contains scripts that (1) delineate an experimental watershed in western North Carolina and (2) conduct subsequent topographic analyses. Importantly, we use R scripting to initiate the [WhiteBox tools](http://www.uoguelph.ca/~hydrogeo/WhiteboxTools/index.html) geographic information system (GIS).  This requires the user to download WBT execuatable, define a workspace directory, and manage workflow in both R environment and the workspace directory. 

An example workflow: 
1. Define DEM within the R enviornment. 
2. Smooth DEM with a moving window filter to remove irregulaties 
3. Write the updated DEM to defined workspace directory
4. Conduct topographic analysis using WBT, where WBT are executed from Rscript
5. Define resulting spatial files in R environment.

The example above highlights the transfer of data between R enviornment and workspace directory.  To use R geospatial tools (raster, sf, tmap ,etc), the spatial data must be in the R environment. Conversely, to use WBT, the data must be exported to the working directory.  Reading and writing large spatial files takes time/memory, so try to limit the number of "handoffs" where possible.  
