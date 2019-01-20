####################################################################################
# Name: WNC Threshold-Based Watershed Delineation
# Coder: C. Nathan Jones
# Date: 20 Jan 2019
# Purpose: Create function to delineate watershed based on area thresholds
####################################################################################


####################################################################################
# Step 1: Setup Worskspace ---------------------------------------------------------
####################################################################################
# Clear Memory  
rm(list=ls(all=TRUE))

# Load packages 
library(sf)         # for spatial analysis
library(raster)     # for spatial analysis
library(tidyverse)  # for data wrangling

#define relevant directories
wbt_dir<-     "C:\\WBT/whitebox_tools"
scratch_dir<- "C:\\ScratchWorkspace/"
data_dir<-    "//storage.research.sesync.org/njones-data/Research Projects/WNC_watershed/spatial_data/RawData/"
output_dir<-   "//storage.research.sesync.org/njones-data/Research Projects/WNC_watershed/spatial_data/DerivedData/"

#Download data
dem<-raster(paste0(data_dir,"WS2_DEM1.tif"))
watershed<-raster(paste0(output_dir, "watershed.tif"))

#Crop DEM to watershed
dem<-dem*watershed
dem<-crop(dem, extent(c(1353165,1357325,961094,967555.7)))

#Plot
plot(dem)

####################################################################################
# Step 2: DEM Preprocessing ---------------------------------------------------------
####################################################################################
#Export DEM and stream layer to local working directory
writeRaster(dem, 
            paste0(scratch_dir,"dem.tif"), 
            overwrite=T)

#Gaussian Filter
system(paste(paste(wbt_dir), 
             "-r=GaussianFilter", 
             paste0("--wd=",scratch_dir),
             "-i='dem.tif'", 
             "-o='dem_filter.tif'",
             "--sigma=3"))

#Fill "single cell" depressions
system(paste(paste(wbt_dir),
             "-r=FillSingleCellPits",
             paste0("--wd=",scratch_dir),
             "--dem='dem_filter.tif'",
             "-o='dem_breach_minor.tif'"))

#Breach larger depressions
system(paste(paste(wbt_dir), 
             "-r=BreachDepressions", 
             paste0("--wd=",scratch_dir),
             "--dem='dem_breach_minor.tif'", 
             "-o='dem_breach_major.tif'"))

#Conduct flow accumulation and flow direction analysis
system(paste(paste(wbt_dir), 
             "-r=D8FlowAccumulation", 
             "--out_type='cells'",
             paste0("--wd=",scratch_dir),
             "--dem='dem_breach_major.tif'", 
             "-o='fac.tif'"))
system(paste(paste(wbt_dir), 
             "-r=D8Pointer", 
             paste0("--wd=",scratch_dir),
             "--dem='dem_breach_major.tif'", 
             "-o='fdr.tif'",
             "--out_type=sca"))

#Retreive fac and fdr rasters for subsequent steps
fac<-raster(paste0(scratch_dir,"fac.tif"))
  fac@crs<-dem@crs
fdr<-raster(paste0(scratch_dir,"fdr.tif"))
  fdr@crs<-dem@crs

####################################################################################
# Step 3: Creat delineation function------------------------------------------------
####################################################################################
threshold_fun<-function(fac,         #flow accumulation raster
                        fdr,         #flow direction raster
                        threshold,   #threshold for watersheds [in map units]
                        scratch_dir,
                        output_dir){ #where to store the output shapes!
  
  #Write fdr to output directory
  writeRaster(fdr,
              paste0(scratch_dir, "fdr2.tif"),
              overwrite=T)
  
  #For testing
  threshold<-cellStats(watershed, sum)*0.05*3.25^2
  
  #Define pour points with threshold flow accumulation value
  #Estimate number of cells for threshold
  threshold<-threshold/res(fac)[1]/res(fac)[2]
  fac[fac==threshold]
  
  
  #threshold appraoch isn't going to work. new approach
  #Define fac value ==0 as points
  #Trace flow paths
  #find values along each flow path that are just pass threshold
  #delineate watershed
  #Clip points from that watershed [so we don't double deleniate]
  #there may have to be some control [like the watershed has to be within 5% of the threshold on either side]
  
  
  
  
}