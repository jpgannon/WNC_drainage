####################################################################################
# Name: WNC Watershed Delineation
# Coder: C. Nathan Jones
# Date: 20 Jan 2019
# Purpose: Demonstrate watershed delineation technique using WBT  
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
wbt_dir<-     "/Users/jpgannon/Desktop/WBT/whitebox_tools"
scratch_dir<- "/Users/jpgannon/Desktop/WRRI_Proj/Scratch/"
data_dir<-    "/Users/jpgannon/Desktop/WRRI_Proj/WNC_drainage/DEMS4drainanal/"
output_dir<-   "/Users/jpgannon/Desktop/WRRI_Proj/WNC_drainage/DEMS4drainanal/DerivedData/"
  
#Download data
dem<-raster(paste0(data_dir,"WS2_DEM1.tif"))
pp<-tibble(lat = 36.371671, long= -81.193860)
pp<-st_as_sf(pp, coords=c("long","lat"), crs=4326)
pp<-st_transform(pp, crs=paste0(dem@crs))
pp$UID<-1

#Plot for funzies
plot(dem)
plot(st_geometry(pp), add=T, pch=19, cex=0.5)

####################################################################################
# Step 2: Preprocess DEM------------------------------------------------------------
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

####################################################################################
# Step 3: Define stream network-----------------------------------------------------
####################################################################################
#Create Flow Accumulation Raster
system(paste(paste(wbt_dir), 
             "-r=D8FlowAccumulation", 
             "--out_type='cells'",
             paste0("--wd=",scratch_dir),
             "--dem='dem_breach_major.tif'", 
             "-o='fac.tif'"))

#Define stream network based on fac threshold
flowgrid<-raster(paste0(scratch_dir,"fac.tif"))
flowgrid[flowgrid<5e5]<-NA
flowgrid<-flowgrid*0+1
flowgrid@crs<-dem@crs
writeRaster(flowgrid,paste0(scratch_dir,"flowgrid.tiff"), overwrite=T)

#Convert to polyline
flownet<-rasterToPolygons(flowgrid, dissolve=T)
flownet<-st_as_sf(flownet)

#Export flownet to output
st_write(flownet, paste0(output_dir,"flownet.shp"))

####################################################################################
# Step 4: Delineate Watershed-------------------------------------------------------
####################################################################################
#Export pour point to scratch directory 
st_write(pp, paste0(scratch_dir,"pnts.shp"), delete_layer = T)


#Run flow direction [note we can skip breaching and/or filling sinks b/c we are using NHD data
system(paste(paste(wbt_dir), 
             "-r=D8Pointer", 
             paste0("--wd=",scratch_dir),
             "--dem='dem_breach_major.tif'", 
             "-o='fdr.tif'",
             "--out_type=sca"))

#Create pour pnt raster
system(paste(paste(wbt_dir), 
             "-r=VectorPointsToRaster", 
             paste0("--wd=",scratch_dir),
             "-i='pnts.shp'", 
             "--field=UID",
             "-o=pp.tif",
             "--assign=min",
             "--nodata",
             "--base=dem.tif"))

#Jenson Snap Pour point
system(paste(paste(wbt_dir),
             "-r=JensonSnapPourPoints", 
             paste0("--wd=",scratch_dir),
             "--pour_pts='pp.tif'", 
             "--streams='flowgrid.tif'",
             "-o='pp_snap.tif'",
             "--snap_dist=1000"))

#Convert back to point file
snapgrid<-raster(paste0(scratch_dir,"pp_snap.tif"))
snappnts<-data.frame(rasterToPoints(snapgrid, fun=function(x){x>0}))
colnames(snappnts)<-c('x','y','UID')
snappnts$watershedID<-seq(1, length(snappnts[,1]))
snappnts<-st_as_sf(snappnts, 
                   coords=c("x","y"), 
                   crs=paste(dem@crs))
st_write(snappnts, paste0(scratch_dir,"snap.shp"), delete_layer = T)

#Delineate watershed
system(paste(paste(wbt_dir),
             "-r=Watershed", 
             paste0("--wd=",scratch_dir),
             "--d8_pntr='fdr.tif'", 
             "--pour_pts='snap.shp'",
             "-o='watershed.tif"))

#Load watershed raster into R enviornment
ws_grd<-raster(paste0(scratch_dir,"watershed.tif"))

#write to export
writeRaster(ws_grd, paste0(output_dir,"watershed.tif"), overwrite=T)
