setwd("Z:/")
library(raster,sp)
library(rgdal)
library(sf)

six_cm_file = "AKSeward/Data/GIS/Seward_Peninsula/ABoVE_Seward/ABoVE_Data/Soil_Moisture/lbc_method/lbc_6cm.tif"
slope_file = "AKSeward/DEM/IFSAR/SAR/IFSAR_DTM_SAR_2017_aug_flightline_slope.tif"
int_slope_file = "AKSeward/Data/GIS/Seward_Peninsula/ABoVE_Seward/IFSAR_DTM/slope_int.tif"
slope_shape_path = "AKSeward/Data/GIS/Seward_Peninsula/ABoVE_Seward/IFSAR_DTM/slope_cat_polys.shp"
slope_shape_file = "slope_cat_polys.shp"
#read in raster files
six_cm <-raster(six_cm_file,band=1)
slope <-raster(slope_file,band=1)
int_slope <-raster(int_slope_file,band=1)
#slope_shapefile<- read_sf(dsn=slope_shape_path,layer=slope_shape_file)
slope_shapefile<- st_read(system.file(slope_shape_path,package="sf"))
slope_shapefile<- st_read(slope_shape_path)
#make a for loop for polygon slope category creation
extract(six_cm,int_slope)