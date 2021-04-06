# first batch code attempt
library(raster)
library(sf)
library(tmap)
library(gdalUtils)
library(snowfall)
library(parallel)
library(tidyverse)

setwd("/glade/scratch/kjfuller/data/")

# load datasets
veg = raster("fuels_30m.tif")

# reproject and resample
process = function(x){
  if(x == "firehistory.tif"){
    b = "near"
  } else {
    b = "bilinear"
  }
  gdalwarp(srcfile = x, dstfile = paste0("final", x), t_srs = crs(veg), tr = res(veg), r = b, cl = "NSW_fuelsproj.shp", crop_to_cutline = TRUE, multi = TRUE, co = c("BIGTIFF=TRUE", "COMPRESS=DEFLATE"), wo = "NUM_THREADS=ALL_CPUS", overwrite = TRUE)
}

vars = c("ai.pet.grd", "bioclim_cropped.grd", "dem_s.tif", "firehistory.tif", "NDVI_cropped.grd")

sfInit(parallel = TRUE, cpus = detectCores())
sfExport(process, vars, "veg")
sfLibrary(raster)
sfLibrary(sf)
sfLibrary(gdalUtils)

sfLapply(vars, process)

sfStop()