# script for generating terrain variables from DEM-H
library(raster)
library(sf)
library(tmap)
library(tidyverse)
library(insol)
library(meteo)
library(GSIF)
library(rgdal)

# DEM-H dataset
dem = raster("data/DEM_nsw.tif")

# calculate slope
# dem = slope(cgrad(dem),degrees=TRUE)
## ^vector too large

# break raster into tiles for processing
obj <- GDALinfo("data/DEM_nsw.tif")
tile.tbl = GSIF::getSpatialTiles(obj, block.x = 1, overlap.percent = 5, return.SpatialPolygons = FALSE)
tile.tbl$ID = as.character(1:nrow(tile.tbl))

# demslopes = slope(cgrad(dem),degrees=TRUE)
## ^this needs an actual slice of the dem to run. tile.tbl is just an index atm

# tiles = tiling(dem,tilesize=2500,overlapping=25,aspoints=FALSE)
# 
# # calculate slope
# demslope = slope(cgrad(tiles[[1]]),degrees=TRUE)
# demslope = raster(demslope, crs = crs(dem))
