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

slope_function <- function(m.DLSM, i, tile.tbl, dem_tif = "./data/DEM_nsw.tif", out.path="./data/DEM/tiles/"){
  out.tif = paste0(out.path, "T_", tile.tbl[i,"ID"], ".tif")
  if(!file.exists(out.tif)){
    newdata <- readGDAL(dem_tif, offset=unlist(tile.tbl[i,c("offset.y","offset.x")]),
                        region.dim=unlist(tile.tbl[i,c("region.dim.y","region.dim.x")]),
                        output.dim=unlist(tile.tbl[i,c("region.dim.y","region.dim.x")]),
                        silent = TRUE)
    names(newdata) <- "altitude"
    newdata = raster(newdata)
    dem_slopes <- raster(slope(cgrad(newdata), degree = TRUE), crs = projection(newdata))
    extent(dem_slopes) = extent(newdata)
    writeRaster(dem_slopes, out.tif)
  }
}



demslopes = slope(cgrad(dem),degrees=TRUE)
## ^this needs an actual slice of the dem to run. tile.tbl is just an index atm

# tiles = tiling(dem,tilesize=2500,overlapping=25,aspoints=FALSE)
# 
# # calculate slope
# demslope = slope(cgrad(tiles[[1]]),degrees=TRUE)
# demslope = raster(demslope, crs = crs(dem))
