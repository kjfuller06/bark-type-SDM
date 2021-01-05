# script for generating terrain variables from DEM-H
# need to go back and re-crop the DEM so that it doesn't stop directly at the bbox of NSW. As is, the edges won't compute slop accurately because they are missing neighbour cells.
library(raster)
library(sf)
library(tmap)
library(tidyverse)
library(insol)
library(meteo)
library(GSIF)
library(rgdal)
library(snowfall)

# load DEM-H
dem = system.file("data/DEM_nsw.tif", package = "rgdal")

# calculate slope and aspect
dem_slopes = terrain(dem, opt = c('slope', 'aspect'), unit = 'degrees')
# write to disk
writeRaster(dem_slope, "data/dem_slope.aspect_30m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
# writeGDAL(dem_slope, "data/dem_slope.aspect_30m.grd", drivername = "GTiff", type = "Int16", 
#           options="COMPRESS=DEFLATE", copy_drivername = "GTiff")
# system(paste0('gdalwarp',
# dem_slope[[1]],
# '\"./data/dem_slope_30m.tif\"',
# '-ot \"Float32\" -co \"BIGTIFF=YES\"',
# '-wm 2000 -co \"COMPRESS=DEFLATE\" -overwrite -multi',
# '-wo \"NUM_THREADS=ALL_CPUS\"'))
## ^this just spits out a number (127), nothing else happens

# break raster into tiles for processing
# obj <- GDALinfo(dem)
# tile.tbl = GSIF::getSpatialTiles(obj, block.x = 0.1, overlap.percent = 5, return.SpatialPolygons = FALSE)
# tile.tbl$ID = as.character(1:nrow(tile.tbl))
# 
# slope_function <- function(i, tile.tbl, dem, dem_path = "./data/DEM_nsw.tif", out.path="./data/DEM/tiles/"){
#   out.tif = paste0(out.path, "T_", tile.tbl[i,"ID"], ".tif")
#   if(!file.exists(out.tif)){
#     cov.files = paste0(dem_path, dem)
#     newdata <- readGDAL(cov.files, offset=unlist(tile.tbl[i,c("offset.y","offset.x")]),
#                         region.dim=unlist(tile.tbl[i,c("region.dim.y","region.dim.x")]),
#                         output.dim=unlist(tile.tbl[i,c("region.dim.y","region.dim.x")]),
#                         silent = TRUE)
#     names(newdata) <- "altitude"
#     newdata = raster(newdata)
#     dem_slopes <- raster(slope(cgrad(newdata), degree = TRUE), crs = projection(newdata))
#     extent(dem_slopes) = extent(newdata)
#     writeGDAL(dem_slopes, out.tif, drivername = "GTiff", type = "Int16", 
#               options="COMPRESS=DEFLATE", copy_drivername = "GTiff")
#   }
# }
# 
# memory.limit(size = 56000)
# 
# sfInit(parallel = TRUE, cpus = parallel::detectCores())
# sfExport("slope_function", "tile.tbl", "dem")
# sfLibrary(rgdal)
# 
# out.lst <- sfClusterApplyLB(1:nrow(tile.tbl), 
#                              function(x){ slope_function(x, tile.tbl, dem) })
# sfStop()
