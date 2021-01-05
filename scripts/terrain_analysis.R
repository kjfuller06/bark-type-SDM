# script for generating terrain variables from DEM-H
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
dem = raster("data/DEM_nsw.tif")

# break raster into tiles for processing
obj <- GDALinfo("data/DEM_nsw.tif")
tile.tbl = GSIF::getSpatialTiles(obj, block.x = 1, overlap.percent = 5, return.SpatialPolygons = FALSE)
tile.tbl$ID = as.character(1:nrow(tile.tbl))

slope_function <- function(i, tile.tbl, dem, dem_path = "./data/DEM_nsw.tif", out.path="./data/DEM/tiles/"){
  out.tif = paste0(out.path, "T_", tile.tbl[i,"ID"], ".tif")
  if(!file.exists(out.tif)){
    cov.files = paste0(dem_path, dem)
    newdata <- readGDAL(cov.files, offset=unlist(tile.tbl[i,c("offset.y","offset.x")]),
                        region.dim=unlist(tile.tbl[i,c("region.dim.y","region.dim.x")]),
                        output.dim=unlist(tile.tbl[i,c("region.dim.y","region.dim.x")]),
                        silent = TRUE)
    names(newdata) <- "altitude"
    newdata = raster(newdata)
    dem_slopes <- raster(slope(cgrad(newdata), degree = TRUE), crs = projection(newdata))
    extent(dem_slopes) = extent(newdata)
    writeGDAL(dem_slopes, out.tif, drivername = "GTiff", type = "Int16", 
              options="COMPRESS=DEFLATE", copy_drivername = "GTiff")
  }
}

sfInit(parallel = TRUE, cpus = parallel::detectCores())
sfExport("slope_function", "tile.tbl", "dem")
sfLibrary(rgdal)

out.lst <- sfClusterApplyLB(1:nrow(tile.tbl), 
                             function(x){ slope_function(x, tile.tbl, dem) })
sfStop()