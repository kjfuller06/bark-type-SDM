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
library(gdalUtils)
library(snowfall)
library(RSAGA)

# load DEM-H
dem = raster("data/DEM_nsw.tif")
gda2020 = st_crs(7856)$proj4string
old = st_crs(dem)$proj4string
gdalwarp("data/DEM_nsw.tif", 
         dstfile = "data/DEM_nsw.sgrd",
         t_srs = gda2020,
         output_Raster = FALSE,
         overwrite = TRUE, verbose = TRUE)
# dem = projectRaster(dem, crs = gda2020)
# writeRaster(dem, "data/DEM_nsw.sgrd")
# write.sgrd(dem, "data/DEM_nsw.sgrd")

# calculate slope and aspect
dem_slopes = terrain(dem, opt = c('slope', 'aspect'), unit = 'degrees')
# write to disk
writeRaster(dem_slopes, "data/dem_slope.aspect_30m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)

# continue here
# calculate Topographic Position Index and Terrain Ruggedness Index
dem_terrains = terrain(dem, opt = c('TPI', 'TRI'), unit = 'degrees')
# write to disk
writeRaster(dem_terrains, "data/dem_TPI.TRI_30m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)

# calculate terrain roughness
dem_roughness = terrain(dem, opt = c('roughness'), unit = 'degrees')
# write to disk
writeRaster(dem_roughness, "data/dem_roughness_30m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)

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

# calculate insolation
RSAGA::rsaga.geoprocessor(lib = "ta_lighting",
                          module = 2,
                          param = list("data/DEM_nsw.sgrd",
                                       GRD_DIRECT = "data/insol_dir_30m.sgrd",
                                       GRD_DIFFUS = "data/insol_dif_30m.sgrd",
                                       GRD_TOTAL = "data/insol_tot_30m.sgrd",
                                       LOCATION = 1,
                                       HOUR_STEP = 1,
                                       DAYS_STEP = 10,
                                       PERIOD = 2,
                                       SHADOW = 0,
                                       METHOD = 2,
                                       DAY = "2011-11-01",
                                       DAY_STOP = "2011-11-01"),
                          intern = FALSE,
                          cores = 25)

