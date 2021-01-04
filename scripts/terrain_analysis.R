# script for generating terrain variables from DEM-H
library(raster)
library(sf)
library(tmap)
library(tidyverse)
library(insol)
library(meteo)

# NSW polygon
nsw = st_read("data/NSW_sans_islands.shp")
# DEM-H dataset
dem = raster("data/DEM/71498/a05f7893-0050-7506-e044-00144fdd4fa6/hdr.adf")
dem

# crop DEM with NSW extent shapefile
# reproject shapefile
nsw = nsw %>% 
  st_transform(crs = st_crs(dem))
dem = crop(dem, nsw)
tm_shape(dem)+tm_raster()+tm_shape(nsw)+tm_borders()

# calculate slope
# dem = slope(cgrad(dem),degrees=TRUE)
## ^vector too large

# break raster into tiles for processing
tiles = tiling(dem,tilesize=2500,overlapping=25,aspoints=FALSE)

# calculate slope
demslope = slope(cgrad(tiles[[1]]),degrees=TRUE)
demslope = raster(demslope, crs = crs(dem))
