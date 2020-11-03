# this script is for examining fuel complexes of NSW 
library(raster)
library(tidyverse)
library(sf)
library(rgdal)
library(tmap)

# get codes
codes = make_EPSG()

# load vegetation data
veg = raster("data/FuelTypeV2_FuelLUT1.tif")
# look for crs code in "codes"
lcc = codes[grep("proj=lcc", codes$prj4),]
lcc = lcc[grep("ellps=GRS80", lcc$prj4),]
lcc = lcc[grep("+lat_0=-33.25", lcc$prj4),]
# EPSG is either 3308 for GDA94 / NSW Lambert or 8058 for GDA2020 / NSW Lambert
## use 3308 as this is a government data source so likely isn't up to date

# have to figure out the crs for this file
records = st_read("data/spp_selection_forLDA.shp") %>% 
  st_transform(crs = 3308)

tmap_mode("view")
tm_shape(veg)+tm_raster()+tm_shape(records)+tm_dots(col = "spp_shr")

# bind veg values to records df
records = cbind(records, fuel = raster::extract(veg, st_coordinates(records), methods = 'simple'))
