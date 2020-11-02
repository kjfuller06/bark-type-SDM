# this script is for examining fuel complexes of NSW 
library(raster)
library(tidyverse)
library(sf)

# load vegetation data
veg = raster("data/FuelTypeV2_FuelLUT1.tif")
# have to figure out the crs for this file
records = st_read("data/spp_selection_forLDA.shp")
