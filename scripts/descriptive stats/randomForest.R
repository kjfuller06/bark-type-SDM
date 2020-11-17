library(raster)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(tmap)
library(randomForest)

# testing random forest ####
veg = raster("data/fuels_reproj.tif")
records = st_read("data/spp_selection_P-A.shp") %>% 
  st_as_sf(crs = st_crs(veg))


