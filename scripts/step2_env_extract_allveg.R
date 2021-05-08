library(raster)
library(snow)
library(parallel)
library(sf)
library(tidyverse)
library(data.table)

veg = raster("data/fuels_30m.tif")
traits = data.table::fread("data/alltraits_site-specific.csv", select = c(1:2, 5:8, 10:11))
names(traits)[c(7:8)] = c("x", "y")
traits = st_as_sf(traits, coords = c("x", "y"), crs = st_crs(veg))

setwd("D:/chapter1/other_data/Final")

beginCluster()
# load datasets
mask = list.files("./", pattern = "^proj", recursive = TRUE, full.names = TRUE)
m = raster(mask[1])
for(i in c(2:length(mask))){
  x = raster(mask[i])
  m = raster::stack(m, x)
}

# extract values and cbind to records sf
records = cbind(traits, 
                raster::extract(m, st_coordinates(traits), methods = 'simple'))
endCluster()

# convert to df and write to disk
records$lon = st_coordinates(records)[,1]
records$lat = st_coordinates(records)[,2]
st_geometry(records) = NULL
data.table::fwrite(records, "allsites_allveg_30m.csv")
