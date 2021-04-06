# record of code run on Cheyenne
library(raster)
library(sf)
library(tmap)
library(gdalUtils)
library(snowfall)
library(parallel)
library(tidyverse)

setwd("/glade/scratch/kjfuller")

# load datasets
veg = raster("RFS Fuel Type/fuels_30m.tif")

# all resampled datasets
proj = list.files("./data", pattern = "^proj", recursive = FALSE, full.names = TRUE)
p = raster(proj[1])
for(i in c(2:length(proj))){
  x = raster(proj[i])
  p = raster::stack(p, x)
}

# species data
records = read.csv("data/species_sampleV.1_P-A_wide.csv", colClasses = c(rep(NA, 2), rep("NULL", 179))) %>% 
  st_as_sf(coords = c(lon = "lon", lat = "lat"), crs = st_crs(veg))

# extract values and cbind to records df
records = cbind(records, 
                fueltype = raster::extract(veg, st_coordinates(records), methods = 'simple'),
                raster::extract(all, st_coordinates(records), methods = 'simple'))

# # correct bioclim temperature units
# temps = bioclim[[c('bio1', 'bio2', 'bio5', 'bio6', 'bio7', 'bio8', 'bio9', 'bio10', 'bio11')]]/10
# bioclim = raster::stack(temps, bioclim[[c(names(bioclim)[c(3,4,12:19)])]])

# convert to df and write to disk
records$lon = st_coordinates(records)[,1]
records$lat = st_coordinates(records)[,2]
st_geometry(records) = NULL
write.csv(records, "data/dataextract_allsites_30m.csv", row.names = FALSE)
