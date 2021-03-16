# record of code run on Cheyenne
library(raster)
library(sf)
library(tmap)
library(gdalUtils)
library(snowfall)
library(parallel)
library(tidyverse)

# load datasets
veg = raster("data/for_fuels_30m.tif")
nsw = st_read("data/nsw_sans_islands.shp")
bioclim = raster::stack("data/bioclim_cropped.grd")
fire = raster("data/firehistory.tif")
dems = raster("data/dem_s.tif")
arid = raster("data/ai_et0.tif")
pet = raster("data/et0_yr.tif")

# stack all aridity-related layers
petall = raster::stack(raster("data/et0_01.tif"), raster("data/et0_02.tif"), raster("data/et0_03.tif"), raster("data/et0_04.tif"), raster("data/et0_05.tif"), raster("data/et0_06.tif"), raster("data/et0_07.tif"), raster("data/et0_08.tif"), raster("data/et0_09.tif"), raster("data/et0_10.tif"), raster("data/et0_11.tif"), raster("data/et0_12.tif"))
ai.pet = raster::stack(arid, pet, petall)

# crop aridity layers to NSW extent
nsw1 = nsw %>% 
  st_transform(crs = crs(ai.pet))
ai.pet = crop(arid, extent(nsw1))
# correct aridity units
ai.pet[[1]] = ai.pet[[1]]/10000

# correct bioclim temperature units
temps = bioclim[[c('bio1', 'bio2', 'bio5', 'bio6', 'bio7', 'bio8', 'bio9', 'bio10', 'bio11')]]/10
bioclim = raster::stack(temps, bioclim[[c(names(bioclim)[c(3,4,12:19)])]])

# reproject fire layer
fire1 = projectRaster(fire, veg, method = 'ngb')
writeRaster(fire, "data/fire_final.tif", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
rm(fire, fire1)