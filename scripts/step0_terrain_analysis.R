library(raster)
library(sf)
library(tidyverse)
library(gdalUtils)
library(snowfall)

# load DEM-H
dem = raster("data/dem_s.tif")

# calculate terrain variables function
terr = function(x){
  terrvar = terrain(dem, opt = x, unit = 'degrees')
  writeRaster(terrvar, paste0("data/dem_", x, "_30m.grd"), format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
}

vars = c("slope", "aspect", "TPI", "TRI", "roughness")

sfInit(parallel = TRUE, cpus = 5)
sfExport("dem", "terr", "vars")
sfLibrary(raster)

sfLapply(vars, terr)

sfStop()