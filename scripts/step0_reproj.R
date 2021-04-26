library(raster)
library(sf)
library(rgdal)
library(gdalUtils)
library(snowfall)
library(parallel)

veg = raster("data/fuels_30m.tif")
layers = list.files("data", ".tif")

reproj = function(x){
  gdalwarp(srcfile = paste0(layers[x]), 
           dstfile = paste0("proj_", layers[x]), 
           t_srs = crs(veg), 
           tr = res(veg), 
           r = 'bilinear',
           cl = "data/veg_extent.shp",
           crop_to_cutline = TRUE,
           te = extent(veg),
           co = c("BIGTIFF=YES", "COMPRESS=DEFLATE"),
           wo = "NUM_THREADS=ALL_CPUS",
           overwrite = TRUE)
}


sfInit(parallel = TRUE, cpus = 32)
sfExport("veg", "layers", "reproj")
sfLibrary(raster)
sfLibrary(sf)
sfLibrary(gdalUtils)

sfLapply(layers, reproj)

sfStop()