## note: one layer at one depth took 1238.34 seconds (~21 min) to run

library(raster)
library(tmap)
library(slga)
library(sf)
library(snowfall)
library(parallel)
options(stringsAsFactors = FALSE)

# load extent shapefile
nsw = st_read("NSW_sans_islands.shp") %>% 
  st_transform(crs = 4283)

fuels = raster("RFS Fuel Type/for_fuels_30m.tif")

# 6-depth function ####
varfun <- function(var) {
  x = list(get_soils_data(product = 'NAT', attribute = var, component = 'VAL', depth = 1, aoi = extent(nsw), write_out = FALSE),
           get_soils_data(product = 'NAT', attribute = var, component = 'VAL', depth = 2, aoi = extent(nsw), write_out = FALSE),
           get_soils_data(product = 'NAT', attribute = var, component = 'VAL', depth = 3, aoi = extent(nsw), write_out = FALSE),
           get_soils_data(product = 'NAT', attribute = var, component = 'VAL', depth = 4, aoi = extent(nsw), write_out = FALSE),
           get_soils_data(product = 'NAT', attribute = var, component = 'VAL',  depth = 5, aoi = extent(nsw), write_out = FALSE),
           get_soils_data(product = 'NAT', attribute = var, component = 'VAL', depth = 6, aoi = extent(nsw), write_out = FALSE))
  names(x[[1]]) = paste(x, "depth 0-5cm", sep = " ")
  names(x[[2]]) = paste(x, "depth 5-15cm", sep = " ")
  names(x[[3]]) = paste(x, "depth 15-30cm", sep = " ")
  names(x[[4]]) = paste(x, "depth 30-60cm", sep = " ")
  names(x[[5]]) = paste(x, "depth 60-100cm", sep = " ")
  names(x[[6]]) = paste(x, "depth 100-200cm", sep = " ")
  r = raster::stack(x)
  projectRaster(r, fuels, method = 'bilinear')
  writeRaster(r, paste("data/soil",var,"_all_30m.grd", sep = ""), format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
}

# start snowfall
sfInit(parallel = TRUE, cpus = 10)
sfExport("nsw", "fuels", "varfun")
sfLibrary(slga)
sfLibrary(raster)

# apply function in parallel
sfLapply(c("BDW", "SOC", "CLY", "SLT", "SND", "PHC", "AWC", "NTO", "PTO", "ECE"), varfun)

# stop snowfall
sfStop()

# 1-depth function ####
var2fun <- function(var) {
  x = get_soils_data(product = 'NAT', attribute = var, component = 'VAL', depth = 1, aoi = extent(nsw), write_out = FALSE)
  projectRaster(x, fuels, method = 'bilinear')
  writeRaster(x, paste("data/soil",var,"_1depth_80m.grd", sep = ""), format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
}

# start snowfall
sfInit(parallel = TRUE, cpus = 2)
sfExport("nsw", "fuels", "var2fun")
sfLibrary(slga)
sfLibrary(raster)

# apply function in parallel
sfLapply(c("DER", "DES"), var2fun)

# stop snowfall
sfStop()
