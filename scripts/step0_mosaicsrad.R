library(raster)
library(sf)
library(rgdal)
library(gdalUtils)
library(tidyverse)
library(snowfall)
library(parallel)

setwd("/glade/scratch/kjfuller/data")
codes = make_EPSG()
c = codes %>% filter(code == 4326) %>% dplyr::select(prj4)

dates = c("0115", "0214", "0315", "0415", "0515", "0615", "0715", "0815", "0915", "1015", "1115", "1215")

sradfun = function(k){
rasters1 = list.files("./SRADTot_NSW", pattern = paste0(k, ".flt"), recursive = TRUE, full.names = TRUE)

rast.list = list()
for(i in c(1:length(rasters1))){
rast.list[i] = raster(rasters1[i], crs = paste(c))
rast.list$fun = mean
rast.mosaic = do.call(mosaic, rast.list)
writeRaster(rast.mosaic, paste0("dem_SRADTot_", k, "_30m.tif"), overwrite = TRUE)
	}
}

sfInit(parallel = TRUE, cpus = 12)
sfExport("c", "dates", "sradfun")
sfLibrary(raster)
sfLibrary(sf)
sfLibrary(gdalUtils)

sfLapply(dates, sradfun)

sfStop()
