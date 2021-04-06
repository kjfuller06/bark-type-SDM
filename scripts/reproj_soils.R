library(raster)
library(sf)
library(rgdal)
library(gdalUtils)
library(snowfall)
library(parallel)

setwd("/glade/scratch/kjfuller")
veg = raster("data/fuels_30m.tif")

# soil data
soils = list.files("./data", pattern = "^soil", recursive = TRUE, full.names = TRUE)

soilfun = function(x){
	k = substr(soils[x], 1, 11)
	gdalwarp(srcfile = soils[x], 
		dstfile = paste0("data/proj_", k, "30m.tif"), 
		t_srs = crs(veg), 
		tr = res(veg), 
		r = 'bilinear',
		cl = "data/veg_extent.shp",
		crop_to_cutline = TRUE,
		co = c("BIGTIFF=YES", "COMPRESS=DEFLATE"),
		wo = "NUM_THREADS=ALL_CPUS",
		overwrite = TRUE)
}

sfInit(parallel = TRUE, cpus = 66)
sfExport("veg", "soils", "soilfun")
sfLibrary(slga)
sfLibrary(raster)
sfLibrary(sf)
sfLibrary(rgdal)
sfLibrary(gdalUtils)

sfLapply(c(1:66), soilfun)

sfStop()
