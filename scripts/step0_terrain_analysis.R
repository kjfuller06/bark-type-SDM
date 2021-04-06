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

#----------- on Cheyenne ---------------
library(raster)
library(tidyverse)
library(rgdal)
library(gdalUtils)
library(dynatopmodel)
library(sf)

codes = make_EPSG()
demcrs = codes %>% 
  filter(code = 7859) %>% 
  dplyr::select(prj4)
dem = raster("data/dem_s.tif")
gdalwarp(srcfile = "data/dem_s.tif", dstfile = "data/dem_crs7859.tif", t_srs = paste(demcrs), output_Raster = TRUE, overwrite = TRUE, verbose = TRUE, multi = TRUE, co = c("BIGTIFF=TRUE", "COMPRESS=DEFLATE"), wo = "NUM_THREADS=ALL_CPUS", r = 'bilinear')
## this probably work fine. I just thought it was producing errors because head(dem) returned all NANs but that's accurate in the new projection

#-------- ran this instead ----------
library(raster)
library(tidyverse)
library(rgdal)
library(gdalUtils)
library(sf)

codes = make_EPSG()
demcrs = codes %>% filter(code == 7858) %>% dplyr::select(prj4)
dem = raster("data/dem_s.tif")
dem = projectRaster(dem, crs = paste(demcrs), method = 'bilinear')
writeRaster(dem_crs, "data/dem_projected.tif")

#----------------- ran on laptop because it has SAGA, tiled to conserve memory ---------------------
library(raster)
library(tidyverse)
library(RSAGA)
library(rgdal)
library(gdalUtils)
library(FRK)

setwd("D:/chapter1")
codes = make_EPSG()
c = codes %>% 
  filter(code == 7858) %>% 
  dplyr::select(prj4)
nsw = st_read("NSW_sans_islands.shp") %>% 
  st_transform(crs = 7858)
minx = extent(nsw)[1]
maxx = extent(nsw)[2]
miny = extent(nsw)[3]
maxy = extent(nsw)[4]

halfx = (maxx - minx)/2 + minx
halfy = (maxy - miny)/2 + miny
tiles = data.frame(id = c(rep(1, 5), 
                          rep(2, 5), 
                          rep(3, 5), 
                          rep(4, 5)), 
                   x = c(c(minx, halfx, halfx, minx, minx),
                         c(halfx, maxx, maxx, halfx, halfx),
                         c(minx, halfx, halfx, minx, minx),
                         c(halfx, maxx, maxx, halfx, halfx)),
                   y = c(c(miny, miny, halfy, halfy, miny),
                         c(miny, miny, halfy, halfy, miny),
                         c(halfy, halfy, maxy, maxy, halfy),
                         c(halfy, halfy, maxy, maxy, halfy)))
polys = df_to_SpatialPolygons(tiles, keys = "id", coords = c("x", "y"), proj = CRS(paste(c))) %>% 
  st_as_sf()
tile1 = polys[1,]
st_write(tile1, "processed datasets/tile1.shp")
tile2 = polys[2,]
st_write(polys[2,], "processed datasets/tile2.shp")
tile3 = polys[3,]
tile4 = polys[4,]

env = rsaga.env()
gdalwarp(srcfile = "processed datasets/dem_projected.tif",
         dstfile = "processed datasets/dem_proj_tile1.tif",
         cl = "processed datasets/tile1.shp", 
         crop_to_cutline = TRUE,
         te = extent(tile1),
         co = "BIGTIFF=YES",
         output_Raster = FALSE,
         overwrite = TRUE, verbose = TRUE)

rsaga.import.gdal(in.grid = "processed datasets/dem_proj_tile1.tif", out.grid = "processed datasets/dem_proj_tile1.sgrd", env = env)

memory.limit(size = 8213552)
rsaga.wetness.index(in.dem = "processed datasets/dem_s.sgrd", out.wetness.index = "processed datasets/dem_wetness_30m.sgrd", env = env)
# run SAGA function
RSAGA::rsaga.geoprocessor(lib = "ta_hydrology",
                          module = 15,
                          param = list(DEM = "processed datasets/dem_s.sgrd"),
                          intern = FALSE,
                          cores = 8)

#---------- another TWI option: dynatopmodel -----------
library(raster)
library(tidyverse)
library(rgdal)
library(gdalUtils)
library(dynatopmodel)
library(sf)

setwd("D:/chapter1")
dem = raster("processed datasets/dem_projected.tif")
dem = projectRaster(dem, crs = paste(demcrs), method = 'bilinear')
dem_twi = upslope.area(dem, atb = TRUE)
