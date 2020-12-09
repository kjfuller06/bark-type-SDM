# this script is for reprojecting rasters to all be the same EPSG for modeling
# 1) load datasets
# 2) reproject NSW shapefile to each of the raster layers' EPSG and clip to make reprojection faster
# 3) resample rasters to crs and res of veg layer
#       - method = biliear for continuous data; weighted average of the four nearest cells
#       - method = ngb for categorical data; value of the nearest cell
# 4) write rasters to disk

library(raster)
library(sf)
library(tmap)

# 1) ####
# fuel layer
veg = raster("data/FuelTypeV2_FuelLUT1.tif")
# WorldClim datasets
r2.5 = getData('worldclim', var = 'bio', res = 2.5, path = "data/")
# aridity data from CGIARCS
arid = raster('data/ai_et0/ai_et0.tif')
# NSW boundary
nsw = st_read("data/NSW_sans_islands.shp")
# fire history
fire = raster("data/firehistory.tif")
# soil layers
bdod = raster("data/bulkdensity.tif")
cec = raster("data/cec.tif")
cfvo = raster("data/coarsefragments.tif")
sand = raster("data/sand.tif")
silt = raster("data/silt.tif")
clay = raster("data/clay.tif")
nitrogen = raster("data/nitrogen.tif")
ph = raster("data/pH.tif")
soc = raster("data/soc.tif")
ocd = raster("data/ocd.tif")

# 2) ####
# veg layer doesn't need cropping because its extent is already equal to NSW
# veg layer is 30m2 res

# WorldClim data - 4km2 res
nsw1 = nsw %>% 
  st_transform(crs = st_crs(r2.5))
r2.5.1 = crop(r2.5, extent(nsw1))

# aridity - 800m res
nsw1 = nsw %>% 
  st_transform(crs = st_crs(arid))
arid1 = crop(arid, extent(nsw1))

# 3) ####
# WorldClim layers are the reference dataset; just change the crs
r2.5.2 = projectRaster(r2.5.1, crs = crs(veg))

# veg layer is categorical; method is 'ngb'
veg2 = projectRaster(veg, r2.5.2, method = 'ngb')

# aridity layer is continuous; method = 'bilinear'
arid2 = projectRaster(arid1, r2.5.2, method = 'bilinear')

# 4) ####
# WorldClim data
# stackSave(r2.5.2, "data/worldclim_reproj.stk")
# ^ doesn't work because the new layers aren't saved in disk; no solution- will need to re-download and change the crs again in the working script each time

# veg data
writeRaster(veg2, "data/fuels_reproj.tif")

# aridity data
writeRaster(arid2, "data/aridity_reproj.tif")
