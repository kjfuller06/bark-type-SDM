# this script is for reprojecting rasters to all be the same EPSG for modeling
# 1) load datasets
# 2) reproject NSW shapefile to each of the raster layers' EPSG and clip to make reprojection faster
# 3) resample rasters to crs and res of veg layer
#       - method = biliear for continuous data; weighted average of the four nearest cells
#       - method = ngb for categorical data; value of the nearest cell

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

# 2) ####
# veg layer doesn't need cropping because its extent is already equal to NSW

# WorldClim data
nsw1 = nsw %>% 
  st_transform(crs = st_crs(r2.5))
r2.5.1 = crop(r2.5, extent(nsw1))

# aridity
nsw1 = nsw %>% 
  st_transform(crs = st_crs(arid))
arid1 = crop(arid, extent(nsw1))

# 3) ####
# aridity layer is continuous; method = 'bilinear'
arid2 = projectRaster(arid1, crs = crs(veg))
arid2 = projectRaster(arid2, veg, method = 'bilinear')

# WorldClim layers
r2.5.2 = projectRaster(r2.5.1, crs = crs(veg))
r2.5.2 = projectRaster(r2.5.2, veg, method = 'bilinear')
