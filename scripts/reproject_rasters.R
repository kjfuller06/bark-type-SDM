# this script is for reprojecting rasters to all be the same EPSG for modeling
## Need to download higher resolution BioClim data and reproject again
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
# NSW boundary
nsw = st_read("data/NSW_sans_islands.shp")
# WorldClim datasets
r0.5_410 = getData('worldclim', var = 'bio', res = 0.5, lon = 148, lat = -30, path = "data/")
r0.5_411 = getData('worldclim', var = 'bio', res = 0.5, lon = 150, lat = -30, path = "data/")
r0.5_310 = getData('worldclim', var = 'bio', res = 0.5, lon = 147, lat = -25, path = "data/")
r0.5_311 = getData('worldclim', var = 'bio', res = 0.5, lon = 150, lat = -25, path = "data/")

# mosaic the tiles together
r0.5 = mosaic(r0.5_410, r0.5_411, fun = mean)
r0.5 = mosaic(r0.5, r0.5_310, r0.5_311, fun = mean)

save = r0.5


# aridity data from CGIARCS
arid = raster('data/ai_et0/ai_et0.tif')
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
# veg layer, fire layer and soil layers don't need cropping because the extent is already equal to NSW
# veg layer is 30m2 res
# fire layer is ~80m res (I think)
# soil layers are 250m res

# WorldClim data - 4km2 res
nsw1 = nsw %>% 
  st_transform(crs = st_crs(r0.5))
r2.5.1 = crop(r0.5, extent(nsw1))

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

# fire layer is categorical (kind of; data are integers); method = 'ngb'
fire2 = projectRaster(fire, r2.5.2, method = 'ngb')

# soils layers are all continuous; method = 'bilinear'
bdod2 = projectRaster(bdod, r2.5.2, method = 'bilinear')
bdod2 = bdod2/100
cec2 = projectRaster(cec, r2.5.2, method = 'bilinear')
cec2 = cec2/10
cfvo2 = projectRaster(cfvo, r2.5.2, method = 'bilinear')
cfvo2 = cfvo2/10
sand2 = projectRaster(sand, r2.5.2, method = 'bilinear')
sand2 = sand2/10
silt2 = projectRaster(silt, r2.5.2, method = 'bilinear')
silt2 = silt2/10
clay2 = projectRaster(clay, r2.5.2, method = 'bilinear')
clay2 = clay2/10
nit2 = projectRaster(nitrogen, r2.5.2, method = 'bilinear')
nit2 = nit2/100
ph2 = projectRaster(ph, r2.5.2, method = 'bilinear')
ph2 = ph2/10
soc2 = projectRaster(soc, r2.5.2, method = 'bilinear')
soc2 = soc2/10
ocd2 = projectRaster(ocd, r2.5.2, method = 'bilinear')
ocd2 = ocd2/10

# 4) ####
# WorldClim data
# stackSave(r2.5.2, "data/worldclim_reproj.stk")
# ^ doesn't work because the new layers aren't saved in disk; no solution- will need to re-download and change the crs again in the working script each time

# veg data
writeRaster(veg2, "data/fuels_reproj.tif")

# aridity data
writeRaster(arid2, "data/aridity_reproj.tif")

# fire data
writeRaster(fire2, "data/fire_reproj.tif", overwrite = TRUE)

# soil data
writeRaster(bdod2, "data/bdod_reproj.tif", overwrite = TRUE)
writeRaster(cec2, "data/cec_reproj.tif", overwrite = TRUE)
writeRaster(cfvo2, "data/cfvo_reproj.tif", overwrite = TRUE)
writeRaster(sand2, "data/sand_reproj.tif", overwrite = TRUE)
writeRaster(silt2, "data/silt_reproj.tif", overwrite = TRUE)
writeRaster(clay2, "data/clay_reproj.tif", overwrite = TRUE)
writeRaster(nit2, "data/nitrogen_reproj.tif", overwrite = TRUE)
writeRaster(ph2, "data/ph_reproj.tif", overwrite = TRUE)
writeRaster(soc2, "data/soc_reproj.tif", overwrite = TRUE)
writeRaster(ocd2, "data/ocd_reproj.tif", overwrite = TRUE)

