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
bio1 = raster('data/wc2.1_30s_bio/wc2.1_30s_bio_1.tif')
bio2 = raster('data/wc2.1_30s_bio/wc2.1_30s_bio_2.tif')
bio3 = raster('data/wc2.1_30s_bio/wc2.1_30s_bio_3.tif')
bio4 = raster('data/wc2.1_30s_bio/wc2.1_30s_bio_4.tif')
bio5 = raster('data/wc2.1_30s_bio/wc2.1_30s_bio_5.tif')
bio6 = raster('data/wc2.1_30s_bio/wc2.1_30s_bio_6.tif')
bio7 = raster('data/wc2.1_30s_bio/wc2.1_30s_bio_7.tif')
bio8 = raster('data/wc2.1_30s_bio/wc2.1_30s_bio_8.tif')
bio9 = raster('data/wc2.1_30s_bio/wc2.1_30s_bio_9.tif')
bio10 = raster('data/wc2.1_30s_bio/wc2.1_30s_bio_10.tif')
bio11 = raster('data/wc2.1_30s_bio/wc2.1_30s_bio_11.tif')
bio12 = raster('data/wc2.1_30s_bio/wc2.1_30s_bio_12.tif')
bio13 = raster('data/wc2.1_30s_bio/wc2.1_30s_bio_13.tif')
bio14 = raster('data/wc2.1_30s_bio/wc2.1_30s_bio_14.tif')
bio15 = raster('data/wc2.1_30s_bio/wc2.1_30s_bio_15.tif')
bio16 = raster('data/wc2.1_30s_bio/wc2.1_30s_bio_16.tif')
bio17 = raster('data/wc2.1_30s_bio/wc2.1_30s_bio_17.tif')
bio18 = raster('data/wc2.1_30s_bio/wc2.1_30s_bio_18.tif')
bio19 = raster('data/wc2.1_30s_bio/wc2.1_30s_bio_19.tif')
bioclim = raster::stack(bio1, bio2, bio3, bio4, bio5, bio6, bio7, bio8, bio9, bio10, bio11, bio12, bio13, bio14, bio15, bio16, bio17, bio18, bio19)

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
# fire layer is ~80m res
# soil layers are 250m res

# WorldClim data - ~800m res
nsw1 = nsw %>% 
  st_transform(crs = st_crs(bioclim))
bioclim = crop(bioclim, extent(nsw1))

# aridity - 800m res
nsw1 = nsw %>% 
  st_transform(crs = st_crs(arid))
arid = crop(arid, extent(nsw1))

# 3) ####
# the veg layer is the crs reference dataset and will only be used for subsetting. No change made
# the fire layer is the res reference dataset; just change the crs
fire = projectRaster(fire, crs = crs(veg), method = 'ngb')

# WorldClim data are continuous; method is 'bilinear'
bioclim = projectRaster(bioclim, fire, method = 'bilinear')
## ^didn't work, ran out of storage

# veg layer is categorical; method is 'ngb'
veg = projectRaster(veg, fire, method = 'ngb')

# aridity layer is continuous; method = 'bilinear'
arid = projectRaster(arid, fire, method = 'bilinear')

# fire layer is categorical (kind of; data are integers); method = 'ngb'

# soils layers are all continuous; method = 'bilinear'
bdod = projectRaster(bdod, fire, method = 'bilinear')
bdod = bdod/100
cec = projectRaster(cec, fire, method = 'bilinear')
cec = cec/10
cfvo = projectRaster(cfvo, fire, method = 'bilinear')
cfvo = cfvo/10
sand = projectRaster(sand, fire, method = 'bilinear')
sand = sand/10
silt = projectRaster(silt, fire, method = 'bilinear')
silt = silt/10
clay = projectRaster(clay, fire, method = 'bilinear')
clay = clay/10
nit = projectRaster(nitrogen, fire, method = 'bilinear')
nit = nit/100
ph = projectRaster(ph, fire, method = 'bilinear')
ph = ph/10
soc = projectRaster(soc, fire, method = 'bilinear')
soc = soc/10
ocd = projectRaster(ocd, fire, method = 'bilinear')
ocd = ocd/10

# 4) ####
# WorldClim data
# stackSave(r2.5.2, "data/worldclim_reproj.stk")
# ^ doesn't work because the new layers aren't saved in disk; no solution- will need to re-download and change the crs again in the working script each time

# veg data
writeRaster(veg, "data/fuels_reproj.tif")

# aridity data
writeRaster(arid, "data/aridity_reproj.tif")

# fire data
writeRaster(fire, "data/fire_reproj.tif", overwrite = TRUE)

# soil data
writeRaster(bdod, "data/bdod_reproj.tif", overwrite = TRUE)
writeRaster(cec, "data/cec_reproj.tif", overwrite = TRUE)
writeRaster(cfvo, "data/cfvo_reproj.tif", overwrite = TRUE)
writeRaster(sand, "data/sand_reproj.tif", overwrite = TRUE)
writeRaster(silt, "data/silt_reproj.tif", overwrite = TRUE)
writeRaster(clay, "data/clay_reproj.tif", overwrite = TRUE)
writeRaster(nit, "data/nitrogen_reproj.tif", overwrite = TRUE)
writeRaster(ph, "data/ph_reproj.tif", overwrite = TRUE)
writeRaster(soc, "data/soc_reproj.tif", overwrite = TRUE)
writeRaster(ocd, "data/ocd_reproj.tif", overwrite = TRUE)

