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
bioclim2 = crop(bioclim, extent(nsw1))

# aridity - 800m res
nsw1 = nsw %>% 
  st_transform(crs = st_crs(arid))
arid1 = crop(arid, extent(nsw1))

# 3) ####
# the veg layer is the crs reference dataset and will only be used for subsetting. No change made
# the fire layer is the res reference dataset; just change the crs
fire2 = projectRaster(fire, crs = crs(veg), method = 'ngb')

# WorldClim data are continuous; method is 'bilinear'
bioclim3 = projectRaster(bioclim2, fire2, method = 'bilinear')
## ^didn't work, ran out of storage

# veg layer is categorical; method is 'ngb'
veg2 = projectRaster(veg, fire2, method = 'ngb')

# aridity layer is continuous; method = 'bilinear'
arid2 = projectRaster(arid1, fire2, method = 'bilinear')

# fire layer is categorical (kind of; data are integers); method = 'ngb'

# soils layers are all continuous; method = 'bilinear'
bdod2 = projectRaster(bdod, fire2, method = 'bilinear')
bdod2 = bdod2/100
cec2 = projectRaster(cec, fire2, method = 'bilinear')
cec2 = cec2/10
cfvo2 = projectRaster(cfvo, fire2, method = 'bilinear')
cfvo2 = cfvo2/10
sand2 = projectRaster(sand, fire2, method = 'bilinear')
sand2 = sand2/10
silt2 = projectRaster(silt, fire2, method = 'bilinear')
silt2 = silt2/10
clay2 = projectRaster(clay, fire2, method = 'bilinear')
clay2 = clay2/10
nit2 = projectRaster(nitrogen, fire2, method = 'bilinear')
nit2 = nit2/100
ph2 = projectRaster(ph, fire2, method = 'bilinear')
ph2 = ph2/10
soc2 = projectRaster(soc, fire2, method = 'bilinear')
soc2 = soc2/10
ocd2 = projectRaster(ocd, fire2, method = 'bilinear')
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

