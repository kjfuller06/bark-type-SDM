# this script is for reprojecting rasters to all be the same EPSG for modeling
## Need to download higher resolution BioClim data and reproject again
# 1) load datasets
# 2) reproject NSW shapefile to each of the raster layers' EPSG and clip to make reprojection faster
# 3) resample rasters to crs and res of veg layer, write to disk and remove from memory storage
#       - method = biliear for continuous data; weighted average of the four nearest cells
#       - method = ngb for categorical data; value of the nearest cell

library(raster)
library(sf)
library(tmap)

# 1) ####
# fuel layer
veg = raster("data/FuelTypeV2_FuelLUT1.tif")
# NSW boundary
nsw = st_read("data/NSW_sans_islands.shp")
# WorldClim datasets
bioclim = mosaic(raster("data/wc0.5/bio1_310.bil"),
                 raster("data/wc0.5/bio1_311.bil"),
                 raster("data/wc0.5/bio1_410.bil"),
                 raster("data/wc0.5/bio1_411.bil"), fun = mean)
names(bioclim)[1] = "bio1"
for(i in c(2:19)){
  x = mosaic(raster(paste("data/wc0.5/bio", i, "_310.bil", sep = "")),
             raster(paste("data/wc0.5/bio", i, "_311.bil", sep = "")),
             raster(paste("data/wc0.5/bio", i, "_410.bil", sep = "")),
             raster(paste("data/wc0.5/bio", i, "_411.bil", sep = "")), fun = mean)
  bioclim = raster::stack(bioclim, x)
  names(bioclim)[i] = paste0("bio", i, sep = "")
}
crs(bioclim) = st_crs(4326)$proj4string
## ^check bioclim's crs online. I'm on the plane. I used some notes from the layer_check script to assign this.

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

# terrain layers
slope = raster("data/dem_slope_30m.grd")
aspect = raster("data/dem_aspect_30m.grd")
TPI = raster("data/dem_TPI_30m.grd")
TRI = raster("data/dem_TRI_30m.grd")
roughness = raster("data/dem_roughness_30m.grd")
terrain = raster::stack(slope, aspect, TPI, TRI, roughness)
rm(slope, aspect, TPI, TRI, roughness)

# 2) ####
# veg layer, fire layer and soil layers don't need cropping because the extent is already equal to NSW
# veg layer is 30m2 res
# terrain layers are 30m2 res
# fire layer is ~80m res
# soil layers are 250m res
# WorldClim layers are 800m res

# terrain layers - ~30m res
nsw1 = nsw %>% 
  st_transform(crs = st_crs(terrain))
terrain = crop(terrain, extent(nsw1))

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
writeRaster(fire, "data/fire_reproj_80m.tif", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)

# the terrain layers are continuous; method is 'bilinear'
terrain = projectRaster(terrain, fire, method = 'bilinear')
writeRaster(terrain, "data/terrain1_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
rm(terrain)

# WorldClim data are continuous; method is 'bilinear'
bioclim = projectRaster(bioclim, fire, method = 'bilinear')
# correct temperature for bioclim temp-based variables
temps = bioclim[[c('bio1', 'bio2', 'bio5', 'bio6', 'bio7', 'bio8', 'bio9', 'bio10', 'bio11')]]/10
bio_other = bioclim[[c('bio3', 'bio4', 'bio12', 'bio13', 'bio14', 'bio15', 'bio16', 'bio17', 'bio18', 'bio19')]]
bioclim = raster::stack(temps, bio_other)
rm(temps, bio_other)
# write to disk and remove from memory
writeRaster(bioclim, "data/bioclim_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
rm(bioclim)

# aridity layer is continuous; method = 'bilinear'
arid = projectRaster(arid, fire, method = 'bilinear')
writeRaster(arid, "data/aridity_80m.tif", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
rm(arid)

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
soils = raster::stack(bdod, cec, cfvo, sand, silt, clay, nit, ph, soc, ocd)
rm(bdod, cec, cfvo, sand, silt, clay, nit, ph, soc, ocd)
writeRaster(soils, "data/soils_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)

## pH is acting weird
writeRaster(ph, "data/soils_pHonly_80m.tif", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
