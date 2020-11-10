library(raster)

# load the reference raster
prsm_precip = raster("data/PRISM data/ppt/PRISM_ppt_30yr_normal_4kmM2_annual_asc.asc")

# # reproject CSGIARCSI and NPN datasets
# aridity = raster('data/CGIARCSI data/ai_et0/ai_et0.tif')
# PET = raster('data/CGIARCSI data/et0_yr/et0_yr.tif')
# agdd = raster('data/NPN data/data.tif')
# 
# # crop these to match the prism datasets
# aridity = crop(aridity, extent(prsm_precip))
# PET = crop(PET, extent(prsm_precip))
# agdd = crop(agdd, extent(prsm_precip))
# 
# # reproject CSGIARCSI datasets
# aridity = projectRaster(aridity, crs = "+proj=longlat +datum=NAD83 +no_defs", method = 'bilinear')
# PET = projectRaster(PET, crs = "+proj=longlat +datum=NAD83 +no_defs", method = 'bilinear')
# 
# # write these to disk so I don't have to do it again
# writeRaster(aridity, 'data/CGIARCSI data/ai_et0/at_et0_reprojected.tif', overwrite = TRUE)
# writeRaster(PET, 'data/CGIARCSI data/et0_yr/et0_yr_reprojected.tif', overwrite = TRUE)

# reload datasets
aridity = raster('data/CGIARCSI data/ai_et0/at_et0_reprojected.tif')
PET = raster('data/CGIARCSI data/et0_yr/et0_yr_reprojected.tif')

# resample to same extent and dimensions
aridity = aggregate(aridity, fact = 5)
PET = aggregate(PET, fact = 5)
aridity <- resample(aridity, prsm_precip, method = 'bilinear')
PET <- resample(PET, prsm_precip, method = 'bilinear')
agdd = resample(agdd, prsm_precip, method = 'bilinear')

# write to disk
writeRaster(aridity, 'data/CGIARCSI data/ai_et0/at_et0_cropped.tif', overwrite = TRUE)
writeRaster(PET, 'data/CGIARCSI data/et0_yr/et0_yr_cropped.tif', overwrite = TRUE)
writeRaster(agdd, 'data/NPN data/agdd_reprojected.tif', overwrite = TRUE)

# same process for monthly PET
for(i in sprintf('%0.2d', 1:12)){
  PET1 = raster(paste('data/CGIARCSI data/et0_month/et0_', i, '.tif', sep = ""))
  PET1 = crop(PET1, extent(prsm_precip))
  PET1 = projectRaster(PET1, crs = "+proj=longlat +datum=NAD83 +no_defs", method = 'bilinear')
  PET1 = aggregate(PET1, fact = 5)
  PET1 <- resample(PET1, prsm_precip, method = 'bilinear')
  writeRaster(PET1, paste('data/CGIARCSI data/et0_month/et0_', i, '_projected.tif', sep = ""), overwrite = TRUE)
}
