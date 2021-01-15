library(raster)
library(tmap)
library(slga)
library(sf)
options(stringsAsFactors = FALSE)

# load extent shapefile
nsw = st_read("data/NSW_sans_islands.shp") %>% 
  st_transform(crs = 4283)

# soil bulk density- whole earth ####
doParallel::registerDoParallel()
bdw <- lapply(seq.int(6), function(d) {
  get_soils_data(product = 'NAT', attribute = 'BDW', component = 'VAL',
                 depth = d, aoi = extent(nsw), write_out = FALSE)
})

names(bdw[[1]]) = "BDW depth 0-5cm"
names(bdw[[2]]) = "BDW depth 5-15cm"
names(bdw[[3]]) = "BDW depth 15-30cm"
names(bdw[[4]]) = "BDW depth 30-60cm"
names(bdw[[5]]) = "BDW depth 60-100cm"
names(bdw[[6]]) = "BDW depth 100-200cm"

writeRaster(bdw, "data/soilbdw_all_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)

# soil organic carbon ####
doParallel::registerDoParallel()
soc <- lapply(seq.int(6), function(d) {
  get_soils_data(product = 'NAT', attribute = 'SOC', component = 'VAL',
                 depth = d, aoi = extent(nsw), write_out = FALSE)
})

names(soc[[1]]) = "SOC depth 0-5cm"
names(soc[[2]]) = "SOC depth 5-15cm"
names(soc[[3]]) = "SOC depth 15-30cm"
names(soc[[4]]) = "SOC depth 30-60cm"
names(soc[[5]]) = "SOC depth 60-100cm"
names(soc[[6]]) = "SOC depth 100-200cm"

writeRaster(soc, "data/soilsoc_all_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
rm(soc)

# soil bulk density- fine earth ####
doParallel::registerDoParallel()
bdf <- lapply(seq.int(6), function(d) {
  get_soils_data(product = 'NAT', attribute = 'BDF', component = 'VAL',
                 depth = d, aoi = extent(nsw), write_out = FALSE)
})

names(bdf[[1]]) = "BDF depth 0-5cm"
names(bdf[[2]]) = "BDF depth 5-15cm"
names(bdf[[3]]) = "BDF depth 15-30cm"
names(bdf[[4]]) = "BDF depth 30-60cm"
names(bdf[[5]]) = "BDF depth 60-100cm"
names(bdf[[6]]) = "BDF depth 100-200cm"

writeRaster(bdf, "data/soilbdf_all_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)

# clay content ####
doParallel::registerDoParallel()
cly <- lapply(seq.int(6), function(d) {
  get_soils_data(product = 'NAT', attribute = 'CLY', component = 'VAL',
                 depth = d, aoi = extent(nsw), write_out = FALSE)
})

names(cly[[1]]) = "CLY depth 0-5cm"
names(cly[[2]]) = "CLY depth 5-15cm"
names(cly[[3]]) = "CLY depth 15-30cm"
names(cly[[4]]) = "CLY depth 30-60cm"
names(cly[[5]]) = "CLY depth 60-100cm"
names(cly[[6]]) = "CLY depth 100-200cm"

writeRaster(cly, "data/soilclay_all_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
# silt content ####
doParallel::registerDoParallel()
slt <- lapply(seq.int(6), function(d) {
  get_soils_data(product = 'NAT', attribute = 'SLT', component = 'VAL',
                 depth = d, aoi = extent(nsw), write_out = FALSE)
})

names(slt[[1]]) = "SLT depth 0-5cm"
names(slt[[2]]) = "SLT depth 5-15cm"
names(slt[[3]]) = "SLT depth 15-30cm"
names(slt[[4]]) = "SLT depth 30-60cm"
names(slt[[5]]) = "SLT depth 60-100cm"
names(slt[[6]]) = "SLT depth 100-200cm"

writeRaster(slt, "data/soilsilt_all_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
# sand content ####
doParallel::registerDoParallel()
snd <- lapply(seq.int(6), function(d) {
  get_soils_data(product = 'NAT', attribute = 'SND', component = 'VAL',
                 depth = d, aoi = extent(nsw), write_out = FALSE)
})

names(snd[[1]]) = "SND depth 0-5cm"
names(snd[[2]]) = "SND depth 5-15cm"
names(snd[[3]]) = "SND depth 15-30cm"
names(snd[[4]]) = "SND depth 30-60cm"
names(snd[[5]]) = "SND depth 60-100cm"
names(snd[[6]]) = "SND depth 100-200cm"

writeRaster(snd, "data/soilsand_all_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
# pH based on CaCl2 extraction ####
doParallel::registerDoParallel()
phc <- lapply(seq.int(6), function(d) {
  get_soils_data(product = 'NAT', attribute = 'PHC', component = 'VAL',
                 depth = d, aoi = extent(nsw), write_out = FALSE)
})

names(phc[[1]]) = "PHC depth 0-5cm"
names(phc[[2]]) = "PHC depth 5-15cm"
names(phc[[3]]) = "PHC depth 15-30cm"
names(phc[[4]]) = "PHC depth 30-60cm"
names(phc[[5]]) = "PHC depth 60-100cm"
names(phc[[6]]) = "PHC depth 100-200cm"

writeRaster(phc, "data/soilpHCaCl2_all_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
# available water capacity ####
doParallel::registerDoParallel()
awc <- lapply(seq.int(6), function(d) {
  get_soils_data(product = 'NAT', attribute = 'AWC', component = 'VAL',
                 depth = d, aoi = extent(nsw), write_out = FALSE)
})

names(awc[[1]]) = "AWC depth 0-5cm"
names(awc[[2]]) = "AWC depth 5-15cm"
names(awc[[3]]) = "AWC depth 15-30cm"
names(awc[[4]]) = "AWC depth 30-60cm"
names(awc[[5]]) = "AWC depth 60-100cm"
names(awc[[6]]) = "AWC depth 100-200cm"

writeRaster(awc, "data/soilAWC_all_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
# total nitrogen ####
doParallel::registerDoParallel()
nit <- lapply(seq.int(6), function(d) {
  get_soils_data(product = 'NAT', attribute = 'NTO', component = 'VAL',
                 depth = d, aoi = extent(nsw), write_out = FALSE)
})

names(nit[[1]]) = "NTO depth 0-5cm"
names(nit[[2]]) = "NTO depth 5-15cm"
names(nit[[3]]) = "NTO depth 15-30cm"
names(nit[[4]]) = "NTO depth 30-60cm"
names(nit[[5]]) = "NTO depth 60-100cm"
names(nit[[6]]) = "NTO depth 100-200cm"

writeRaster(nit, "data/soilnto_all_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
# total phosphorus ####
doParallel::registerDoParallel()
pho <- lapply(seq.int(6), function(d) {
  get_soils_data(product = 'NAT', attribute = 'PTO', component = 'VAL',
                 depth = d, aoi = extent(nsw), write_out = FALSE)
})

names(pho[[1]]) = "PTO depth 0-5cm"
names(pho[[2]]) = "PTO depth 5-15cm"
names(pho[[3]]) = "PTO depth 15-30cm"
names(pho[[4]]) = "PTO depth 30-60cm"
names(pho[[5]]) = "PTO depth 60-100cm"
names(pho[[6]]) = "PTO depth 100-200cm"

writeRaster(pho, "data/soilntp_all_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
# electrical conductivity in 1:5 water-soil solution ####
doParallel::registerDoParallel()
ecd <- lapply(seq.int(6), function(d) {
  get_soils_data(product = 'NAT', attribute = 'ECD', component = 'VAL',
                 depth = d, aoi = extent(nsw), write_out = FALSE)
})

names(ecd[[1]]) = "ECD depth 0-5cm"
names(ecd[[2]]) = "ECD depth 5-15cm"
names(ecd[[3]]) = "ECD depth 15-30cm"
names(ecd[[4]]) = "ECD depth 30-60cm"
names(ecd[[5]]) = "ECD depth 60-100cm"
names(ecd[[6]]) = "ECD depth 100-200cm"

writeRaster(ecd, "data/soilecd_all_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
# CEC ####
doParallel::registerDoParallel()
cec <- lapply(seq.int(6), function(d) {
  get_soils_data(product = 'NAT', attribute = 'CEC', component = 'VAL',
                 depth = d, aoi = extent(nsw), write_out = FALSE)
})

names(cec[[1]]) = "CEC depth 0-5cm"
names(cec[[2]]) = "CEC depth 5-15cm"
names(cec[[3]]) = "CEC depth 15-30cm"
names(cec[[4]]) = "CEC depth 30-60cm"
names(cec[[5]]) = "CEC depth 60-100cm"
names(cec[[6]]) = "CEC depth 100-200cm"

writeRaster(cec, "data/soilcec_all_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)

# ECEC ####
doParallel::registerDoParallel()
ece <- lapply(seq.int(6), function(d) {
  get_soils_data(product = 'NAT', attribute = 'ECEC', component = 'VAL',
                 depth = d, aoi = extent(nsw), write_out = FALSE)
})

names(ece[[1]]) = "ECEC depth 0-5cm"
names(ece[[2]]) = "ECEC depth 5-15cm"
names(ece[[3]]) = "ECEC depth 15-30cm"
names(ece[[4]]) = "ECEC depth 30-60cm"
names(ece[[5]]) = "ECEC depth 60-100cm"
names(ece[[6]]) = "ECEC depth 100-200cm"

writeRaster(ece, "data/soilecec_all_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
# depth to hard rock ####
doParallel::registerDoParallel()
der <- get_soils_data(product = 'NAT', attribute = 'DER', component = 'VAL',
                 depth = 1, aoi = extent(nsw), write_out = FALSE)

names(der[[1]]) = "DER depth 0-5cm"
names(der[[2]]) = "DER depth 5-15cm"
names(der[[3]]) = "DER depth 15-30cm"
names(der[[4]]) = "DER depth 30-60cm"
names(der[[5]]) = "DER depth 60-100cm"
names(der[[6]]) = "DER depth 100-200cm"

writeRaster(der, "data/soilder_all_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)

# depth of soil A and B horizons ####
doParallel::registerDoParallel()
des <- get_soils_data(product = 'NAT', attribute = 'DES', component = 'VAL',
                      depth = 1, aoi = extent(nsw), write_out = FALSE)

names(des[[1]]) = "DES depth 0-5cm"
names(des[[2]]) = "DES depth 5-15cm"
names(des[[3]]) = "DES depth 15-30cm"
names(des[[4]]) = "DES depth 30-60cm"
names(des[[5]]) = "DES depth 60-100cm"
names(des[[6]]) = "DES depth 100-200cm"

writeRaster(des, "data/soildes_all_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)

# plant exploitable (effective) depth ####
doParallel::registerDoParallel()
dpe <- get_soils_data(product = 'NAT', attribute = 'DPE', component = 'VAL',
                      depth = 1, aoi = extent(nsw), write_out = FALSE)

names(dpe[[1]]) = "DPE depth 0-5cm"
names(dpe[[2]]) = "DPE depth 5-15cm"
names(dpe[[3]]) = "DPE depth 15-30cm"
names(dpe[[4]]) = "DPE depth 30-60cm"
names(dpe[[5]]) = "DPE depth 60-100cm"
names(dpe[[6]]) = "DPE depth 100-200cm"

writeRaster(dpe, "data/soildpe_all_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)

# coarse fragments ####
doParallel::registerDoParallel()
cfg <- lapply(seq.int(6), function(d) {
  get_soils_data(product = 'NAT', attribute = 'CFG', component = 'VAL',
                 depth = d, aoi = extent(nsw), write_out = FALSE)
})

names(cfg[[1]]) = "CFG depth 0-5cm"
names(cfg[[2]]) = "CFG depth 5-15cm"
names(cfg[[3]]) = "CFG depth 15-30cm"
names(cfg[[4]]) = "CFG depth 30-60cm"
names(cfg[[5]]) = "CFG depth 60-100cm"
names(cfg[[6]]) = "CFG depth 100-200cm"

writeRaster(cfg, "data/soilcfg_all_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
