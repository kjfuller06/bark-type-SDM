library(raster)
library(tmap)
library(slga)
library(sf)
library(snowfall)
library(parallel)
options(stringsAsFactors = FALSE)

# load extent shapefile
nsw = st_read("data/NSW_sans_islands.shp") %>% 
  st_transform(crs = 4283)

# soil bulk density- whole earth ####
doParallel::registerDoParallel()

# download datasets
bdw <- lapply(seq.int(6), function(d) {
  get_soils_data(product = 'NAT', attribute = 'BDW', component = 'VAL',
                 depth = d, aoi = extent(nsw), write_out = FALSE)
})

# change names of layers
names(bdw[[1]]) = "BDW depth 0-5cm"
names(bdw[[2]]) = "BDW depth 5-15cm"
names(bdw[[3]]) = "BDW depth 15-30cm"
names(bdw[[4]]) = "BDW depth 30-60cm"
names(bdw[[5]]) = "BDW depth 60-100cm"
names(bdw[[6]]) = "BDW depth 100-200cm"

# stack for saving
bdw = raster::stack(bdw)

# save to disk
writeRaster(bdw, "data/CSIRO_soils/soilbdw_all_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)

# remove from working memory
rm(bdw)

# soil organic carbon ####
socfun <- function(d) {
  get_soils_data(product = 'NAT', attribute = 'SOC', component = 'VAL',
                 depth = d, aoi = extent(nsw), write_out = FALSE)
}

sfInit(parallel = TRUE, cpus = detectCores())
sfExport("nsw", "socfun")
sfLibrary(slga)
sfLibrary(raster)

system.time({
  
  soc = sfLapply(seq.int(6), socfun)
  
  names(soc[[1]]) = "SOC depth 0-5cm"
  names(soc[[2]]) = "SOC depth 5-15cm"
  names(soc[[3]]) = "SOC depth 15-30cm"
  names(soc[[4]]) = "SOC depth 30-60cm"
  names(soc[[5]]) = "SOC depth 60-100cm"
  names(soc[[6]]) = "SOC depth 100-200cm"
  
  soc = raster::stack(soc)
  
  writeRaster(soc, "data/CSIRO_soils/soilsoc_all_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
})[[3]]

sfStop()

# clay content ####
clyfun <- function(d) {
  get_soils_data(product = 'NAT', attribute = 'CLY', component = 'VAL',
                 depth = d, aoi = extent(nsw), write_out = FALSE)
}

sfInit(parallel = TRUE, cpus = detectCores())
sfExport("nsw", "clyfun")
sfLibrary(slga)
sfLibrary(raster)

system.time({

  cly = sfLapply(seq.int(6), clyfun)

names(cly[[1]]) = "CLY depth 0-5cm"
names(cly[[2]]) = "CLY depth 5-15cm"
names(cly[[3]]) = "CLY depth 15-30cm"
names(cly[[4]]) = "CLY depth 30-60cm"
names(cly[[5]]) = "CLY depth 60-100cm"
names(cly[[6]]) = "CLY depth 100-200cm"

cly = raster::stack(cly)

writeRaster(cly, "data/CSIRO_soils/soilclay_all_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
})[[3]]

sfStop()
# 4075.87secs

# silt content ####
system.time({
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

slt = raster::stack(slt)

writeRaster(slt, "data/CSIRO_soils/soilsilt_all_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
})[[3]]

# 10945.51secs

# sand content ####
sndfun <- function(d) {
  get_soils_data(product = 'NAT', attribute = 'SND', component = 'VAL',
                 depth = d, aoi = extent(nsw), write_out = FALSE)
}

sfInit(parallel = TRUE, cpus = detectCores())
sfExport("nsw", "sndfun")
sfLibrary(slga)
sfLibrary(raster)

system.time({
  
  snd = sfLapply(seq.int(6), sndfun)
  
  names(snd[[1]]) = "SND depth 0-5cm"
  names(snd[[2]]) = "SND depth 5-15cm"
  names(snd[[3]]) = "SND depth 15-30cm"
  names(snd[[4]]) = "SND depth 30-60cm"
  names(snd[[5]]) = "SND depth 60-100cm"
  names(snd[[6]]) = "SND depth 100-200cm"
  
  snd = raster::stack(snd)
  
  writeRaster(snd, "data/CSIRO_soils/soilsand_all_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
})[[3]]

sfStop()

# pH based on CaCl2 extraction ####
phcfun <- function(d) {
  get_soils_data(product = 'NAT', attribute = 'PHC', component = 'VAL',
                 depth = d, aoi = extent(nsw), write_out = FALSE)
}

sfInit(parallel = TRUE, cpus = detectCores())
sfExport("nsw", "phcfun")
sfLibrary(slga)
sfLibrary(raster)

system.time({
  
  phc = sfLapply(seq.int(6), phcfun)
  
  names(phc[[1]]) = "PHC depth 0-5cm"
  names(phc[[2]]) = "PHC depth 5-15cm"
  names(phc[[3]]) = "PHC depth 15-30cm"
  names(phc[[4]]) = "PHC depth 30-60cm"
  names(phc[[5]]) = "PHC depth 60-100cm"
  names(phc[[6]]) = "PHC depth 100-200cm"
  
  phc = raster::stack(phc)
  
  writeRaster(phc, "data/CSIRO_soils/soilphc_all_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
})[[3]]

sfStop()

# available water capacity ####
awcfun <- function(d) {
  get_soils_data(product = 'NAT', attribute = 'AWC', component = 'VAL',
                 depth = d, aoi = extent(nsw), write_out = FALSE)
}

sfInit(parallel = TRUE, cpus = detectCores())
sfExport("nsw", "awcfun")
sfLibrary(slga)
sfLibrary(raster)

system.time({
  
  awc = sfLapply(seq.int(6), awcfun)
  
  names(awc[[1]]) = "AWC depth 0-5cm"
  names(awc[[2]]) = "AWC depth 5-15cm"
  names(awc[[3]]) = "AWC depth 15-30cm"
  names(awc[[4]]) = "AWC depth 30-60cm"
  names(awc[[5]]) = "AWC depth 60-100cm"
  names(awc[[6]]) = "AWC depth 100-200cm"
  
  awc = raster::stack(awc)
  
  writeRaster(awc, "data/CSIRO_soils/soilawc_all_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
})[[3]]

sfStop()

# total nitrogen ####
ntofun <- function(d) {
  get_soils_data(product = 'NAT', attribute = 'NTO', component = 'VAL',
                 depth = d, aoi = extent(nsw), write_out = FALSE)
}

sfInit(parallel = TRUE, cpus = detectCores())
sfExport("nsw", "ntofun")
sfLibrary(slga)
sfLibrary(raster)

system.time({
  
  nto = sfLapply(seq.int(6), ntofun)
  
  names(nto[[1]]) = "NTO depth 0-5cm"
  names(nto[[2]]) = "NTO depth 5-15cm"
  names(nto[[3]]) = "NTO depth 15-30cm"
  names(nto[[4]]) = "NTO depth 30-60cm"
  names(nto[[5]]) = "NTO depth 60-100cm"
  names(nto[[6]]) = "NTO depth 100-200cm"
  
  nto = raster::stack(nto)
  
  writeRaster(nto, "data/CSIRO_soils/soilnto_all_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
})[[3]]

sfStop()

# total phosphorus ####
ptofun <- function(d) {
  get_soils_data(product = 'NAT', attribute = 'PTO', component = 'VAL',
                 depth = d, aoi = extent(nsw), write_out = FALSE)
}

sfInit(parallel = TRUE, cpus = detectCores())
sfExport("nsw", "ptofun")
sfLibrary(slga)
sfLibrary(raster)

system.time({
  
  pto = sfLapply(seq.int(6), ptofun)
  
  names(pto[[1]]) = "PTO depth 0-5cm"
  names(pto[[2]]) = "PTO depth 5-15cm"
  names(pto[[3]]) = "PTO depth 15-30cm"
  names(pto[[4]]) = "PTO depth 30-60cm"
  names(pto[[5]]) = "PTO depth 60-100cm"
  names(pto[[6]]) = "PTO depth 100-200cm"
  
  pto = raster::stack(pto)
  
  writeRaster(pto, "data/CSIRO_soils/soilpto_all_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
})[[3]]

sfStop()

# ECEC ####
ecefun <- function(d) {
  get_soils_data(product = 'NAT', attribute = 'ECE', component = 'VAL',
                 depth = d, aoi = extent(nsw), write_out = FALSE)
}

sfInit(parallel = TRUE, cpus = detectCores())
sfExport("nsw", "ecefun")
sfLibrary(slga)
sfLibrary(raster)

system.time({
  
  ece = sfLapply(seq.int(6), ecefun)
  
  names(ece[[1]]) = "ECE depth 0-5cm"
  names(ece[[2]]) = "ECE depth 5-15cm"
  names(ece[[3]]) = "ECE depth 15-30cm"
  names(ece[[4]]) = "ECE depth 30-60cm"
  names(ece[[5]]) = "ECE depth 60-100cm"
  names(ece[[6]]) = "ECE depth 100-200cm"
  
  ece = raster::stack(ece)
  
  writeRaster(ece, "data/CSIRO_soils/soilece_all_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
})[[3]]

sfStop()

# depth to hard rock- test ####
## need to test
derfun <- function(d) {
  get_soils_data(product = 'NAT', attribute = 'DER', component = 'VAL',
                 depth = d, aoi = extent(nsw), write_out = FALSE)
}

sfInit(parallel = TRUE, cpus = detectCores())
sfExport("nsw", "derfun")
sfLibrary(slga)
sfLibrary(raster)

system.time({
  
  der = derfun(1)
  
  writeRaster(der, "data/CSIRO_soils/soilder_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
})[[3]]

sfStop()
# 1033.82

# depth of soil A and B horizons ####
desfun <- function(d) {
  get_soils_data(product = 'NAT', attribute = 'DES', component = 'VAL',
                 depth = d, aoi = extent(nsw), write_out = FALSE)
}

sfInit(parallel = TRUE, cpus = detectCores())
sfExport("nsw", "desfun")
sfLibrary(slga)
sfLibrary(raster)

system.time({
  
  des = desfun(1)
  
  writeRaster(des, "data/CSIRO_soils/soildes_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
})[[3]]

sfStop()
