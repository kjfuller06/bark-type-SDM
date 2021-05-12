# record of code run on Cheyenne
library(raster)
library(sf)
library(tmap)
library(gdalUtils)
library(snowfall)
library(parallel)
library(tidyverse)

# dataset cropping ####
# load datasets
veg = raster("RFS Fuel Type/fuels_30m.tif")
nsw = st_read("NSW_fuelsproj.shp")
bioclim = raster::stack("data/bioclim_cropped.grd")
fire = raster("data/firehistory.tif")
dems = raster("data/dem_s.tif")
arid = raster("data/ai_et0.tif")
pet = raster("data/et0_yr.tif")

# stack all aridity-related layers
petall = raster::stack(raster("data/et0_01.tif"), raster("data/et0_02.tif"), raster("data/et0_03.tif"), raster("data/et0_04.tif"), raster("data/et0_05.tif"), raster("data/et0_06.tif"), raster("data/et0_07.tif"), raster("data/et0_08.tif"), raster("data/et0_09.tif"), raster("data/et0_10.tif"), raster("data/et0_11.tif"), raster("data/et0_12.tif"))
ai.pet = raster::stack(arid, pet, petall)

# crop aridity layers to NSW extent
nsw1 = nsw %>% 
  st_transform(crs = crs(ai.pet))
ai.pet = crop(arid, extent(nsw1))
# correct aridity units
ai.pet[[1]] = ai.pet[[1]]/10000

# correct bioclim temperature units
# temps = bioclim[[c('bio1', 'bio2', 'bio5', 'bio6', 'bio7', 'bio8', 'bio9', 'bio10', 'bio11')]]/10
# bioclim = raster::stack(temps, bioclim[[c(names(bioclim)[c(3,4,12:19)])]])

# writeRaster(bioclim, "bioclim_cropped.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
## can't write to stack with changed values- not sure why
writeRaster(ai.pet, "data/ai.pet.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)

# reproject fire layer, resample using nearest neighbour
gdaltindex("data/veg_extent.shp", "data/fuels_30m.tif")
gdalwarp(srcfile = "data/firehistory.tif", dstfile = "data/fire_final.tif", t_srs = crs(veg), tr = res(veg), r = "near", cl = "data/veg_extent.shp", crop_to_cutline = TRUE, multi = TRUE, co = c("BIGTIFF=TRUE", "COMPRESS=DEFLATE"), wo = "NUM_THREADS=ALL_CPUS", overwrite = TRUE)
rm(fire, fire1)

#--------------- first soils download----------------
## job in Casper
options(stringsAsFactors = FALSE)

# load extent shapefile
nsw = st_read("data/NSW_sans_islands.shp") %>% 
  st_transform(crs = 4283)

# depth to hard rock ####
depthfun <- function(x) {
  get_soils_data(product = 'NAT', attribute = x, component = 'VAL',
                 depth = 1, aoi = extent(nsw), write_out = FALSE)
}

sfInit(parallel = TRUE, cpus = detectCores())
sfExport("nsw", "depthfun")
sfLibrary(slga)
sfLibrary(raster)

depth = sfLapply(list("DER", "DES"), depthfun)

names(depth[[1]]) = "DER"
names(depth[[2]]) = "DES"

writeRaster(depth[[1]], "data/soilder_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
writeRaster(depth[[2]], "data/soildes_80m.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)

sfStop()

#---------------- BDW -------------------
library(raster)
library(rgdal)
library(slga)
library(sf)
library(snowfall)
library(parallel)
options(stringsAsFactors = FALSE)

nsw = st_read("data/NSW_sans_islands.shp") %>% 
  st_transform(crs = 4283)

vars = c("BDW", "SOC", "CLY", "SLT", "SND", "PHC", "AWC", "NTO", "PTO", "ECE")

a = 1

sfun = function(x){
  s = get_soils_data(product = 'NAT', attribute = vars[a], component = 'VAL', depth = x, aoi = extent(nsw), write_out = FALSE)
  names(s) = paste0(vars[a], "D", x)
  writeRaster(s, paste0("data/soil", vars[a], "_D", x, "_80m.tif"), format = "GTiff", overwrite = TRUE)
}

sfInit(parallel = TRUE, cpus = 6)
sfExport("nsw", "sfun", "a", "vars")
sfLibrary(slga)
sfLibrary(raster)
sfLibrary(sf)
sfLibrary(rgdal)

sfLapply(seq.int(6), sfun)

sfStop()

## batch script
# #!/bin/bash -l
# #SBATCH --job-name=downBDW
# #SBATCH --account=UWSY0001
# #SBATCH --ntasks=6
# #SBATCH --cpus-per-task=1
# #SBATCH --mem=100G
# #SBATCH --time=06:00:00
# #SBATCH --partition=dav
# 
# ### Temp data to scratch
# export TMPDIR=/glade/scratch/kjfuller/temp
# 
# ### Load modules
# module load R/4.0.2
# module unload netcdf
# module load gdal
# 
# ### Run analysis script
# R CMD BATCH /glade/scratch/kjfuller/scripts/downBDW.R
# 
# ### Store job stats in log file
# scontrol show job $SLURM_JOBID

#------------------ run on laptop --------------------------
library(raster)
library(rgdal)
library(slga)
library(sf)
library(snowfall)
library(parallel)
options(stringsAsFactors = FALSE)

nsw = st_read("data/NSW_sans_islands.shp") %>% 
  st_transform(crs = 4283)

vars = c("PHC", "AWC", "NTO", "PTO", "ECE")

sfun = function(x){
  s = get_soils_data(product = 'NAT', attribute = vars[a], component = 'VAL', depth = x, aoi = extent(nsw), write_out = FALSE)
  writeRaster(s, paste0("soil", vars[a], "_D", x, "_80m.tif"), format = "GTiff", overwrite = TRUE)
  rm(s)
}

sfInit(parallel = TRUE, cpus = 6)
sfExport("nsw", "sfun", "vars")
sfLibrary(slga)
sfLibrary(raster)
sfLibrary(sf)
sfLibrary(rgdal)

a = 1
sfExport("a")
sfLapply(c(2, 4, 5, 6), sfun)
a = 2
sfExport("a")
sfLapply(c(1,3), sfun)
a = 3
sfExport("a")
sfLapply(c(2, 5), sfun)
a = 4
sfExport("a")
sfLapply(c(1, 4, 6), sfun)
a = 5
sfExport("a")
sfLapply(c(2), sfun)

sfStop()


#--------- mosaic terrain tiles ----------
library(raster)
library(sf)
library(rgdal)
library(gdalUtils)
library(tidyverse)

setwd("/glade/scratch/kjfuller/data")
veg = raster("processed datasets/fuels_30m.tif")
tiles = list.files("./processed datasets/PlanCurvature/NSW", pattern = "hdr.adf", recursive = TRUE, full.names = TRUE)
t1 = system.file("processed datasets/PlanCurvature/NSW/e140/s29/e140s29/hdr.adf", package = "gdalUtils")
t2 = system.file("processed datasets/PlanCurvature/NSW/e140/s30/e140s30/hdr.adf", package = "gdalUtils")
codes = make_EPSG()
c = codes %>% filter(code == 4326) %>% dplyr::select(prj4)

mosaic_rasters(gdalfile = tiles[c(1:2)],
              dst_dataset = "proj_dem_plan_30m.tif",
              s_srs = CRS(paste0(c)),
              of = "GTiff",
              separate = FALSE,
              verbose = TRUE,
              overwrite = TRUE)
gdalinfo("proj_dem_plan_30m.tif")
test = raster("proj_dem_plan_30m.tif")

## not working

#----------- next try --------------
library(raster)
library(sf)
library(rgdal)
library(gdalUtils)
library(tidyverse)

setwd("/glade/scratch/kjfuller/data")
codes = make_EPSG()
c = codes %>% filter(code == 4326) %>% dplyr::select(prj4)

# plan curvature
rasters1 = list.files("./plan_NSW", pattern = "hdr.adf", recursive = TRUE, full.names = TRUE)
rast.list <- list()
for(i in 1:length(rasters1)){
  rast.list[i] <- raster(rasters1[i], crs = paste(c))
  }
rast.list$fun <- mean
rast.mosaic <- do.call(mosaic, rast.list)
writeRaster(rast.mosaic, "dem_plan_30m.tif", overwrite = TRUE)

# profile curvature
rasters1 = list.files("./profile_NSW", pattern = "hdr.adf", recursive = TRUE, full.names = TRUE)
rast.list <- list()
for(i in 1:length(rasters1)){
  rast.list[i] <- raster(rasters1[i], crs = paste(c))
  }
rast.list$fun <- mean
rast.mosaic <- do.call(mosaic, rast.list)
writeRaster(rast.mosaic, "dem_profile_30m.tif", overwrite = TRUE)

# TWI
rasters1 = list.files("./TWI_NSW", pattern = "hdr.adf", recursive = TRUE, full.names = TRUE)
rast.list <- list()
for(i in 1:length(rasters1)){
  rast.list[i] <- raster(rasters1[i], crs = paste(c))
  }
rast.list$fun <- mean
rast.mosaic <- do.call(mosaic, rast.list)
writeRaster(rast.mosaic, "dem_TWI_30m.tif", overwrite = TRUE)

# SRAD total ---------------
library(sf)
library(raster)
library(rgdal)
library(gdalUtils)
library(tidyverse)
library(snowfall)
library(parallel)

setwd("/glade/scratch/kjfuller/data")
codes = make_EPSG()
c = codes %>% filter(code == 4326) %>% dplyr::select(prj4)

dates = c("0115", "0214", "0315", "0415", "0515", "0615", "0715", "0815", "0915", "1015", "1115", "1215")

sradfun = function(k){
  rasters1 = list.files("./SRADTot_NSW", pattern = paste0(k, ".flt"), recursive = TRUE, full.names = TRUE)
  
  rast.list = list()
  for(i in c(1:length(rasters1))){
    rast.list[i] = raster(rasters1[i], crs = paste(c))
  }
  rast.list$fun = mean
  rast.mosaic = do.call(mosaic, rast.list)
  writeRaster(rast.mosaic, paste0("dem_SRADTot_", k, "_30m.tif"), overwrite = TRUE)
}

sfInit(parallel = TRUE, cpus = 12)
sfExport("c", "dates", "sradfun")
sfLibrary(raster)
sfLibrary(sf)
sfLibrary(gdalUtils)

sfLapply(dates, sradfun)

sfStop()

## doesn't work, error: unable to find inherited method for function 'mosaic' for signature '"RasterLayer', 'missing'"
## run as interactive script instead, mosaicking the tiles in batches like:
r = rast.list[c(1:2)]
r$fun = mean
rast.mosaic = do.call(mosaic, r)

r = rast.list[c(3:10)]
r$fun = mean
rast.mosaic = do.call(mosaic, c(rast.mosaic, r))

r = rast.list[c(10:20)]
r$fun = mean
rast.mosaic = do.call(mosaic, c(rast.mosaic, r))

r = rast.list[c(11:20)]
r$fun = mean
rast.mosaic = do.call(mosaic, c(rast.mosaic, r))

r = rast.list[c(21:30)]
r$fun = mean
rast.mosaic = do.call(mosaic, c(rast.mosaic, r))

r = rast.list[c(31:50)]
r$fun = mean
rast.mosaic = do.call(mosaic, c(rast.mosaic, r))

r = rast.list[c(50:110)]
r$fun = mean
rast.mosaic = do.call(mosaic, c(rast.mosaic, r))

r = rast.list[111]
r$fun = mean
rast.mosaic = do.call(mosaic, c(rast.mosaic, r))
writeRaster(rast.mosaic, "dem_SRADTot_0115_30m.tif", overwrite = TRUE)
## works fine, result has min and max values of 3.176657 and 30.95401
### I think this is because I was trying to run do.call(mosaic) with each sequetial raster, instead of the whole list; yep, it works now
#### looped mosaic didn't mosaic the second tile for some reason, mosaicked with the rest of the tiles manually
# --> it looks like the second tile is replaced by fum = mean when that gets assigned; moved rast.list$fun = mean down to the function level instead of running it for each element in the list and this solved it.

# ----------------- rasterize land tenure shapefile --------------------
library(sf)
library(raster)
library(tidyverse)
library(rgdal)
library(gdalUtils)
library(stars)

setwd("/glade/scratch/kjfuller/data")
codes = make_EPSG()
c = codes %>% filter(code == 7853) %>% dplyr::select(prj4)

tenure = st_read("NSW_Landuse_2013.shp") ### 1043185 features
veg = raster("fuels_30m.tif")
veg = projectRaster(veg, crs = paste(c), method = 'ngb')
writeRaster(veg, "veg_tenureproj.tif")

tenure = tenure %>% 
  # st_transform(crs = 7853) %>% 
  st_transform(crs = st_crs(veg))
names(tenure)[2] = "values"
tenure$values = as.numeric(tenure$values)

raster_temp = raster(extent(veg), resolution = res(veg), crs = crs(veg))
df = tenure
st_geometry(df) = NULL
ten = rasterize(tenure, raster_temp, df$values, fun = mean)


# output.rast = raster(extent(veg), res = res(veg), crs = crs(veg))
# st = raster::stack(output.rast)
# for(i in c(1)){
#   tenure.rast = st_rasterize(tenure[c((1+(i-1)*2000), i*2000),], st_as_stars(st_bbox(veg), nx = ncol(veg), ny = nrow(veg), values = NA_real_))
#   tenure.rast = as(tenure.rast, Class = "Raster")
#   output.rast = mosaic(output.rast, tenure.rast, fun = mean)
#   }
# 
# 
# 
# 
# raster_temp = raster(extent(veg), resolution = res(veg), crs = crs(veg))
# rec = rasterize(records, raster_temp, field = 110)

# setwd("/glade/scratch/kjfuller/data")
# codes = make_EPSG()
# c = codes %>% filter(code == 7853) %>% dplyr::select(prj4)
# 
# tenure = st_read("NSW_Landuse_2013.shp") ### 1043185 features
# veg = raster("fuels_30m.tif")
# veg = projectRaster(veg, crs = paste(c), method = 'ngb')
# 
# records = records %>% 
#   st_transform(crs = 7853) 
# records$values = as.numeric(records$values)
# 
# 
# # for(i in c(1)){
#   records.rast = st_rasterize(records, st_as_stars(st_bbox(ndvi), nx = ncol(ndvi), ny = nrow(ndvi), values = NA_real_))
#   records.rast2 = as(records.rast, Class = "Raster")
# # }

#--------------------- soil layers ---------------------------
library(raster)
library(sf)
library(rgdal)
library(gdalUtils)
library(snowfall)
library(parallel)

setwd("/glade/scratch/kjfuller")
veg = raster("data/fuels_30m.tif")

# soil data
soils = list.files("./data", pattern = "^soil", recursive = FALSE, full.names = TRUE)
soils = soils[!grepl("gri", soils)]

soilfun = function(x){
  k = substr(soils[x], 8, nchar(soils[x])-7)
  s = raster(soils[x])
  s = projectRaster(s, veg, method = 'bilinear')
  writeRaster(s, paste0("data/proj_", k, "30m.tif"), overwrite = TRUE)
  # gdalwarp(srcfile = soils[x],
  #          dstfile = paste0("data/proj_", k, "30m.tif"),
  #          t_srs = crs(veg),
  #          tr = res(veg),
  #          r = 'bilinear',
  #          cl = "data/veg_extent.shp",
  #          crop_to_cutline = TRUE,
  #          multi = TRUE,
  #          co = c("BIGTIFF=YES", "COMPRESS=DEFLATE"),
  #          wo = "NUM_THREADS=ALL_CPUS",
  #          overwrite = TRUE)
}

sfInit(parallel = TRUE, cpus = 66)
sfExport("veg", "soils", "soilfun")
sfLibrary(slga)
sfLibrary(raster)
sfLibrary(sf)
sfLibrary(rgdal)
sfLibrary(gdalUtils)

sfLapply(c(1:66), soilfun)

sfStop()

#--------------------- dem layers ---------------------------
library(raster)
library(sf)
library(rgdal)
library(gdalUtils)

print("should be SRADRat_11, demsing14")
setwd("/glade/scratch/kjfuller")
veg = raster("data/fuels_30m.tif")

# terrain data -> changed to remove already-resampled data layers
dems = list.files("./data", pattern = "^dem_plan", recursive = FALSE, full.names = TRUE)
dems[2] = list.files("./data", pattern = "^dem_profile", recursive = FALSE, full.names = TRUE)
dems[3] = list.files("./data", pattern = "^dem_TWI", recursive = FALSE, full.names = TRUE)
dems[4:27] = list.files("./data", pattern = "^dem_SRAD", recursive = FALSE, full.names = TRUE)
dems = soils[!grepl("gri", dems)]

# done already: plan, profile, TWI, SRADRat_01-10 (1-13)
# running now: SRADRat_12 (15)
x = 14
k = substr(dems[x], 8, nchar(dems[x])-7)
d = raster(dems[x])
d = projectRaster(d, veg, method = 'bilinear')
writeRaster(d, paste0("data/proj_", k, "30m.tif"), overwrite = TRUE)

#--------------------- remaining layers ---------------------------
library(raster)
library(sf)
library(rgdal)
library(gdalUtils)
library(snowfall)
library(parallel)

setwd("/glade/scratch/kjfuller")
veg = raster("data/fuels_30m.tif")

# remaining data
all = list.files("./data", pattern = "^fire", recursive = FALSE, full.names = TRUE)
all[2] = list.files("./data", pattern = "^NDVI", recursive = FALSE, full.names = TRUE)

allfun = function(x){
  k = substr(all[x], 8, nchar(all[x])-4)
  a = raster(all[x])
  a = projectRaster(a, veg, method = 'bilinear')
  writeRaster(a, paste0("data/proj_", k, "_30m.tif"), overwrite = TRUE)
}

sfInit(parallel = TRUE, cpus = 2)
sfExport("veg", "all", "allfun")
sfLibrary(slga)
sfLibrary(raster)
sfLibrary(sf)
sfLibrary(rgdal)
sfLibrary(gdalUtils)

sfLapply(c(1:2), allfun)

sfStop()

#-------------------- joining bark traits and environmental data ----------------------
library(tidyverse)

setwd("/glade/scratch/kjfuller/data")
# load environmental data
env = read.csv("dataextract_allsites_30m.csv")
# load bark observations
PA = read.csv("site-specific_P-A_wide_barks.csv")

# join dataframes, retaining all data
PA_env = full_join(env, PA)
## 17355 rows
PA_env = drop_na(PA_env)
## 219 rows
write.csv(PA_env, "joined_gonewrong.csv", row.names = FALSE)
## won't be used anyway because it uses actual variable values

#------------------ mask one environmental data layer by forest veg types --------------
library(raster)

setwd("/glade/scratch/kjfuller/data")
env = raster("proj_ai.pet_10.tif")
veg = raster("for_fuels_30m.tif")

env = raster::mask(env, veg)
writeRaster(env, "proj_ai.pet_10_mask.tif")

#---------------- mask all environmental data layers -------------------------------
library(raster)

setwd("/glade/scratch/kjfuller")
veg = raster("data/for_fuels_30m.tif")
env = list.files("./data", pattern = "^proj", recursive = FALSE, full.names = TRUE)
env2 = list.files("./data", pattern = "^proj", recursive = FALSE, full.names = FALSE)

for(i in c(1:length(env))){
  x = raster(env[i])
  x = raster::mask(x, veg)
  writeRaster(x, paste0("data/mask_", env2[i]))
}

#----------------- additional masking script ---------------------------------
library(raster)

setwd("/glade/scratch/kjfuller")
veg = raster("data/for_fuels_30m.tif")
env = list.files("./data", pattern = "^proj", recursive = FALSE, full.names = TRUE)
env2 = list.files("./data", pattern = "^proj", recursive = FALSE, full.names = FALSE)

for(i in c(length(env):1)){
  x = raster(env[i])
  x = raster::mask(x, veg)
  writeRaster(x, paste0("data/mask_", env2[i]))
}

#-------------- rasterPCA timing test -------------------
library(RStoolbox)
library(raster)
library(sf)
library(tidyverse)

# example with rlogo
data(rlogo)
ggRGB(rlogo, 1,2,3)

## Run PCA
set.seed(25)
lay3ncomp3_check = system.time({
  rpc <- rasterPCA(rlogo, nComp = 3, spca = TRUE, maskCheck = TRUE)
})[[3]]
rpc

## Model parameters:
a = summary(rpc$model)
moddata = data.frame(center = a$center,
                     scale = a$scale)
loadings = as.data.frame(loadings(rpc$model)[,1:3])
loadings = rbind(loadings, a$sdev)
row.names(loadings)[4] = "sdev"

setwd("/glade/scratch/kjfuller")
mask = list.files("./data", pattern = "^mask", recursive = FALSE, full.names = TRUE)

m1 = raster(mask[1])
m2 = raster(mask[20])
m3 = raster(mask[40])
m4 = raster(mask[60])
m5 = raster(mask[80])
r = raster::stack(m1, m2, m3, m4, m5)

set.seed(225)
lay5ncomp3_check = system.time({
  pca = rasterPCA(r, nComp = 3, spca = TRUE, maskCheck = TRUE)
})[[3]]

#------------- rasterPCA of all layers -------------------
n = 20
layers = all
check = "nocheck"
label = paste0("data/PCA_N", n, "Lall", "s5000", check, "_")
library(RStoolbox)
library(raster)
library(sf)
library(tidyverse)

setwd("/glade/scratch/kjfuller")
mask = list.files("./data", pattern = "^mask", recursive = FALSE, full.names = TRUE)

p = raster(mask[1])
for(i in c(2:length(mask))){
  x = raster(mask[i])
  p = raster::stack(p, x)
}

set.seed(225)
elapsed = system.time({
  pca = rasterPCA(p, nComp = n, spca = TRUE, maskCheck = TRUE)
})
time = Sys.time()

# save PCA time
capture.output(time, elapsed, file = paste0(label, "timing.txt"))
# save text output of princomp
capture.output(pca, file = paste0(label, "textoutput.txt"))
capture.output(summary(rpc$model), file = paste0(label, "textoutput2.txt"))

# save pc values as grid layers
r = pca$map
writeRaster(r, paste0(label, "PC.grd"), overwrite = TRUE)
writeRaster(pca$map, paste0(label, "PC.tif"), bylayer = TRUE, overwrite = TRUE)

# save all model components as csv's
mod = summary(pca$model)
moddata = data.frame(center = mod$center,
                     scale = mod$scale)
loadings = as.data.frame(loadings(pca$model)[,1:n])
loadings = rbind(loadings, mod$sdev)
row.names(loadings)[layers+1] = "sdev"
write.csv(moddata, paste0(label, "layer_scaling.csv"), row.names = TRUE)
write.csv(loadings, paste0(label, "loadings_sdev.csv"), row.names = TRUE)

## timed out with no result

#----------------------- test and time rasterToPoints() --------------------
library(raster)
library(sf)
library(tidyverse)

setwd("D:/chapter1/other_data/Original/aridity/et0_mo")

r1 = raster("et0_01.tif")
r2 = raster("et0_02.tif")
r3 = raster("et0_03.tif")
s = raster::stack(r1, r2, r3)

t = system.time({
  pts = rasterToPoints(s)
})[[3]]
## ran out of memory

t2 = system.time({
  pts2 = as.data.frame(s, xy = TRUE)
})[[3]]
## ran out of memory

#------------------- test and time rasterToPoints() on Cheyenne -----------------
library(raster)
library(sf)
library(tidyverse)

setwd("/glade/scratch/kjfuller/data")

r1 = raster("mask_proj_ai.pet_10.tif")
r2 = raster("mask_proj_ai.pet_11.tif")
r3 = raster("mask_proj_ai.pet_12.tif")
s = raster::stack(r1, r2, r3)

t = system.time({
  pts = rasterToPoints(s)
})
capture.output(t, file = "rasterToPoints_time.txt")

t2 = system.time({
  pts2 = as.data.frame(s, xy = TRUE)
})
capture.output(t, file = "as.data.frame_time.txt")
## ^much faster

#--------------------- raster to df on Casper --------------------
library(raster)
library(sf)
library(tidyverse)

setwd("/glade/scratch/kjfuller/data")

mask = list.files("./", pattern = "^mask", recursive = FALSE, full.names = TRUE)

p = raster(mask[1])
for(i in c(2:length(mask))){
  x = raster(mask[i])
  p = raster::stack(p, x)
}

pts = as.data.frame(p, xy = TRUE)
pts = drop_na(pts)
write.csv(pts, "all_values_forPCA.csv", row.names = FALSE)
write.table(pts, "all_values_forPCA.txt", sep = ",", row.names = FALSE)

## ^ ran out of memory

#--------------------- raster to df one at a time on Casper ----------------
library(raster)
library(sf)
library(tidyverse)

setwd("/glade/scratch/kjfuller/data")

mask = list.files("./", pattern = "^mask", recursive = FALSE, full.names = TRUE)

for(i in c(1:length(mask))){
  r = raster(mask[i])
  df = as.data.frame(r, xy = TRUE)
  df = drop_na(df)
  write.csv(df, paste0(i, "_values_forPCA.csv"), row.names = FALSE)
  write.table(df, paste0(i, "_values_forPCA.txt"), sep = ",", row.names = FALSE)
}
## generates an empty output

#--------------------- rasterToPoints() to as.data.frame() on Casper ----------------
library(raster)
library(sf)
library(parallel)
library(snowfall)

setwd("/glade/scratch/kjfuller/data")

mask = list.files("./", pattern = "^mask", recursive = FALSE, full.names = TRUE)
# first script run on mask[c(1:12, 34:46, 66:78, 98:110)]

df_fun = function(x){
  r = raster(mask[x])
  b = substr(mask[x], 14, nchar(mask[x])-4)
  df = as.data.frame(rasterToPoints(r), xy = TRUE)
  write.csv(df, paste0(b, "_forPCA.csv"), row.names = FALSE)
}

sfInit(parallel = TRUE, cpus = 8)
sfExport("mask", "df_fun")
sfLibrary(raster)
sfLibrary(sf)

sfLapply(c(1:130), df_fun)

sfStop()


# 4:03 pm start for as.data.frame(rasterToPoints(r), xy = TRUE)
# 4:13 pm end time
# 4:14 pm start for as.data.frame(rasterToPoints(r))
# 4:26 pm end time (or close)
# 86.8 GB memory used to store 4 raster-derived dataframes

#--------------------- full_join of extracted data ----------------
library(tidyverse)
library(data.table)

setwd("/glade/scratch/kjfuller/data")

forpca = list.files("./", pattern = "forPCA.csv$")

setDTthreads(8)
pca1 = data.table::fread(forpca[1])
for(i in c(2:length(forpca))){
  x = data.table::fread(forpca[i])
  pca1 = full_join(pca1, x)
}

data.table::fwrite(pca1, "allvalues_forPCA.csv")


#--------------------- re-reproject SRADTot rasters -----------------------
i = 1
library(raster)
library(sf)
library(rgdal)
library(gdalUtils)

setwd("/glade/scratch/kjfuller/data")
veg = raster("fuels_30m.tif")
layers = list.files("./", "^dem_SRADTot", recursive = FALSE, full.names = FALSE)
layers = layers[!grepl("forPCA", layers)]

reproj = function(x){
  gdalwarp(srcfile = paste0(layers[x]), 
           dstfile = paste0("proj_", layers[x]), 
           t_srs = paste(crs(veg)), 
           tr = res(veg), 
           cl = "veg_extent.shp", 
           crop_to_cutline = TRUE, 
           te = c(extent(veg)[1], extent(veg)[3], extent(veg)[2], extent(veg)[4]), 
           output_Raster = TRUE, 
           overwrite = TRUE, 
           verbose = TRUE,
           multi = TRUE, 
           co = c("BIGTIFF=YES"), 
           wo = "NUM_THREADS=ALL_CPUS", 
           r = 'bilinear')
}

reproj(i)

#----------------------- mask SRADTot layers ---------------------------
i = 1
library(raster)

setwd("/glade/scratch/kjfuller/data")
veg = raster("for_fuels_30m.tif")
env = list.files("./", pattern = "^proj_dem_SRADTot", recursive = FALSE, full.names = FALSE)

mask = function(x){
  r = raster(env[x])
  r = raster::mask(r, veg)
  writeRaster(r, paste0("mask_", env[x]), overwrite = TRUE)
}

mask(i)



#----------------------- test PCA outputs ---------------------
library(tidyverse)
library(data.table)
library(vegan)
library(factoextra)
# library(Hmisc)
# library(plotrix)

# create data and run PCA
set.seed(225)
data("iris")
t2 = system.time({
  iris2 = decostand(iris[, 1:4], method = "range")
  mod = prcomp(iris2, scale = T)
})
rm(iris2)

# figures
fviz_eig(mod)
fviz_pca_var(mod,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
             )
# stats
eigval = get_eigenvalue(mod)
res.var = get_pca_var(mod)
v1 = data.frame(res.var$coord)
v2 = data.frame(res.var$cor)
v3 = data.frame(res.var$cos2)
v4 = data.frame(res.var$contrib)
vars = cbind(v1, v2, v3, v4)
nom = c("coords", "corr", "cos2", "contrib")
for(a in c(1:4)){
  for(i in c(1:length(names(v1)))){
    names(vars)[i+length(names(v1))*(a-1)] = paste0(nom[a], i)
  }
}
write.csv(vars, "PCA1_stats.csv", row.names = FALSE)
res.ind = get_pca_ind(mod)
i1 = data.frame(res.ind$contrib)
i2 = data.frame(res.ind$cos2)
inds = cbind(i1, i2)
nom = c("contrib", "rep")
for(a in c(1:2)){
  for(i in c(1:length(names(i1)))){
    names(inds)[i+length(names(i1))*(a-1)] = paste0(nom[a], i)
  }
}

# store model outputs
sco = data.frame(scores(mod))
iris3 = iris
for(a in c(1:ncol(scores(mod)))){
  iris3 = cbind(iris3, data.frame(sco[a]))
}
iris3 = cbind(iris3, inds)
rm(sco, PC1, PC2)
data.table::fwrite(iris3, "PCA1_subsample.csv")

set.seed(225)
# try prediction
records = iris[,c(1:4)]
scaled = scale(records,
               center = mod$center,
               scale = mod$scale)
coord_fun = function(ind, loadings){
  r = loadings*ind
  apply(r, 2, sum)
}
pca.loadings = mod$rotation
scaled.coord = t(apply(scaled, 1, coord_fun, pca.loadings))
## ^this doesn't appear to work at all

# try the predict() function
records = iris[,c(1:4)]
ind.sup = predict(mod, newdata = records)
## ^same issue

# try with example data from tutorial
library("factoextra")
data(decathlon2)
decathlon2.active <- decathlon2[1:23, 1:10]
head(decathlon2.active[, 1:6])

res.pca <- prcomp(decathlon2.active, scale = TRUE)

fviz_eig(res.pca)

fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

eig.val <- get_eigenvalue(res.pca)
res.var <- get_pca_var(res.pca)
res.ind <- get_pca_ind(res.pca)

ind.sup <- decathlon2[24:27, 1:10]
ind.sup[, 1:6]
ind.sup.coord <- predict(res.pca, newdata = decathlon2.active)
ind.sup.coord[, 1:4]
p <- fviz_pca_ind(res.pca, repel = TRUE)
# Add supplementary individuals
fviz_add(p, ind.sup.coord, color ="blue")
## ^this works

# try again
set.seed(225)
res.pca <- prcomp(records, scale = TRUE)

fviz_eig(res.pca)

fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

eig.val <- get_eigenvalue(res.pca)
res.var <- get_pca_var(res.pca)
res.ind <- get_pca_ind(res.pca)
ind.sup.coord <- predict(res.pca, newdata = records)
p <- fviz_pca_ind(res.pca, repel = TRUE)
# Add supplementary individuals
fviz_add(p, ind.sup.coord, color ="blue")
## ^this works. Issue is with the earlier application of decostand()

# decostand transformation code
records = iris[,c(1:4)]
tmp <- apply(records, 2, min)
ran <- apply(records, 2, max)
ran <- ran - tmp
if (any(records < 0)) {
  k <- min(records)
  if (method %in% c("total", "frequency", "pa", "chi.square", "rank",
                    "rrank")) {
    warning("input data contains negative entries: result may be non-sense\n")
  }
} else {k <- .Machine$double.eps}
k = .Machine$double.eps
ran <- pmax(k, ran)
records <- sweep(records, 2, tmp, "-")
records <- sweep(records, 2, ran, "/")
set.seed(225)
# try prediction
scaled = scale(records,
               center = mod$center,
               scale = mod$scale)
coord_fun = function(ind, loadings){
  r = loadings*ind
  apply(r, 2, sum)
}
pca.loadings = mod$rotation
scaled.coord = t(apply(scaled, 1, coord_fun, pca.loadings))
head(scaled.coord)
head(mod$x)

#----------------------- PCA of subsample --------------------
library(tidyverse)
library(data.table)
library(vegan)
library(factoextra)

setwd("/glade/scratch/kjfuller/data")

# select multithread; read in data
setDTthreads(32)
t1 = system.time({
  pca1 = data.table::fread("allvalues_forPCA8.csv")
  pca1 = as.data.frame(pca1)
})

# remove non-complete cases and record change in nrow()
n1 = nrow(pca1)
pca1 = na.omit(pca1)
n2 = nrow(pca1)

# write na.omit(df) to file
data.table::fwrite(pca1, "allvalues_forPCA8_na.omit.csv")

# sample 10,000 rows, standardize ranges and run PCA
set.seed(225)
pca1 = pca1[sample(1:nrow(pca1), 10000, replace = FALSE),]
t2 = system.time({
  pca2 = decostand(pca1[, c(1, 2, 4:ncol(pca1))], method = "range")
  mod = prcomp(pca2, scale = T)
})
rm(pca2)

# figures
tiff("PCA1_subsample_fig1.tiff", width = 500, height = 500, res = 100)
fviz_eig(mod)
dev.off()
tiff("PCA1_subsample_fig2.tiff", width = 500, height = 500, res = 100)
fviz_pca_var(mod,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
)
dev.off()

# stats
eigval = get_eigenvalue(mod)
write.csv(eigval, "PCA1_axesstats.csv")
res.var = get_pca_var(mod)
v1 = data.frame(res.var$coord)
v2 = data.frame(res.var$cor)
v3 = data.frame(res.var$cos2)
vars = cbind(v1, v2, v3)
nom = c("coords", "contrib", "rep")
for(a in c(1:3)){
  for(i in c(1:length(names(v1)))){
    names(vars)[i+length(names(v1))*(a-1)] = paste0(nom[a], i)
  }
}
write.csv(vars, "PCA1_stats.csv")
res.ind = get_pca_ind(mod)
i1 = data.frame(res.ind$contrib)
i2 = data.frame(res.ind$cos2)
inds = cbind(i1, i2)
nom = c("contrib", "rep")
for(a in c(1:2)){
  for(i in c(1:length(names(i1)))){
    names(inds)[i+length(names(i1))*(a-1)] = paste0(nom[a], i)
  }
}

# store model outputs
sco = data.frame(scores(mod))
for(a in c(1:ncol(scores(mod)))){
  pca1 = cbind(pca1, data.frame(sco[a]))
}
pca1 = cbind(pca1, inds)
rm(sco)
data.table::fwrite(iris3, "PCA1_subsample.csv")

# write stats outputs to file; timing and nrow()
capture.output(
  paste0("time to read forPCA8.csv = ", t1),
  paste0("rows of data = ", n1),
  paste0("rows after removing NaNs = ", n2),
  paste0("time to transform and run a PCA on 10,000 rows = ", t2),
  file = "stats_forPCA8.txt"
)


#----------------------- PCA of all data --------------------
library(tidyverse)
library(data.table)
library(vegan)
library(factoextra)

setwd("/glade/scratch/kjfuller/data")

# select multithread; read in data
setDTthreads(36)
t1 = system.time({
  df = data.table::fread("allvalues_forPCA8_na.omit.csv")
  df = as.data.frame(df)
})[[3]]

# run PCA
set.seed(225)
t2 = system.time({
  pca2 = decostand(df[, c(1, 2, 4:ncol(df))], method = "range")
  mod = prcomp(pca2, scale = T)
})
rm(pca2)

# figures
tiff("PCA1_subsample_fig1.tiff", width = 500, height = 500, res = 100)
fviz_eig(mod)
dev.off()
tiff("PCA1_subsample_fig2.tiff", width = 500, height = 500, res = 100)
fviz_pca_var(mod,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
)
dev.off()

# stats
eigval = get_eigenvalue(mod)
write.csv(eigval, "PCA1_eigenstats.csv")
res.var = get_pca_var(mod)
v1 = data.frame(res.var$coord)
v2 = data.frame(res.var$cor)
v3 = data.frame(res.var$cos2)
vars = cbind(v1, v2, v3)
nom = c("coords", "contrib", "rep")
for(a in c(1:3)){
  for(i in c(1:length(names(v1)))){
    names(vars)[i+length(names(v1))*(a-1)] = paste0(nom[a], i)
  }
}
write.csv(vars, "PCA_varstats.csv")
res.ind = get_pca_ind(mod)
i1 = data.frame(res.ind$contrib)
i2 = data.frame(res.ind$cos2)
inds = cbind(i1, i2)
nom = c("contrib", "rep")
for(a in c(1:2)){
  for(i in c(1:length(names(i1)))){
    names(inds)[i+length(names(i1))*(a-1)] = paste0(nom[a], i)
  }
}

# store model outputs
sco = data.frame(scores(mod))
df2 = df[,c(1:3)]
for(a in c(1:ncol(scores(mod)))){
  df2 = cbind(df2, data.frame(sco[a]))
}
df2 = cbind(df2, inds)
rm(sco)
data.table::fwrite(df2, "PCA_values.csv")

# write metadata outputs to file; timing and nrow()
capture.output(
  paste0("time to read forPCA8.csv = ", t1),
  paste0("time to transform and run a PCA on all rows = ", t2),
  file = "PCA_metastats.txt"
)


#-----------------------PCA redo2 ---------------------------
library(tidyverse)
library(data.table)
library(vegan)
library(factoextra)

setwd("/glade/scratch/kjfuller/data")

# select multithread; read in data
setDTthreads(18)
t1 = system.time({
  df = data.table::fread("allvalues_forPCA8_na.omit.csv")
  df = as.data.frame(df)
})[[3]]

# run PCA
set.seed(225)
t2 = system.time({
  pca2 = decostand(df[, c(1, 2, 4:ncol(df))], method = "range")
  mod = prcomp(pca2, scale = T)
})
rm(pca2)

# stats
res.var = get_pca_var(mod)
v1 = data.frame(res.var$coord)
v2 = data.frame(res.var$cor)
v3 = data.frame(res.var$cos2)
v4 = data.frame(res.var$contrib)
vars = cbind(v1, v2, v3, v4)
nom = c("coords", "corr", "cos2", "contrib")
for(a in c(1:4)){
  for(i in c(1:length(names(v1)))){
    names(vars)[i+length(names(v1))*(a-1)] = paste0(nom[a], i)
  }
}
write.csv(vars, "PCA_varstats.csv")

# write metadata outputs to file; timing and nrow()
capture.output(
  paste0("time to read forPCA8.csv = ", t1),
  paste0("time to transform and run a PCA on all rows = ", t2),
  file = "PCA_metastats.txt"
)

#-------------------- extract PCA values for site data -------------------------
library(tidyverse)
library(data.table)

setwd("/glade/scratch/kjfuller/data")

# assign number of cores and read in data
setDTthreads(18)
t1 = system.time({
  df = data.table::fread("PCA_values1-14.csv", select = c(1:17))
  n1 = nrow(df)
})[[3]]

t2 = system.time({
  df2 = data.table::fread("alltraits_site-specific.csv", select = c(1:2, 5:8, 10:11))
  names(df2)[c(7:8)] = c("x", "y")
  n2 = nrow(df2)
})[[3]]

# write metadata outputs to file
capture.output(
  paste0("time to read 14 columns of PCA_values1-14 = ", t1),
  paste0("nrow() of PCA_values1-14 = ", n1),
  paste0("time to read 8 columns of site traits = ", t2),
  paste0("nrow() of site traits df = ", n2),
  file = "PCA_extract_metastats.txt"
)

t3 = system.time({
  traits_pca = left_join(df2, df)
  n3 = nrow(traits_pca)
  data.table::fwrite(traits_pca, "alltraits_site-specific_PCAs.csv")
})[[3]]

# write additional metadata outputs to file
capture.output(
  paste0("time to join dfs and write to file = ", t3),
  paste0("nrow() of joined df = ", n3),
  file = "PCA_extract_metastats.txt",
  append = TRUE
)

## failed, coordinates do not match because coords for extracted raster falues are at cell centers

#----------------------- PCA_redo3 -----------------
library(tidyverse)
library(data.table)
library(vegan)
library(factoextra)

setwd("/glade/scratch/kjfuller/data")

# select multithread; read in data
setDTthreads(18)
t1 = system.time({
  df = data.table::fread("allvalues_forPCA8_na.omit.csv")
  df = as.data.frame(df)
})[[3]]

# run PCA
set.seed(225)
t2 = system.time({
  pca2 = decostand(df[, c(1, 2, 4:ncol(df))], method = "range")
  mod = prcomp(pca2, scale = T)
})
rm(pca2)

# stats
v1 = data.frame(mod$center)
v2 = data.frame(mod$scale)
v3 = data.frame(mod$rotation)
vars = cbind(v1, v2, v3)
write.csv(vars, "PCA_predict.csv")

# capture output in case of issues
capture.output(
  mod$center,
  mod$scale,
  mod$rotation,
  file = "PCA_redo3.txt"
)

#----------------------- extract variable values at site locations ---------------------
library(raster)
library(snow)
library(parallel)
library(sf)
library(tidyverse)
library(data.table)

setwd("/glade/scratch/kjfuller/data")

beginCluster(n = 36, type = "SOCK")
# load datasets
veg = raster("for_fuels_30m.tif")
records = st_read("sitetraits.shp")
mask = list.files("./vars", pattern = "^mask", recursive = FALSE, full.names = FALSE)
m = raster(mask[1])
for(i in c(2:length(mask))){
  x = raster(mask[i])
  m = raster::stack(m, x)
}

# extract values and cbind to records sf
records = cbind(records, 
                raster::extract(m, st_coordinates(records), methods = 'simple'))
endCluster()

# convert to df and write to disk
records$lon = st_coordinates(records)[,1]
records$lat = st_coordinates(records)[,2]
st_geometry(records) = NULL
data.table::fwrite(records, "allsites_30m.csv")

#---------------------- convert vars to PCA -------------------------
library(tidyverse)
library(data.table)

setwd("/glade/scratch/kjfuller/data")

# read in data
records = read.csv("allsites_30m.csv")
# save file for non-numeric values
records = na.omit(records)
names(records)[137:138] = c("x", "y")
write.csv(records, "allsites_30m_na.omit.csv", row.names = FALSE)
# remove non-numeric columns
records = as.matrix(records[,c(137, 138, 7:136)])
all = data.table::fread("allvalues_forPCA8_na.omit.csv")
# remove fuels data
all = as.data.frame(all[,c(1, 2, 4:133)])
all = as.matrix(all)
# calculate scaling filters
tmp = apply(all, 2, min)
ran = apply(all, 2, max)
ran = ran - tmp
k = min(all)
# subsample original data for PCA conversion test
rec2 = all[c(1:100),]
rm(all)
ran = pmax(k, ran)
# scale data according to decostand function
records = sweep(records, 2, tmp, "-")
records = sweep(records, 2, ran, "/")

# load PCA conversion values
mod = read.csv("PCA_predict.csv")

# save stats
capture.output(
  paste0("number of rows of input df = ", nrow(records)),
  file = "PCA_predict_monitoring.txt"
)

# scale data according to PCA scale
scaled = scale(records,
               center = mod$mod.center,
               scale = mod$mod.scale)

# save stats
capture.output(
  paste0("number of rows of input df after scaling = ", nrow(scaled)),
  file = "PCA_predict_monitoring.txt",
  append = TRUE
)

# convert to PCA values and save
coord_fun = function(ind, loadings){
  r = loadings*ind
  apply(r, 2, sum)
}
pca.loadings = mod[,c(4:ncol(mod))]
scaled.coord = t(apply(scaled, 1, coord_fun, pca.loadings))
data.table::fwrite(scaled.coord, file = "allsites_PCA_30m.csv")

# save stats
capture.output(
  paste0("number of rows of output df = ", nrow(scaled.coord)),
  file = "PCA_predict_monitoring.txt",
  append = TRUE
)

# run same procedure for test data
rec2 = sweep(rec2, 2, tmp, "-")
rec2 = sweep(rec2, 2, ran, "/")
scaled = scale(rec2,
               center = mod$mod.center,
               scale = mod$mod.scale)
coord_fun = function(ind, loadings){
  r = loadings*ind
  apply(r, 2, sum)
}
pca.loadings = mod[,c(4:ncol(mod))]
scaled.coord = t(apply(scaled, 1, coord_fun, pca.loadings))
data.table::fwrite(scaled.coord, file = "predict_test.csv")
rm(list = ls())

# load original PCA values for test comparison
test = data.table::fread("PCA_values.csv", nrow = 100)
data.table::fwrite(test, file = "predict_sample.csv")

#--------------------- PCA redo4_redo ------------------------
#----------------------- PCA_redo3 -----------------
library(tidyverse)
library(data.table)
library(vegan)
library(factoextra)

setwd("/glade/scratch/kjfuller/data")

# assign number of cores and read in data
setDTthreads(36)
t1 = system.time({
  df = data.table::fread("allvalues_forPCA8_na.omit.csv")
  df = as.data.frame(df[,c(1, 2, 4:57, 124:132, 58:96, 133, 97:123)])
  df2 = data.table::fread("allsites_30m_na.omit.csv")
  df2 = as.data.frame(df2[,c(137, 138, 7:136)])
  df = rbind(df, df2)
  df$ID = c(1:nrow(df))
})[[3]]

# run PCA
set.seed(225)
t2 = system.time({
  pca2 = decostand(df[,c(1:(ncol(df)-1))], method = "range")
  mod = prcomp(pca2, scale = T)
})[[3]]
rm(pca2)

## variable stats
eigval = get_eigenvalue(mod)
write.csv(eigval, "PCA4_eigenstats.csv")
res.var = get_pca_var(mod)
v1 = data.frame(res.var$coord)
v2 = data.frame(res.var$cor)
v3 = data.frame(res.var$cos2)
v4 = data.frame(res.var$contrib)
vars = cbind(v1, v2, v3, v4)
nom = c("coords", "corr", "cos2", "contrib")
for(a in c(1:4)){
  for(i in c(1:length(names(v1)))){
    names(vars)[i+length(names(v1))*(a-1)] = paste0(nom[a], i)
  }
}
write.csv(vars, "PCA4_varstats.csv")

# prediction stats
v1 = data.frame(mod$center)
v2 = data.frame(mod$scale)
v3 = data.frame(mod$rotation)
vars = cbind(v1, v2, v3)
write.csv(vars, "PCA4_predict.csv")
rm(v1, v2, v3, v4, vars)

# sites
res.ind = get_pca_ind(mod)
i1 = data.frame(res.ind$contrib)
i2 = data.frame(res.ind$cos2)
ind = cbind(i1, i2)
nom = c("contrib", "rep")
for(a in c(1:2)){
  for(i in c(1:length(names(i1)))){
    names(ind)[i+length(names(i1))*(a-1)] = paste0(nom[a], i)
  }
}
df2 = data.frame(ID = df[,c(1, 2, col(df))])
ind = cbind(df2, ind)
data.table::fwrite(ind, "PCA4_indstats.csv")
rm(ind)

# score model outputs
sco = data.frame(scores(mod))
for(a in c(1:ncol(scores(mod)))){
  df2 = cbind(df2, data.frame(sco[a]))
}
rm(sco)
data.table::fwrite(df2, "PCA4_values.csv")

capture.output(
  paste0("time to read and bind dfs = ", t1),
  paste0("time to run PCA on values and sites = ", t2),
  paste0("output rows = ", nrow(df2)),
  file = "PCA4_monitoring.txt"
)


#---------------------- testing rasterToPoints() and back ----------------------
library(raster)
library(sf)
library(tidyverse)
library(tmap)

r_precip <- raster("data/ASCIIgrid.txt")
crs(r_precip) <- '+init=EPSG:4326' # WGS84
r_precip 
aus = getData("GADM", country = "AUS", level = 1)
precip = mask(r_precip, aus)
res(precip)
precip = aggregate(precip, fact = 10)
df = as.data.frame(rasterToPoints(precip), xy = TRUE)
df = df %>% 
  st_as_sf(coords = c("x", "y"), crs = st_crs(precip))

tm_shape(df)+tm_dots()

values(precip) = NA
df = rasterize(df, precip, field = names(df)[1], fun = mean)
tm_shape(df)+tm_raster()
## ^ this works just fine



#------------------- testing raster tiling method ---------------
library(raster)
library(sf)
library(tidyverse)
library(devtools)
# install_github("https://github.com/cran/GSIF")
library(GSIF)
library(rgdal)
library(gdalUtils)

# load files
veg = raster("data/fuels_30m.tif")

# create test data
r_precip <- raster("data/ASCIIgrid.txt")
crs(r_precip) <- '+init=EPSG:4326' # WGS84
r_precip = projectRaster(r_precip, crs = crs(veg))
aus = getData("GADM", country = "AUS", level = 1) %>% 
  st_as_sf() %>% 
  st_transform(crs = st_crs(veg))
precip = mask(r_precip, aus)
res(precip)
precip = aggregate(precip, fact = 10)
writeRaster(precip, "data/testtif.tif", overwrite = TRUE)

fn = raster("data/testtif.tif")
obj = rgdal::GDALinfo("data/testtif.tif")
## block.x seems to be based on the crs; number is the squared size of each tile in coordinates units
tiles.pol = GSIF::getSpatialTiles(obj, block.x = 750000, return.SpatialPolygons = TRUE)
n = length(tiles.pol)
tiles = GSIF::getSpatialTiles(obj, block.x = 750000, return.SpatialPolygons = FALSE)
tile.pol = SpatialPolygonsDataFrame(tiles.pol, tiles)
plot(fn)
lines(tile.pol)

tilefun = function(x){
  r = raster::crop(precip, tiles.pol[x])
  tryCatch({
    r = as(r, "SpatialPixelsDataFrame")
    writeGDAL(r, paste0("test", x, ".tif"), drivername = "GTIFF", type = "Float32")
  }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
}

lapply(c(1:n), tilefun)

df = data.table::fread("data/alltraits_site-specific.csv", select = c(1:2, 5:8, 10:11))
names(df)[c(7:8)] = c("x", "y")
df = st_as_sf(df, coords = c("x", "y"), crs = st_crs(veg))
df = df[c(1:1000),]
rastfun = function(x){
  tryCatch({
    r = raster(paste0("test", x, ".tif"))
    df = rasterize(df, r, field = 1, fun = mean)
    df = as(df, "SpatialPixelsDataFrame")
    writeGDAL(df, paste0("PC", x, ".tif"), drivername = "GTIFF", type = "Float32")
  }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
}

lapply(c(1:n), rastfun)

r = raster("PC13.tif")
r2 = raster("PC20.tif")
r3 = raster("PC21.tif")
r4 = raster("PC28.tif")
r = raster::mosaic(r, r2, r3, r4, fun = mean)
plot(r)

#---------- tile PCA data -------------
library(raster)
library(sf)
library(tidyverse)
library(devtools)
# install_github("https://github.com/cran/GSIF")
library(GSIF)
library(rgdal)
library(gdalUtils)

# load files
veg = raster("fuels_30m.tif")
values(veg) = NA
nsw = st_read("NSW_sans_islands.shp") %>% 
  st_transform(crs = st_crs(veg))
df = data.table::fread("PCA_values1-14.csv", select = c(1:2, 4))
df = st_as_sf(df, coords = c("x", "y"), crs = st_crs(veg))

obj = rgdal::GDALinfo("fuels_30m.tif")
## block.x seems to be based on the crs; number is the squared size of each tile in coordinates units
tiles.pol = GSIF::getSpatialTiles(obj, block.x = 150000, return.SpatialPolygons = TRUE)
n = length(tiles.pol)
tiles = GSIF::getSpatialTiles(obj, block.x = 150000, return.SpatialPolygons = FALSE)
tile.pol = SpatialPolygonsDataFrame(tiles.pol, tiles)

df2 = data.table::fread("alltraits_site-specific.csv", select = c(1:2, 5:8, 10:11))
names(df2)[c(7:8)] = c("x", "y")
df2 = st_as_sf(df2, coords = c("x", "y"), crs = st_crs(veg))
# df = df[c(1:1000),]
rastfun = function(x){
  tryCatch({
    r = raster(tile.pol[x,], res = res(veg), crs = crs(veg))
    df3 = rasterize(df, r, field = names(df)[1], fun = mean)
    writeRaster(df3, paste0("PC01_", x, ".tif"))
  }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
}

lapply(c(1:n), rastfun)



#-------------------- PC values for RFs -------------------
library(data.table)
library(tidyverse)
library(sf)
library(raster)

setwd("/glade/scratch/kjfuller/data")

PC = data.table::fread("PCA4_values1.csv", select = c(1:17))
nrow(PC)
## 167235707
data.table::fwrite(PC, "PCA4_values1-14.csv")
PC = PC[c(167211707:167235707),]
sites = read.csv("alltraits_site-specific.csv")
nrow(sites)
## 33376
nrow(na.omit(sites))
## 12659
sites = sites[, c(1:2, 5:8, 10:11)]
names(sites)[7:8] = c("x", "y")
names(PC)[1:2] = c("x", "y")
sites = left_join(sites, PC)
nrow(sites)
## 173,298
sites = unique(sites)
nrow(sites)
## 91,113
length(unique(sites$x, sites$y))
## 9,462
sites = sites %>% 
  dplyr::select(-ID.ID)
sites = unique(sites)
nrow(sites)
## 20,630
sites = sites[complete.cases(sites$PC1)]
nrow(sites)
## 14,210
length(unique(sites$x, sites$y))
## 5949
ssf = st_read("sitetraits.shp")
nrow(ssf)
## 33,376
ssf = unique(ssf)
nrow(ssf)
## 20,630
length(unique(ssf$geometry))
## 9,462

nrow(sites)
## 14,210
names(sites) = c("id", "date", "spp", "b1", "b2", "ribbons", "x", "y", "pc01":"pc14")
any(is.na(sites))
## FALSE

data.table::fwrite(sites, "PCA4_sitesforRF.csv")

length(unique(sites$spp))
## 178
spp = sites %>% 
  group_by(spp) %>% 
  tally() %>% 
  as.data.frame()
nrow(spp[spp$n > 5,])
## 133
nrow(spp[spp$n > 10,])
## 112
nrow(spp[spp$n > 15,])
## 106
nrow(spp[spp$n > 25,])
## 90
nrow(spp[spp$n > 100,])
## 45
data.table::fwrite(spp, "species_summary_forRF.csv")

length(unique(sites$b1))
## 10
b1 = sites %>% 
  group_by(b1) %>% 
  tally() %>% 
  as.data.frame()
data.table::fwrite(b1, "bark1_summary_forRF.csv")

length(unique(sites$b2))
## 6
b2 = sites %>% 
  group_by(b2) %>% 
  tally() %>% 
  as.data.frame()
data.table::fwrite(b2, "bark2_summary_forRF.csv")

veg = raster("fuels_30m.tif")
nsw = st_read("NSW_sans_islands.shp") %>% 
  st_transform(crs = st_crs(veg))
sites = sites %>% 
  st_as_sf(coords = c("x", "y"), crs = st_crs(veg))

tiff("spp_distribution_forRF.tiff", width = 500, height = 500, res = 100, units = "px")
plot(st_geometry(nsw))
plot(st_geometry(sites), add = TRUE)
dev.off()

st_write(sites, "sitetraits_forRF.shp")

