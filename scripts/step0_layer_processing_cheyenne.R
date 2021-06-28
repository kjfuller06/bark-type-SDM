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


#--------------------- re-reproject SRADTot rasters -> boo -----------------------
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
## ^this caused considerable problems and had to be redone

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

#----------------- last check before PCA -----------------
library(data.table)
library(tidyverse)

setwd("/glade/scratch/kjfuller/data")

vars = list.files("./forPCA", full.names = TRUE)

a = data.table::fread(vars[1])
input = as.data.frame(a)
t1 = system.time({
  for(i in c(2:130)){
    a = data.table::fread(vars[i])
    input = full_join(input, as.data.frame(a))
  }
  NaNs = sum(is.na(input))
  input = na.omit(input)
})

# input stats
capture.output(
  paste0("time to read and rbind datasets = ", t1),
  paste0("nrow(input) before scaling = ", nrow(input)),
  paste0("rows removed by na.omit = ", NaNs),
  paste0("input min(x) = ", min(input$x)),
  paste0("input max(x) = ", max(input$x)),
  paste0("input min(y) = ", min(input$y)),
  paste0("input max(y) = ", max(input$y)),
  file = "PCAinput_stats.txt"
)

#--------------------- PCA final ------------------------
start = Sys.time()
t0 = system.time({
  library(tidyverse)
  library(data.table)
  library(vegan)
  library(factoextra)
  
  setwd("/glade/scratch/kjfuller/data")
  
  # assign number of cores and read in data
  setDTthreads(36)
  input = data.table::fread("allvalues_forPCA_na.omit.csv")
  input$ID = c(1:nrow(input))
  xyd = input[,c(1, 2, ncol(input))]
})[[3]]

# input stats
x1 = min(input$x)
x2 = max(input$x)
y1 = min(input$y)
y2 = max(input$y)

capture.output(
  paste0("start of script = ", start),
  paste0("setup time, including loading data = ", t0),
  paste0("nrow(input) before scaling = ", nrow(input)),
  paste0("input min(x) = ", x1),
  paste0("input max(x) = ", x2),
  paste0("input min(y) = ", y1),
  paste0("input max(y) = ", y2),
  file = "PCA_notes.txt"
)

# scale values and write to file
set.seed(225)
t1 = system.time({
  scaled = decostand(input[,c(1:(ncol(input)-1))], method = "range")
})[[3]]
rm(input)
index = list(1:14, 15:40, 41:65, 66:90, 91:115, 116:ncol(scaled))

t2 = system.time({
for(i in c(1:6)){
  setDTthreads(36)
  data.table::fwrite(scaled[,index[[i]]], paste0("PCA_scaledinputs", i, ".csv"))
}
})[[3]]

# scaled input stats
x1 = min(scaled$x)
x2 = max(scaled$x)
y1 = min(scaled$y)
y2 = max(scaled$y)

capture.output(
  paste0("time to scale variables = ", t1),
  paste0("nrow(input) after scaling = ", nrow(scaled)),
  paste0("scaled input min(x) = ", x1),
  paste0("scaled input max(x) = ", x2),
  paste0("scaled input min(y) = ", y1),
  paste0("scaled input max(y) = ", y2),
  paste0("time to write scaled inputs = ", t2),
  file = "PCA_notes.txt",
  append = TRUE
)

# run PCA
set.seed(225)
t3 = system.time({
  mod = prcomp(scaled, scale = T)
})[[3]]
rm(scaled)

capture.output(
  paste0("time to run model = ", t3),
  file = "PCA_notes.txt",
  append = TRUE
)

# score outputs
t4 = system.time({
  sco = as.data.frame(scores(mod))
  sco = cbind(xyd, sco)
  for(i in c(1:6)){
    setDTthreads(36)
    data.table::fwrite(sco[,index[[i]]], paste0("PCA_values", i, ".csv"))
  }
})[[3]]
rm(sco)

capture.output(
  paste0("time to extract and write scores = ", t4),
  file = "PCA_notes.txt",
  append = TRUE
)

# figures
tiff("PCA_eigvalfig.tiff", width = 500, height = 500, res = 100)
fviz_eig(mod)
dev.off()
tiff("PCA_sitecos2.tiff", width = 500, height = 500, res = 100)
fviz_pca_ind(mod,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
dev.off()
tiff("PCA_varfig.tiff", width = 500, height = 500, res = 100)
fviz_pca_var(mod,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
)
dev.off()

# site stats
t5 = system.time({
  res.ind = get_pca_ind(mod)
  i1 = data.frame(res.ind$contrib)
  i2 = data.frame(res.ind$cos2)
  nom = c("contrib.", "cos2.")
  for(i in c(1:length(names(i1)))){
    names(i1)[i] = paste(nom[1], i)
    names(i2)[i] = paste(nom[2], i)
  }
  i1 = cbind(xyd, i1)
  i2 = cbind(xyd, i2)
  
  setDTthreads(36)
  for(i in c(1:6)){
    data.table::fwrite(i1[,index[[i]]], paste0("PCA_sitecontrib", i, ".csv"))
  }
  setDTthreads(36)
  for(i in c(1:6)){
    data.table::fwrite(i2[,index[[i]]], paste0("PCA_sitecos2", i, ".csv"))
  }
})[[3]]
rm(res.ind, i1, i2)

capture.output(
  paste0("time to extract and write site stats = ", t5),
  file = "PCA_notes.txt",
  append = TRUE
)

## variable stats
t6 = system.time({
  eigval = get_eigenvalue(mod)
  write.csv(eigval, "PCA_eigenvalues.csv")
  rm(eigval)
  res.var = get_pca_var(mod)
  v1 = data.frame(res.var$coord)
  v2 = data.frame(res.var$cor)
  v3 = data.frame(res.var$cos2)
  v4 = data.frame(res.var$contrib)
  vars = list(v1, v2, v3, v4)
  nom = c("coords.", "corr.", "cos2.", "contrib.")
  for(a in c(1:4)){
    for(i in c(1:length(names(v1)))){
      names(vars[[a]])[i] = paste0(nom[a], i)
    }
  }
  for(a in c(1:4)){
    data.table::fwrite(vars[[i]], paste0("PCA_var", nom[a], ".csv"))
  }
  rm(res.var)
  
  # prediction stats
  v1 = data.frame(mod$center)
  v2 = data.frame(mod$scale)
  v3 = data.frame(mod$rotation)
  vars = cbind(v1, v2, v3)
  write.csv(vars, "PCA_predict.csv")
})[[3]]

capture.output(
  paste0("time to extract and write remaining stats = ", t6),
  file = "PCA_notes.txt",
  append = TRUE
)

#---------------------- testing rasterToPoints() and back ----------------------
library(raster)
library(sf)
library(tidyverse)
library(tmap)

r_precip <- raster("data/ASCIIgrid.txt")
crs(r_precip) <- '+init=EPSG:4326' # WGS84
r_precip 
aus = readRDS("gadm36_AUS_1_sp.rds")
res(r_precip)
precip = aggregate(r_precip, fact = 10)
precip = mask(precip, aus)
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

precip = raster("data/testtif.tif")
df = as.data.frame(rasterToPoints(precip), xy = TRUE)
df = st_as_sf(df, coords = c("x", "y"), crs = st_crs(veg))
values(precip) = NA
obj = rgdal::GDALinfo("data/testtif.tif")
## block.x seems to be based on the crs; number is the squared size of each tile in coordinates units
tiles.pol = GSIF::getSpatialTiles(obj, block.x = 750000, return.SpatialPolygons = TRUE)
n = length(tiles.pol)
tiles = GSIF::getSpatialTiles(obj, block.x = 750000, return.SpatialPolygons = FALSE)
tile.pol = SpatialPolygonsDataFrame(tiles.pol, tiles)
plot(precip)
lines(tile.pol)

tilefun = function(x){
  r = raster::crop(precip, tiles.pol[21])
  tryCatch({
    r = as(r, "SpatialPixelsDataFrame")
    writeGDAL(r, "test21.tif", drivername = "GTIFF", type = "Float32")
  }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
}

lapply(21, tilefun)
lapply(c(1:n), tilefun)

rastfun = function(x){
  tryCatch({
    clip = st_as_sf(tile.pol[6,])
    df3 = df[clip,]
    rm(clip)
    r = raster(tile.pol[6,], res = res(precip))
    df3 = rasterize(df3, r, field = "testtif", fun = mean)
    df3 = as(df3, "SpatialPixelsDataFrame")
    writeGDAL(df3, paste0("PC", 6, ".tif"), drivername = "GTIFF", type = "Float32")
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
library(tidyverse)
library(data.table)
library(raster)
library(sf)
library(GSIF)
library(rgdal)
library(gdalUtils)

setwd("/glade/scratch/kjfuller/data")
veg = raster("for_fuels_30m.tif")
nsw = st_read("NSW_sans_islands.shp") %>%
  st_transform(crs = st_crs(veg))

# assign number of cores and read in data
setDTthreads(36)
t1 = system.time({
  df = data.table::fread("PCA_values1-14.csv", select = c(1:2, 4))
  df = as.data.frame(df)
  df = st_as_sf(df, coords = c("x", "y"), crs = st_crs(veg))
  nom = names(df)
  n1 = nrow(df)
})[[3]]

tiff("df_check.tiff", width = 500, height = 500, res = 100, units = "px")
plot(st_geometry(df))
dev.off()

obj = rgdal::GDALinfo("for_fuels_30m.tif")
t.spatial = GSIF::getSpatialTiles(obj, block.x = 150000, return.SpatialPolygons = TRUE)
t.non = GSIF::getSpatialTiles(obj, block.x = 150000, return.SpatialPolygons = FALSE)
tiles = SpatialPolygonsDataFrame(t.spatial, t.non)
tiles$id = c(1:nrow(tiles))
t.sf = st_as_sf(tiles)
t.sf = t.sf[nsw,]
tiles = tiles[tiles$id %in% t.sf$id,]
nt = nrow(tiles)

tiff("tiles_check.tiff", width = 500, height = 500, res = 100, units = "px")
plot(veg)
plot(st_geometry(t.sf), add = TRUE)
dev.off()

rm(t.spatial, t.non, t.sf)

# write metadata outputs to file
capture.output(
  paste0("time to read 3 columns of PCA_values1-14 and convert to sf = ", t1),
  paste0("names of df = ", list(nom)),
  paste0("nrow of df = ", n1),
  paste0("number of tiles = ", nt),
  file = "PC1_raster_metastats.txt",
  append = TRUE
)

resv = res(veg)
rastfun = function(x){
  tryCatch({
    r = raster(tiles[x,], res = resv)
    # df3 = rasterize(df, r, field = 1, fun = mean, update = TRUE)
    # writeRaster(df3, paste0("PC01_", x, "_field-1.tif"))
    df3 = rasterize(df, r, field = "PC1", fun = mean, update = TRUE)
    writeRaster(df3, paste0("PC01_", x, ".tif"), overwrite = TRUE)
  }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
}

testfun = function(x){
  tryCatch({
    r = raster(paste0("PC01_", x, ".tif"))
    tiff(paste0("PC01_", x, "_test.tiff"), width = 500, height = 500, res = 100, units = "px")
    plot(st_geometry(nsw))
    plot(r, add = TRUE)
    dev.off()
  }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
}

sfInit(parallel = TRUE, cpus = 36)
sfExport("tiles", "nsw", "resv", "df", "rastfun")
sfLibrary(raster)
sfLibrary(sf)

sfLapply(c(1:nt), rastfun)


sfStop()

#---------------- tracking down PCA value issues -----------------
library(data.table)

setwd("/glade/scratch/kjfuller/data")

setDTthreads(18)
df = data.table::fread("allvalues_forPCA8.csv")

summary = data.frame(cols = names(df),
                     NaNs = NA)

for(i in c(1:ncol(df))){
  summary$NaNs[i] = sum(is.na(df[,i]))
}

data.table::fwrite(summary, "forPCA8_checkNaNs.csv")

capture.output(
  paste0("nrow(df) = ", nrow(df)),
  paste0("min(x) = ", min(df$x)),
  paste0("max(x) = ", max(df$x)),
  paste0("min(y) = ", min(df$y)),
  paste0("max(y) = ", max(df$y)),
  file = "forPCA8_checkvalues.txt"
)

capture.output(
  summary(df),
  file = "forPCA8_summaryoutput.txt"
)

## values are missing for all y's under 4638061; full NSW range extends to 4027276


#------------------- rasterToPoints_redo ---------------------
library(raster)
library(sf)
library(parallel)
library(snowfall)

capture.output(
  paste0("libraries loaded"),
  file = "raster_extract_notes.txt"
)

setwd("/glade/scratch/kjfuller/data")

mask = list.files("./", pattern = "^mask", recursive = FALSE, full.names = TRUE)

df_fun = function(x){
  r = raster(mask[x])
  n1 = sum(is.na(values(r)))
  df = as.data.frame(rasterToPoints(r), xy = TRUE)
  n2 = sum(is.na(df))
  lab = substr(mask[x], 14, nchar(mask[x])-4)
  write.csv(df, paste0(lab, "_forPCA.csv"), row.names = FALSE)
  capture.output(
    paste0("nrow of ", lab, " = ", nrow(df)),
    paste0("NaNs in raster of ", lab, " = ", n1),
    paste0("NaNs in df of ", lab, " = ", n2),
    file = paste0("raster_extract_", lab, ".txt")
  )
}

capture.output(
  paste0("mask list and function loaded"),
  paste0("initiating snowfall"),
  file = "raster_extract_notes.txt",
  append = TRUE
)

sfInit(parallel = TRUE, cpus = 36)
sfExport("mask", "df_fun")
sfLibrary(raster)
sfLibrary(sf)

sfLapply(c(1:length(mask)), df_fun)

sfStop()

#------------------- rasterToPointsfinal ha! ---------------------
library(raster)
library(sf)
library(parallel)
library(snowfall)

setwd("/glade/scratch/kjfuller/data")

mask = list.files("./masked", pattern = "^mask", recursive = FALSE, full.names = TRUE)
veg = raster("for_fuels_30m.tif")
c1 = st_crs(veg)
veg = as.data.frame(rasterToPoints(veg), xy = TRUE) %>% 
  dplyr::select(x, y)
veg = st_as_sf(veg, coords = c("x", "y"), crs = c1)

df_fun = function(x){
  r = raster(mask[x])
  n1 = sum(is.na(values(r)))
  lab = substr(mask[x], 20, nchar(mask[x])-4)
  veg = cbind(veg, 
              raster::extract(r, st_coordinates(veg), methods = 'simple'))
  veg$lon = st_coordinates(veg)[,1]
  veg$lat = st_coordinates(veg)[,2]
  st_geometry(veg) = NULL
  names(veg)[3] = paste0(lab)
  n2 = sum(is.na(veg[,3]))
  data.table::fwrite(veg, paste0(lab, "_forPCA2.csv"))
  a = data.frame(var = lab,
                 stat = c("min(x)", "max(x)", "min(y)", "max(y)", "nrow", "raster.nans", "df.nans"),
                 value = c(min(df$x), max(df$x), min(df$y), max(df$y), nrow(df), n1, n2))
  data.table::fwrite(a, paste0("checks/", lab, "_forPCA2_check.csv"))
}

sfInit(parallel = TRUE, cpus = 36)
sfExport("mask", "veg", "df_fun")
sfLibrary(raster)
sfLibrary(sf)

sfLapply(c(1:130), df_fun)

sfStop()

#------------------ check outputs-extracted data ----------------
library(data.table)
library(tidyverse)
library(sf)
library(raster)

setwd("/glade/scratch/kjfuller/data")

mask = list.files("./", pattern = "^_proj", recursive = FALSE, full.names = TRUE)

df_fun = function(x){
  df = data.table::fread(mask[x], select = c(1:2))
  lab = substr(mask[x], 5, nchar(mask[x]) - 11)
  capture.output(
    paste0("min(x) of ", lab, " = ", min(df$x)),
    paste0("max(x) of ", lab, " = ", max(df$x)),
    paste0("min(y) of ", lab, " = ", min(df$y)),
    paste0("max(y) of ", lab, " = ", max(df$y)),
    file = paste0("extract_", lab = ".txt"),
    append = TRUE
  )
}

sfInit(parallel = TRUE, cpus = 36)
sfExport("mask", "df_fun")
sfLibrary(data.table)

sfLapply(c(1:length(mask)), df_fun)

sfStop()

## additional script
meta = list.files("./", pattern = "extract_proj", recursive = FALSE, full.names = TRUE)

b = data.frame()
for(i in c(1:length(meta))){
  a = data.table::fread(meta[i], header = FALSE)
  a = as.data.frame(a)
  a$value = map_chr(str_split(a$V2, pattern = " = "), 2)
  a$var = substr(map_chr(str_split(a$V2, pattern = " = "), 1), 11, 50)
  a$stat = substr(a$V2, 1, 6)
  a = a %>% 
    dplyr::select(value, var, stat)
  b = rbind(b, a)
}
write.csv(b, "extract_stats.csv", row.names = FALSE)

dat = read.csv("extract_stats.csv")
dat %>% 
  filter(stat == "min(y)")
## all data have the correct range of values

#------------ collating output checks ---------------
meta = list.files("./", pattern = "raster_extract__", recursive = FALSE, full.names = TRUE)

b = data.frame()
for(i in c(1:length(meta))){
  a = data.table::fread(meta[i], header = FALSE)
  x = map_chr(str_split(a$V2, pattern = " = "), 2)
  a = data.frame(var = substr(map_chr(str_split(a$V2, pattern = " = "), 1), 10, 50)[1],
                 stat = c("nrow", "raster.nans", "df.nans"),
                 value = x)
  dat = rbind(dat, a)
}
dat = dat[order(dat$var),]
dat$value = as.numeric(dat$value)
dat$var = as.factor(dat$var)
dat$stat = as.factor(dat$stat)
write.csv(dat, "extract_stats.csv", row.names = FALSE)

xy = pivot_wider(dat %>% filter(stat == "min(y)" | stat == "min(x)" | stat == "max(y)" | stat == "max(x)"), names_from = stat, values_from = value)
x1 = unique(xy[,c(1, 3)])
x2 = unique(xy[,c(1, 4)])
x3 = unique(xy[,c(2, 3)])
x4 = unique(xy[,c(2, 4)])
names(x1) = c("x", "y")
names(x2) = c("x", "y")
names(x3) = c("x", "y")
names(x4) = c("x", "y")
df = rbind(x1, x2, x3, x4)
df = as.data.frame(df)
df = st_as_sf(df, coords = c("x", "y"), crs = st_crs(veg))
tiff("df_check.tiff", width = 500, height = 500, res = 100, units = "px")
plot(st_geometry(nsw))
plot(st_geometry(df))
dev.off()
st_write(df, "df_check.shp")

bio = dat %>% filter(stat == "min(x)") %>% filter(value != min(value))
summary(bio)

write.csv(b, "extract_stats.csv", row.names = FALSE)

#------------ 1. compare xy's of masked data ------------------
## additional checks
library(tidyverse)
library(raster)
library(sf)
library(data.table)

veg = raster("for_fuels_30m.tif")
c1 = crs(veg)
c2 = st_crs(veg)
rm(veg)
nsw = st_read("NSW_sans_islands.shp") %>% st_transform(crs = c2)

# variables to rasterize again to check distributions
# aridity is good
# bioclim is good

# dem nans:
# mask_proj_dem_aspect_30m = 1069904003 
# mask_proj_dem_plan_30m = 1069905955

dem1 = data.table::fread("_proj_dem_aspect_30m_forPCA.csv", select = c(1:2))
dem2 = data.table::fread("_proj_dem_plan_30m_forPCA.csv", select = c(1:2))
dem = anti_join(dem1, dem2)
dem = as.data.frame(dem)
dem = st_as_sf(dem, coords = c("x", "y"), crs = c2)

plotfun = function(name, df){
  tiff(paste0(name, "_check.tiff"), width = 500, height = 500, res = 100, units = "px")
  plot(st_geometry(nsw))
  plot(st_geometry(df), add = TRUE)
  dev.off()
}
plotfun("dem", dem)
## coastal difference; likely due to differing original source or loss of edge data during calculations
## ^difference is negligible, same NaNs as proj_dem_SRADTot_0115

#---------------- 1a. srads ----------------------
# srad nans:
# mask_proj_dem_SRADTot_0115_30m = 1069905955
# mask_proj_dem_SRADTot_0415_30m = 1069907438
srad1 = data.table::fread("_proj_dem_SRADTot_0115_30m_forPCA.csv", select = c(1:2))
srad2 = data.table::fread("_proj_dem_SRADTot_0415_30m_forPCA.csv", select = c(1:2))
srad = anti_join(srad1, srad2)
srad = as.data.frame(srad)
srad = st_as_sf(srad, coords = c("x", "y"), crs = c2)
plotfun("srad", srad)
## coastal difference; likely due to differing original source or loss of data due to calculations
## ^ not sure why just the coastline would be different between these two variables but it may be due to differences in the original data and not an issue I need to worry about
### ^solved; rasters were projected using different methods:
#   projectRaster = 1069905955
#   gdal = 1069907438
# >> reprojected rasters 4-12 with projectRaster() solved the problem

# ndvi nans:
# mask_proj_NDVI_30m = 1069975815
ndvi = data.table::fread("_proj_NDVI_30m_forPCA.csv", select = c(1:2))
ndvi = as.data.frame(ndvi)
ndvi = st_as_sf(ndvi, coords = c("x", "y"), crs = c2)
plotfun("ndvi", ndvi)
## took too long

# ------------------ 1b. soils -------------------
# soil nans:
# mask_proj_soilAWC_D1_30m = 1070070747
# mask_proj_soildes_30m = 1070069423
# mask_proj_soilPHC_D3_30m = 1070094838
## update takes soilPHC_D3 to 107007047 like the others; must have been my fault
soil1 = data.table::fread("_proj_soilAWC__D1_30m_forPCA.csv", select = c(1:2))
soil2 = data.table::fread("_proj_soildes_30m_forPCA.csv", select = c(1:2))
soil3 = data.table::fread("_proj_soilPHC_D3_30m_forPCA.csv", select = c(1:2))
soil = anti_join(soil2, soil1)
soil = as.data.frame(soil)
soil = st_as_sf(soil, coords = c("x", "y"), crs = c2)
plotfun("soil1", soil)
## this one is missing some suspicious points but it's all of the soil data layers besides the other two selected here

soil = anti_join(soil2, soil3)
soil = as.data.frame(soil)
soil = st_as_sf(soil, coords = c("x", "y"), crs = c2)
plotfun("soil3", soil)
## also coastal, lots of points but might be ok
## ^this one was my fault

# ------------- 1c. final check notes ----------------------
# last check with new PHC_D3 layer- examine overlap of NaNs between categories
# 1069904003 -> 1069905955
# mask_proj_dem_aspect_30m -> mask_proj_dem_plan_30m
# mask_proj_dem_aspect_30m -> mask_proj_dem_SRADTot_0115_30m
## mask_proj_dem_plan_30m == mask_proj_dem_SRADTot_0115_30m
## ^NaNs equal

# 1069905955 -> 1069907438
# mask_proj_dem_SRADTot_0115_30m -> mask_proj_dem_SRADTot_0415_30m
## ^problem***
### ^solved by reprojecting SRAD4-12; values now equal

# 1069907438 -> 1069975815
# mask_proj_dem_SRADTot_0415_30m -> mask_proj_NDVI_30m

# 1069975815 -> 1070069423
# mask_proj_NDVI_30m -> mask_proj_soildes_30m

# 1070069423 -> 1070070747
# mask_proj_soildes_30m -> mask_proj_soilAWC_D1_30m

# After full-mask of all data
# dem_aspect_30m NaNs = 1070130303; !is.na() = 327112007

#----------------- 1c. check dems ----------------------
dem1 = data.table::fread("_proj_dem_aspect_30m_forPCA.csv", select = c(1:2))
dem2 = data.table::fread("_proj_dem_plan_30m_forPCA.csv", select = c(1:2))
dem = anti_join(dem1, dem2)
srad1 = data.table::fread("_proj_dem_SRADTot_0115_30m_forPCA.csv", select = c(1:2))
dem.srad = anti_join(dem1, srad1)
diff = anti_join(dem, dem.srad)
## nrow(diff) = 0
diff = anti_join(dem.srad, dem)
## nrow(diff) = 0
diff = anti_join(dem2, srad1)
## nrow(diff) = 0
diff = anti_join(srad1, dem2)
## nrow(diff) = 0

#------------------- 1c. check srads + ndvi ---------------------
srad2 = data.table::fread("_proj_dem_SRADTot_0415_30m_forPCA.csv", select = c(1:2))
srad = anti_join(srad1, srad2)
## nrow(srad) = 1,499
dars = anti_join(srad2, srad1)
## nrow(dars) = 16
ndvi = data.table::fread("_proj_NDVI_30m_forPCA.csv", select = c(1:2))
srad.ndvi = anti_join(srad, ndvi) ## check if ndvi includes any of the missing values from SRADTot_0415; yes, nrow() = 102
dars.ndvi = anti_join(dars, ndvi) ## check if ndvi includes any of the missing values from SRADTot_0115; yes, nrow() = 1

## this only matters if the final, most masked dataset contains the relevant values
soil2 = data.table::fread("_proj_soilAWC_D1_30m_forPCA.csv", select = c(1:2))
srad.soil = anti_join(srad, soil2)
## nrow() = 721
dars.soil = anti_join(dars, soil2)
## nrow() = 10
## ^need to re-upload processed srad layers 4-12
### ^done; NaNs resolved

#----------------- srad_redo --------------
library(raster)
library(snowfall)
library(parallel)

setwd("/glade/scratch/kjfuller/data")

veg = raster("for_fuels_30m.tif")

all = list.files("./vars", recursive = FALSE, full.names = TRUE)

projfun = function(x){
  lab = substr(all[x], 8, nchar(all[x]))
  r = raster(all[x])
  r = projectRaster(r, veg, method = 'bilinear')
  writeRaster(r, paste0("proj_", lab), overwrite = TRUE)
  r = raster::mask(r, veg)
  writeRaster(r, paste0("mask_proj_", lab), overwrite = TRUE)
}

sfInit(parallel = TRUE, cpus = 9)
sfExport("veg", "all", "projfun")
sfLibrary(raster)

sfLapply(c(1:9), projfun)

sfStop()

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

#----------------- selecting absence plots for RF on laptop -------------
library(sf)
library(raster)
library(tidyverse)
library(data.table)

veg = raster("data/for_fuels_30m.tif")
records = data.table::fread("data/BioNet_allfloralsurvey_cleaned2.csv")
records = records %>% 
  st_as_sf(coords = c("Longitude_GDA94", "Latitude_GDA94"), crs = 4326) %>% 
  st_transform(crs = st_crs(veg))
records = cbind(records, check = raster::extract(veg, st_coordinates(records), method = 'simple'))
nrow(records)
## 457,486
records = na.omit(records)
nrow(records)
## 263399
records = records %>% 
  dplyr::select(geometry)
records$lon = st_coordinates(records)[,1]
records$lat = st_coordinates(records)[,2]
st_geometry(records) = NULL
records = unique(records)
nrow(records)
## 8053
data.table::fwrite(records, "data/survey_locations_forRF.csv")

#------------------ generate P-As for RFs ---------------------
library(tidyverse)

setwd("/glade/scratch/kjfuller/data")

sites = read.csv("PCA4_sitesforRF.csv")
unique(sites$b1)
## smooth, 
## smooth with stocking, 
## halfbark, 
## ironbark, 
## stringybark, 
## subfibrous - rough, 
## subfibrous - stringy, 
## subfibrous - tessellated, 
## subfibrous - peppermint, 
## subfibrous - box

surveys = read.csv("survey_locations_forRF.csv")
names(surveys) = c("x", "y")
sites = left_join(surveys, sites)
nrow(sites)
## 15,695

i = "smooth"
a = sites[sites$b1 == i,]
a[i] = 1
a = a %>% 
  dplyr::select(x, y, paste(i))
sites = left_join(sites, a)
sites[i][is.na(sites[i])] = 0


#---------------- summary figures for veg types ------------
library(data.table)
library(sf)
library(tidyverse)
library(raster)
library(parallel)
library(snowfall)

setwd("/glade/scratch/kjfuller/data")
veg = raster("fuels_30m.tif")

r = veg
r = r %in% 1
writeRaster(r, "veg1_30m.tif")
r = raster("veg1_30m.tif")
df = as.data.frame(rasterToPoints(r), xy = TRUE)
df = df[df$veg1_30m == 1,]
data.table::fwrite(df, "veg1_30m_df.csv")

rastfun = function(x){
  r = veg
  r = r %in% x
  writeRaster(r, paste0("veg", x, "_30m.tif"))
  r = raster(paste0("veg", x, "_30m.tif"))
  df = as.data.frame(rasterToPoints(r), xy = TRUE)
  df = df[df[3] == 1,]
  data.table::fwrite(df, paste0("veg", x, "_30m_df.csv"))
}

sfInit(parallel = TRUE, cpus = 10)
sfExport("veg")
sfLibrary(raster)
sfLibrary(data.table)

sfLapply(c(3:51), rastfun)

sfStop()

#---------------- summary figures for veg types (2) ------------
library(data.table)
library(sf)
library(tidyverse)
library(raster)

setwd("/glade/scratch/kjfuller/data")
veg = raster("fuels_30m.tif")
nsw = st_read("NSW_sans_islands.shp") %>% 
  st_transform(crs = st_crs(veg))

df = data.frame()
for(i in c(1:51)){
  df2 = data.table::fwrite(paste0("veg", i, "_30m_df.csv"))
  df2[3] = i
  names(df2)[3] = "veg_type"
  df = rbind(df, df2)
}
df = st_as_sf(df, coords = c("x", "y"), crs = st_crs(veg))

bio1 = raster("masked/mask_proj_bioclim1_30m.tif")
bio12 = raster("masked/mask_proj_bioclim12_30m.tif")

df = cbind(df, 
           temp = raster::extract(bio1, st_coordinates(df)),
           precip = raster::extract(bio12, st_coordinates(df)))

df = na.omit(df)
df$temp = df$temp/10

plotfun = function(min, max, label){
  par(oma = c(0,0,0,0))
  par(mfrow = c(1, 2))
  plot(st_geometry(nsw))
  plot(st_geometry(df), col = "grey80", add = TRUE)
  plot(st_geometry(df[df$veg_type >= min & df$veg_type <= max,]), col = "red", add = TRUE)
  with(df, plot(precip ~ temp,
                col = "grey80",
                xlab = "Temperature",
                ylab = "Precipitation"))
  with(df[df$veg_type >= min & df$veg_type <= max,], points(precip ~ temp,
                                                            col = "red"))
  mtext(label, side=3, outer=TRUE, line=-3)
}
setwd("D:/chapter1/bark-type-SDM/outputs")

tiff("veg1_rainforests.tiff", width = 1600, height = 900, res = 200, units = "px")
plotfun(1, 1, "Rainforest")
dev.off()

tiff("veg2_wetscler_shrub.tiff", width = 1600, height = 900, res = 200, units = "px")
plotfun(2, 5, "Wet Sclerophyll Forest (shrubby)")
dev.off()

tiff("veg3_wetscler_grass.tiff", width = 1600, height = 900, res = 200, units = "px")
plotfun(6, 10, "Wet Sclerophyll Forest (grassy)")
dev.off()

tiff("veg4_dryscler_shrub.grass.tiff", width = 1600, height = 900, res = 200, units = "px")
plotfun(11, 20, "Dry Sclerophyll Forest (shrubby/grassy)")
dev.off()

tiff("veg5_dryscler_shrub.tiff", width = 1600, height = 900, res = 200, units = "px")
plotfun(21, 34, "Dry Sclerophyll Forest (shrubby)")
dev.off()

tiff("veg6_grassy_woodlands.tiff", width = 1600, height = 900, res = 200, units = "px")
plotfun(35, 41, "Grassy Woodlands")
dev.off()

tiff("veg7_heathlands.tiff", width = 1600, height = 900, res = 200, units = "px")
plotfun(42, 44, "Heathlands")
dev.off()

tiff("veg8_alpine.tiff", width = 1600, height = 900, res = 200, units = "px")
plotfun(45, 46, "Alpine")
dev.off()

tiff("veg9_semi-arid_grassy.tiff", width = 1600, height = 900, res = 200, units = "px")
plotfun(47, 47, "Semi-Arid Woodlands (grassy)")
dev.off()

tiff("veg10_semi-arid_shrub.tiff", width = 1600, height = 900, res = 200, units = "px")
plotfun(48, 51, "Semi-Arid Woodlands (shrubby)")
dev.off()


#------------- PCA tile extract -------------------
library(raster)
library(sf)

setwd("/glade/scratch/kjfuller/data")
veg = raster("for_fuels_30m.tif")

records = read.csv("site-specific_P-A_wide_barks.csv") %>% 
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(veg))

setwd("/glade/scratch/kjfuller/data/PCAtiles")
pc = list.files(pattern = ".tif")
pc = pc[!grepl("test", pc)]
for(a in c(1:14)){
  pc1 = pc[grepl(paste0("PC", a, "_"), pc)]
  for(i in c(1:length(pc1))){
    r = raster(pc1[i])
    records = cbind(records,
                    as.numeric(raster::extract(r, st_coordinates(records), method = 'simple')))
    names(records)[ncol(records)-1] = paste0("PC", a, "_", i)
  }
  
  values = records
  st_geometry(values) = NULL
  values = values[,c((10+a):ncol(values))]
  values$PC_mean = rowMeans(values, na.rm = TRUE)
  
  records = cbind(records[,c(1:(9+a))], values$PC_mean)
  names(records)[10+1] = paste0("PC", a)
}

records$lon = st_coordinates(records)[,1]
records$lat = st_coordinates(records)[,2]
st_geometry(records) = NULL
write.csv(records, "site-specific_P-A_wide_barks_PCAvalues.csv", row.names = FALSE)

records = na.omit(records)
setwd("/glade/scratch/kjfuller/data")
write.csv(records, "PC_valuesforRF.csv", row.names = FALSE)