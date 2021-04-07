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

# mask all layers to nsw boundary