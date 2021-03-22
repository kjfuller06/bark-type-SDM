# record of code run on Cheyenne
library(raster)
library(sf)
library(tmap)
library(gdalUtils)
library(snowfall)
library(parallel)
library(tidyverse)

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


# mask all layers to nsw boundary