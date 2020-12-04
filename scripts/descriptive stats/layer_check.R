# this script is for examining fuel complexes of NSW 
library(raster)
library(tidyverse)
library(sf)
library(rgdal)
library(tmap)

# check for crs ####
# get codes
codes = make_EPSG()

# load vegetation data
veg = raster("data/FuelTypeV2_FuelLUT1.tif")
# look for crs code in "codes"
# lcc = codes[grep("proj=lcc", codes$prj4),]
# lcc = lcc[grep("ellps=GRS80", lcc$prj4),]
# lcc = lcc[grep("+lat_0=-33.25", lcc$prj4),]
# EPSG is either 3308 for GDA94 / NSW Lambert or 8058 for GDA2020 / NSW Lambert
## use 8058

# have to figure out the crs for this file
records = st_read("data/spp_selection_forLDA.shp") %>% 
  st_transform(crs = 3308)

tmap_mode("view")
tm_shape(veg)+tm_raster()+tm_shape(records[records$lng_spt != "n",])+tm_dots(col = "lng_spt")

# bind veg values to records df
records = cbind(records, fuel = raster::extract(veg, st_coordinates(records), methods = 'simple'))

records_df = records
st_geometry(records_df) = NULL
records_df$fuel = as.factor(records_df$fuel)
records_df$spp_shr = as.factor(records_df$spp_shr)
records_df = unique(records_df)

tal = records_df %>% 
  group_by(fuel) %>% 
  tally()

# check out WorldClim data
r2.5 = getData('worldclim', var = 'bio', res = 2.5, path = "data/")
# look for crs code in "codes"
r_check = codes[grep("proj=longlat", codes$prj4),]
r_check = r_check[grep("datum=WGS84", r_check$prj4),]
# use code 4326

# check out the aridity data
arid = raster('data/ai_et0/ai_et0.tif')
# same as above

# examining collinearity ####
# load datasets
r2.5 = getData('worldclim', var = 'bio', res = 2.5, path = "data/")
veg = raster("data/fuels_reproj.tif")
arid = raster("data/aridity_reproj.tif")
nsw = st_read("data/NSW_sans_islands.shp")

# clean WorldClim data
nsw1 = nsw %>% 
  st_transform(crs = st_crs(r2.5))
r2.5.1 = crop(r2.5, extent(nsw1))
r2.5.2 = projectRaster(r2.5.1, crs = crs(veg))

# stack datasets
stck = raster::stack(veg, r2.5.2, arid)

# create pairs plots
# pairs(stck[[c(4, 6:7, 9, 10, 16, 19)]])

# select orthogonal variables
stck = stck[[c(1, 4, 6:7, 9, 10, 16, 19)]]

# create fire history layer ####
r1919 = raster("data/fireyeartifs/fireyear1919.tif")
r1925 = raster("data/fireyeartifs/fireyear1925.tif")
r_add = r1919 + r1925

#pattern = "*.tif$" - filters for main raster files only and skips any associated files (e.g. world files)
grids <- list.files("./data/fireyeartifs" , pattern = "*.tif$")

#create a raster stack from the input raster files 
r_all <- raster::stack(paste0("./data/fireyeartifs/", grids))

# replace all non-NA values with 1
r_all[is.na(r_all) == FALSE] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[2]][values(r_all[[1]]) > 1] = 1
# r_all[[3]][values(r_all[[1]]) > 1] = 1
# r_all[[4]][values(r_all[[1]]) > 1] = 1
# r_all[[5]][values(r_all[[1]]) > 1] = 1
# r_all[[6]][values(r_all[[1]]) > 1] = 1
# r_all[[7]][values(r_all[[1]]) > 1] = 1
# r_all[[8]][values(r_all[[1]]) > 1] = 1
# r_all[[9]][values(r_all[[1]]) > 1] = 1
# r_all[[10]][values(r_all[[1]]) > 1] = 1
# r_all[[11]][values(r_all[[1]]) > 1] = 1
# r_all[[12]][values(r_all[[1]]) > 1] = 1
# r_all[[13]][values(r_all[[1]]) > 1] = 1
# r_all[[14]][values(r_all[[1]]) > 1] = 1
# r_all[[15]][values(r_all[[1]]) > 1] = 1
# r_all[[16]][values(r_all[[1]]) > 1] = 1
# r_all[[17]][values(r_all[[1]]) > 1] = 1
# r_all[[18]][values(r_all[[1]]) > 1] = 1
# r_all[[19]][values(r_all[[1]]) > 1] = 1
# r_all[[20]][values(r_all[[1]]) > 1] = 1
# r_all[[21]][values(r_all[[1]]) > 1] = 1
# r_all[[22]][values(r_all[[1]]) > 1] = 1
# r_all[[23]][values(r_all[[1]]) > 1] = 1
# r_all[[24]][values(r_all[[1]]) > 1] = 1
# r_all[[25]][values(r_all[[1]]) > 1] = 1
# r_all[[26]][values(r_all[[1]]) > 1] = 1
# r_all[[27]][values(r_all[[1]]) > 1] = 1
# r_all[[28]][values(r_all[[1]]) > 1] = 1
# r_all[[29]][values(r_all[[1]]) > 1] = 1
# r_all[[30]][values(r_all[[1]]) > 1] = 1
# r_all[[31]][values(r_all[[1]]) > 1] = 1
# r_all[[32]][values(r_all[[1]]) > 1] = 1
# r_all[[33]][values(r_all[[1]]) > 1] = 1
# r_all[[34]][values(r_all[[1]]) > 1] = 1
# r_all[[35]][values(r_all[[1]]) > 1] = 1
# r_all[[36]][values(r_all[[1]]) > 1] = 1
# r_all[[37]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
# r_all[[1]][values(r_all[[1]]) > 1] = 1
