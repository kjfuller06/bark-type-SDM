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
stck = stck[[c(4, 6:7, 9, 10, 16, 19)]]
