library(raster)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(RColorBrewer)

# Extract environmental data for species occurrence only ####
## load dataset
records = st_read("data/spp_selection_P-A.shp") %>% 
  filter(value == "1")

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

# extract fuel types
records = cbind(records, fueltype = raster::extract(veg, st_coordinates(records), methods = 'simple'))

# extract WorldClim values at species occurrence locations and cbind to records df
records = cbind(records, raster::extract(r2.5.2, st_coordinates(records), methods = 'simple'))

# extract aridity values at species occurrence locations and cbind to records df
records = cbind(records, aridity = raster::extract(arid, st_coordinates(records), methods = 'simple'))

# Plot environmental axes for each species ####
tiff(file = "outputs/frequency_HorseyV.4_records_by_speciesV.2.tiff", width =2200, height = 750, units = "px", res = 200)
# counts
ggplot(data.frame(records), aes(x=spp_shr)) +
  geom_bar()+
  theme(axis.text.x = element_text(angle = 45,
                                   size = 7,
                                   hjust = 1))+ 
  coord_cartesian(ylim=c(0, 700))
dev.off()
## some species have only 2-3 records

# write shapefile to disk and convert sf to df, only because I'm so bad at this with sf's
st_write(records, "data/HorseyV.4_extracted_dataV.4.shp", delete_layer = TRUE)

# Extract environmental data for species presence/absence both ####
## load dataset
veg = raster("data/fuels_reproj.tif")
records = read.csv("data/spp_selection_P-A_allenv.csv") %>% 
  st_as_sf(coords = c(lon = "lon", lat = "lat"), crs = st_crs(veg))

# load datasets
r2.5 = getData('worldclim', var = 'bio', res = 2.5, path = "data/")
arid = raster("data/aridity_reproj.tif")
nsw = st_read("data/NSW_sans_islands.shp")
fire = raster("data/fire_reproj.tif")
bdod = raster("data/bdod_reproj.tif")
cec = raster("data/cec_reproj.tif")
cfvo = raster("data/cfvo_reproj.tif")
sand = raster("data/sand_reproj.tif")
silt = raster("data/silt_reproj.tif")
clay = raster("data/clay_reproj.tif")
nit = raster("data/nitrogen_reproj.tif")
ph = raster("data/ph_reproj.tif")
soc = raster("data/soc_reproj.tif")
ocd = raster("data/ocd_reproj.tif")

# stack soils data
soils = raster::stack(bdod, cec, cfvo, sand, silt, clay, nit, ph, soc, ocd)

# clean WorldClim data
nsw1 = nsw %>% 
  st_transform(crs = st_crs(r2.5))
r2.5.1 = crop(r2.5, extent(nsw1))
r2.5.2 = projectRaster(r2.5.1, crs = crs(veg))
temps = r2.5.2[[c('bio1', 'bio2', 'bio5', 'bio6', 'bio7', 'bio8', 'bio9', 'bio10', 'bio11')]]/10
bio_other = r2.5.2[[c('bio3', 'bio4', 'bio12', 'bio13', 'bio14', 'bio15', 'bio16', 'bio17', 'bio18', 'bio19')]]

# extract fuel types
records = cbind(records, fueltype = raster::extract(veg, st_coordinates(records), methods = 'simple'))

# extract WorldClim values at species occurrence locations and cbind to records df
records = cbind(records, raster::extract(temps, st_coordinates(records), methods = 'simple'))
records = cbind(records, raster::extract(bio_other, st_coordinates(records), methods = 'simple'))

# extract aridity values at species occurrence locations and cbind to records df
records = cbind(records, aridity = raster::extract(arid, st_coordinates(records), methods = 'simple'))

# extract fire history
records = cbind(records, firehistory = raster::extract(fire, st_coordinates(records), methods = 'simple'))

# extract soil data
records = cbind(records, raster::extract(soils, st_coordinates(records), methods = 'simple'))

# write shapefile to disk
# st_write(records, "data/HorseyV.4_extracted_dataV.4_P-A_allenv.shp", delete_layer = TRUE)
### can't write to disk for some reason
# convert to df and write to disk
records$lon = st_coordinates(records)[1]
records$lat = st_coordinates(records)[2]
st_geometry(records) = NULL
write.csv(records, "data/HorseyV.4_extracted_dataV.4_P-A_allenv.csv", row.names = FALSE)
