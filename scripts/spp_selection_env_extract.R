library(raster)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(RColorBrewer)

# Extract environmental data for species presence/absence both ####
## load dataset
veg = raster("data/FuelTypeV2_FuelLUT1.tif")
records = read.csv("data/spp_selection_P-A_allenv.csv") %>% 
  st_as_sf(coords = c(lon = "lon", lat = "lat"), crs = st_crs(veg))

# load datasets
nsw = st_read("data/NSW_sans_islands.shp")
veg = raster("data/FuelTypeV2_FuelLUT1.tif")
fire = raster("data/fire_reproj_80m.tif")
bioclim = raster("data/fire_bioclim_80m.grd")
arid = raster("data/aridity_reproj_80m.tif")
soils = raster("data/soils_80m.grd")
terrain1 = raster("data/terrain1_80m.grd")

# extract values and cbind to records df
records = cbind(records, 
                fueltype = raster::extract(veg, st_coordinates(records), methods = 'simple'),
                firehistory = raster::extract(fire, st_coordinates(records), methods = 'simple'),
                raster::extract(temps, st_coordinates(records), methods = 'simple'),
                raster::extract(bio_other, st_coordinates(records), methods = 'simple'),
                aridity = raster::extract(arid, st_coordinates(records), methods = 'simple'),
                raster::extract(soils, st_coordinates(records), methods = 'simple'),
                raster::extract(terrain1, st_coordinates(records), methods = 'simple'))

# convert to df and write to disk
records$lon = st_coordinates(records)[,1]
records$lat = st_coordinates(records)[,2]
st_geometry(records) = NULL
write.csv(records, "data/HorseyV.4_extractedV.6_data_allenv_80m.csv", row.names = FALSE)

# Plot environmental axes for each species ####
tiff(file = "outputs/frequency_HorseyV.4_records_by_speciesV.2.tiff", width =2200, height = 750, units = "px", res = 200)
# counts
ggplot(data.frame(records %>% filter(value == "1")), aes(x=spp_shr)) +
  geom_bar()+
  theme(axis.text.x = element_text(angle = 45,
                                   size = 7,
                                   hjust = 1))+ 
  coord_cartesian(ylim=c(0, 700))
dev.off()