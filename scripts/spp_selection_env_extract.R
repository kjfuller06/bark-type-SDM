## need to check consistency of datasets. too many NAs in the results here. bioclim shows white lines across the raster layers- could just be a plotting glitch but the tiles didn't look like they lined up perfectly when I downloaded them.
## ^the soils layer for pH is the issue. Not sure why it's not extracting properly. The layer extent should be correct- it stacks with all the other layers just fine...

library(raster)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(RColorBrewer)
library(tmap)

# Extract environmental data for species presence/absence both ####
## load dataset
veg = raster("data/FuelTypeV2_FuelLUT1.tif")
records = read.csv("data/spp_selection_P-A_allenv.csv") %>% 
  st_as_sf(coords = c(lon = "lon", lat = "lat"), crs = st_crs(veg))

# load datasets
nsw = st_read("data/NSW_sans_islands.shp")
veg = raster("data/FuelTypeV2_FuelLUT1.tif")
fire = raster("data/fire_reproj_80m.tif")
bioclim = stack("data/fire_bioclim_80m.grd")
arid = raster("data/aridity_reproj_80m.tif")
soils = stack("data/soils_80m.grd")
terrain1 = stack("data/terrain1_80m.grd")
all = raster::stack(fire, bioclim, arid, soils, terrain1)
rm(fire, bioclim, arid, soils, terrain1)

# extract values and cbind to records df
records = cbind(records, 
                raster::extract(all, st_coordinates(records), methods = 'simple'))

backup = records
# convert to df and write to disk
records$lon = st_coordinates(records)[,1]
records$lat = st_coordinates(records)[,2]
st_geometry(records) = NULL
write.csv(records, "data/data_Sp4V6_allenv_80m.csv", row.names = FALSE)

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