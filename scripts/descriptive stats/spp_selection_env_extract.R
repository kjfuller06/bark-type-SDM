## will need to create extraction for absence records too
library(raster)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(RColorBrewer)

# Extract environmental data for species records ####
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
