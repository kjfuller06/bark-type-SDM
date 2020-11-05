## need to examine res/extent of data layers before extraction
library(raster)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(RColorBrewer)

# Extract environmental data for species records ####
## load dataset
records = st_read("data/spp_selection_forLDA.shp")

# load map of fuel types
fuels = raster("data/FuelTypeV2_FuelLUT1.tif")
# reproject records to fuels crs
records = records %>% 
  st_transform(crs = 3308)
# extract fuel types
records = cbind(records, fueltype = raster::extract(fuels, st_coordinates(records), methods = 'simple'))

# get WorldClim data
# WorldClim metadata notes are in my data layers metadata xlsx
r2.5 = getData('worldclim', var = 'bio', res = 2.5, path = "data/")
# reduce data size by cropping raster by extent of NSW
nsw = st_read("data/NSW_sans_islands.shp") %>% 
  st_set_crs(4326)
# use nsw bbox to crop the raster image
r2.5_crop = crop(x = r2.5, y = nsw)
# reproject records to r2.5 crs
records = records %>% 
  st_transform(crs = 4326)

# extract WorldClim values at species occurrence locations and cbind to records df
records = cbind(records, raster::extract(r2.5_crop, st_coordinates(records), methods = 'simple'))

# perform same processing for aridity data. Data were downloading by following this link: https://cgiarcsi.community/2019/01/24/global-aridity-index-and-potential-evapotranspiration-climate-database-v2/
arid = raster('data/ai_et0/ai_et0.tif')
# use nsw bbox to crop the raster image
arid_crop = crop(x = arid, y = nsw)
# plot(arid_crop)
# extract aridity values at species occurrence locations and cbind to records df
records = cbind(records, aridity = raster::extract(arid_crop, st_coordinates(records), methods = 'simple'))

# can't do any prediction from here. Data are at different spatial res and extent. But this gives me data for plotting species envelopes.

# Plot environmental axes for each species ####
tiff(file = "outputs/frequency_HorseyV.4_records_by_species.tiff", width =2200, height = 750, units = "px", res = 200)
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
st_write(records, "data/HorseyV.4_extracted_dataV.3.shp", delete_layer = TRUE)
