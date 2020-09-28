library(raster)
library(tidyverse)
library(sf)
library(rnaturalearth)

# load dataset and map of Australia
records = st_read("data/Horsey_sampleV.1.shp")
aus = st_read("data/australia.shp")

# plot
# ggplot(records)+
#   geom_sf(data = aus)+
#   geom_sf(aes(colour = Assg_SN))+
#   theme(legend.position = "none")

# get WorldClim data
r2.5 = getData('worldclim', var = 'bio', res = 2.5, path = "data/")

# reduce data size by cropping raster by extent of NSW
nsw = st_read("data/NSW_sans_islands.shp") %>% 
  st_set_crs(4326)
# use nsw bbox to crop the raster image
r2.5_crop = crop(x = r2.5, y = nsw)
# plot(r2.5_crop[['bio1']])

# Actual next step ####
# Getting env. data for presence records is good, then I'll be able to do variance partitioning/PCA to describe the environmental envelops of the species/trait groups
# After that, I'll want to extract presence AND absence data for each species. This will require retaining all unique survey records from the BioNet dataset, instead of just records for the target species. Then, sampling each location for occurrence of each target species.

