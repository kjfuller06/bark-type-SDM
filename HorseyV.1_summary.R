library(raster)
library(tidyverse)
library(sf)
library(rnaturalearth)

records = st_read("data/Horsey_sampleV.1.shp")
aus = st_read("data/australia.shp")

ggplot(records)+
  geom_sf(data = aus)+
  geom_sf()

# r2.5 = getData('worldclim', var = 'bio', res = 2.5)

# next step will be to extract environmental data and plot species + bark types against variables