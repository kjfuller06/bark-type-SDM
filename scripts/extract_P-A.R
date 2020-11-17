# script for extracting presence/absence data from species selection records
library(tidyverse)
library(sf)
library(reshape2)
library(raster)

# read records in again for plotting
records = read.csv("data/BioNet_allfloralsurvey_cleaned2.csv") %>% 
  dplyr::select(-X, -DateFirst, -DateLast)
spp = read.csv("data/Horsey_candidate_speciesV.4.csv")

# load vegetation map
veg = raster("data/fuels_reproj.tif")

# transform records to same crs as veg layer and extract veg types
records = records %>% 
  st_as_sf(coords = c("Longitude_GDA94", "Latitude_GDA94"), crs = 4326) %>% 
  st_transform(crs = st_crs(veg))
records = cbind(records, fuels_reproj = raster::extract(veg, st_coordinates(records), methods = 'simple'))

# remove wetlands, grasslands, alpine habitat, urban, etc.
types = c(42:46, 52:74)
records = records %>% 
  filter(!(fuels_reproj %in% types))
records = drop_na(records)

# fill species columns with number of occurrences per unique combination of lon + lat
# remove geometry again
records$lon = st_coordinates(records)[,1]
records$lat = st_coordinates(records)[,2]
st_geometry(records) = NULL
# generate stats
PA = dcast(records, lon + lat ~ Assgn_ScientificName, fill = 0, value.var = "Assgn_ScientificName")
# select only species from Horsey selection
selection = match(spp$BioNetspecies, colnames(PA))
selection = selection[is.na(selection) == FALSE]
PA = PA[,c(1, 2, selection)]
# replace non-zero observations with "1"
PA[,c(3:ncol(PA))] = PA[,c(3:ncol(PA))] %>% 
  mutate_if(is.numeric, ~1 * (. != 0))
# create df with just bark traits to join back to PA later
info = unique(spp[,c(1, 3, 4, 5, 6)])

# melt to long form
longf = melt(PA, id.vars = c("lon", "lat"))

# add bark traits back to long form df
longf = longf %>% 
  left_join(info, by = c("variable" = "BioNetspecies")) %>% 
  dplyr::select(-variable)

# add new bark categories
cat = read.csv("data/Horsey_candidate_speciesV.4_types.csv")
cat = cat[,1:6]
longf = longf %>% 
  left_join(cat, by = c("bark1", "bark2", "ribbons"))

# create shapefile as well
longf_shp = longf %>% 
  st_as_sf(coords = c(lon = "lon", lat = "lat"), crs = st_crs(veg))

# write to disk
write.csv(PA, "data/spp_selection_P-A.csv", row.names = FALSE)
write.csv(longf, "data/spp_selection_P-A_coordinates.csv", row.names = FALSE)
write_sf(longf_shp, "data/spp_selection_P-A.shp")
