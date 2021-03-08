# script for extracting presence/absence data from species selection records
## need to incorporate site-specific bark traits in species_sampleV.1.shp
#     -create distinct names for the same species with tree form and mallee form
#     -generate datasets for each bark type- including the different growth forms of such species

library(tidyverse)
library(sf)
library(reshape2)
library(raster)

# read records in again for plotting
records = read.csv("data/BioNet_allfloralsurvey_cleaned2.csv") %>% 
  dplyr::select(-X, -DateFirst, -DateLast)
nomen = read.csv("data/Candidate_speciesV.1.csv") %>% 
  dplyr::select(-BioNet) %>% 
  unique()
names(nomen) = c("Assgn_ScientificName", "NicolleName")
traits = read.csv("data/Allspecies_traitsV.1.csv") ## should try to use species_sampleV.1.shp instead, which has site-specific trait details 

# rename BioNet species
records = records %>% 
  left_join(nomen)
for(i in c(1:nrow(records))){
  if(is.na(records$NicolleName[i]) == FALSE){
    records$Assgn_ScientificName[i] = records$NicolleName[i]
  }
}
records = records %>% 
  dplyr::select(-NicolleName) %>% 
  unique()

# load vegetation map
veg = raster("data/FuelTypeV2_FuelLUT1.tif")

# transform records to same crs as veg layer and extract veg types
records = records %>% 
  st_as_sf(coords = c("Longitude_GDA94", "Latitude_GDA94"), crs = 4326) %>% 
  st_transform(crs = st_crs(veg))
records = cbind(records, fuel = raster::extract(veg, st_coordinates(records), methods = 'simple'))
## 457,435 records

# remove wetlands, grasslands, alpine habitat, urban, etc.
types = c(42:46, 52:74)
records = records %>%
  filter(!(fuel %in% types))
## 270,025 records
records = drop_na(records)
## 263,371 records
## dropped records lie outside the veg type boundary- sometimes these were still within the state where the veg layer was inaccurately clipped

# fill species columns with number of occurrences per unique combination of lon + lat
# remove geometry again
records$lon = st_coordinates(records)[,1]
records$lat = st_coordinates(records)[,2]
st_geometry(records) = NULL
# generate stats
PA = dcast(records, lon + lat ~ Assgn_ScientificName, fill = 0, value.var = "Assgn_ScientificName")
## 8,053 rows = sampling locations
## 4,946 columns = species/subspecies

# select only species from Horsey selection
selection = match(unique(nomen$NicolleName), colnames(PA))
selection = selection[is.na(selection) == FALSE]
PA = PA[,c(1, 2, selection)]
## 8,053 rows = sampling locations
## 178 columns = species/subspecies

# replace non-zero observations with "1"
PA[,c(3:ncol(PA))] = PA[,c(3:ncol(PA))] %>% 
  mutate_if(is.numeric, ~1 * (. != 0))

# melt to long form
longf = melt(PA, id.vars = c("lon", "lat"))
## 1,417,328 observations
## 1,403,858 as 0's
## 13,470 as 1's

# add bark traits back to long form df
longf = longf %>% 
  left_join(info, by = c("variable" = "NicolleName"))
  # dplyr::select(-variable)

# write to disk
write.csv(PA, "data/species_sampleV.1_P-A_wide.csv", row.names = FALSE)
write.csv(longf, "data/species_sampleV.1_P-A_long.csv", row.names = FALSE)
