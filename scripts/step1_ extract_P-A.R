# script for extracting presence/absence data from species selection records
library(tidyverse)
library(sf)
library(reshape2)
library(raster)

# process data ####
# load datasets
records = read.csv("outputs/BioNet_allfloralsurvey_cleaned2.csv") %>% 
  dplyr::select(-DateFirst, -DateLast)
nomen = read.csv("data/Candidate_speciesV.1.csv") %>% 
  unique()
names(nomen) = c("ScientificName", "Assgn_ScientificName", "NicolleName")
traits = read_sf("data/species_sampleV.1.shp")
veg = raster("data/fuels_30m.tif")

# modify traits to include alternative names for species with more than one growth form
traits = traits %>% 
  st_transform(crs = st_crs(veg))
traits$lon = st_coordinates(traits)[,1]
traits$lat = st_coordinates(traits)[,2]
st_geometry(traits) = NULL
# subset out species with multiple growth forms
selection = unique(traits[,c(5, 6)]) %>% 
  group_by(Ncll19N) %>% 
  tally() %>% 
  filter(n>1) %>% 
  as.data.frame() 
growthforms = unique(traits[traits$Ncll19N %in% selection$Ncll19N,][,c(5, 6, 9)])
growthforms$Ncll19N_form = paste(growthforms$Ncll19N, growthforms$mallee, sep = "_")
# recombine datasets
growthforms = growthforms %>% 
  left_join(traits)
names(growthforms)[c(1,4)] = c("NicolleName", "Ncll19N")
traits$NicolleName = traits$Ncll19N
traits = rbind(traits[!(traits$Ncll19N %in% selection$Ncll19N),], growthforms)

# create unified reference sheet for site-name-bark-type application to BioNet data
traits = left_join(traits, nomen) %>% 
  dplyr::select(-NicolleName)

# rename BioNet species
# first reproject coordinates, then turn back into data frame for efficiency
records = records %>% 
  st_as_sf(coords = c("Longitude_GDA94", "Latitude_GDA94"), crs = 4326) %>% 
  st_transform(crs = st_crs(veg))
records$lon = st_coordinates(records)[,1]
records$lat = st_coordinates(records)[,2]
st_geometry(records) = NULL
## 462,150 records
records = records %>% 
  left_join(traits)
## 464,290 records
## this takes the data frame from nrow() = 462,150 to nrow() = 464,290, so some sites have both the tree and mallee forms of switchable species
for(i in c(1:nrow(records))){
  if(is.na(records$Ncll19N[i]) == FALSE){
    records$Assgn_ScientificName[i] = records$Ncll19N[i]
  }
}
records = records[,c(1:4, 6:7)]

# convert back into sf and extract veg types
records = records %>% 
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(veg))
records = cbind(records, fuel = raster::extract(veg, st_coordinates(records), methods = 'simple'))
## 464,290 records

# remove wetlands, grasslands, alpine habitat, urban, etc.
types = c(52:74)
records = records %>%
  filter(!(fuel %in% types))
## 281,026 records
records = drop_na(records)
## 274,372 records
## dropped records lie outside the veg type boundary- sometimes these were still within the state where the veg layer was inaccurately clipped
backup = records

# generate presence/absence data per species ####
# fill species columns with number of occurrences per unique combination of lon + lat
# remove geometry again
records$lon = st_coordinates(records)[,1]
records$lat = st_coordinates(records)[,2]
st_geometry(records) = NULL
# generate stats
PA = dcast(records, lon + lat ~ Assgn_ScientificName, fill = 0, value.var = "Assgn_ScientificName")
## 8,326 rows = sampling locations
## 5,120 columns = species/subspecies

# select only species from Horsey selection
selection = match(unique(traits$Ncll19N), colnames(PA))
selection = selection[is.na(selection) == FALSE]
PA = PA[,c(1, 2, selection)]
## 8,326 rows = sampling locations
## 182 columns = species/subspecies

# replace non-zero observations with "1"
PA[,c(3:ncol(PA))] = PA[,c(3:ncol(PA))] %>% 
  mutate_if(is.numeric, ~1 * (. != 0))

# melt to long form
longf = melt(PA, id.vars = c("lon", "lat"))
## 1,498,680 observations
## 1,485,325 as 0's
## 13,355 as 1's

# add bark traits back to long form df
longf = longf %>% 
  left_join(unique(traits[,c(5:9),]), by = c("variable" = "Ncll19N"))
  # dplyr::select(-variable)

# select unique bark observations only from site trait observations
traits2 = traits %>% 
  dplyr::select(Hrsyb1_,
                Hrsyb2_,
                lon,
                lat)
names(traits2) = c("b1", "b2", "lon", "lat")

# generate stats for bark traits
PA2 = dcast(traits2, lon + lat ~ b1, fill = 0, value.var = "b1")
PA2[,c(3:ncol(PA2))] = PA2[,c(3:ncol(PA2))] %>% 
  mutate_if(is.numeric, ~1 * (. != 0))
names(PA2) = c("lon",
               "lat",
               "halfbark",
               "ironbark",
               "smooth",
               "smooth_stocking",
               "stringybark",
               "s_box",
               "s_peppermint",
               "s_rough",
               "s_stringy",
               "s_tessellated")

# write to disk
write.csv(PA, "data/species_sampleV.1_P-A_wide.csv", row.names = FALSE)
write.csv(longf, "data/species_sampleV.1_P-A_long.csv", row.names = FALSE)
write.csv(traits, "data/alltraits_site-specific.csv", row.names = FALSE)
write.csv(PA2, "data/site-specific_P-A_wide_barks.csv", row.names = FALSE)
