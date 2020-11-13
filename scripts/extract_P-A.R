# script for extracting presence/absence data from species selection records
library(tidyverse)
library(sf)
library(reshape2)

# read records in again for plotting
records = read.csv("data/BioNet_allfloralsurvey_cleaned2.csv") %>% 
  dplyr::select(-X, -DateFirst, -DateLast)
names(records)[3:4] = c("lat", "lon")
spp = read.csv("data/Horsey_candidate_speciesV.4.csv")

# fill species columns with number of occurrences per unique combination of lon + lat
df2 = dcast(df, lon + lat ~ spp_shr, fill = 0, value.var = "spp_shr")
# replace non-zero observations with "1"
df2[,c(3:ncol(df2))] = df2[,c(3:ncol(df2))] %>% 
  mutate_if(is.numeric, ~1 * (. != 0))
# create df with just bark traits to join back to df2 later
info = unique(df[,c(1:4)])

# remove species that are in less than 1% of samples and greater than 50% of samples
df3 = df2
no_coords = df2[,c(3:ncol(df2))]
sumstats = colSums(no_coords)/nrow(no_coords)
sumstats <- sumstats[which(sumstats > 0.01 & sumstats < 0.5)]
sample = no_coords[,which(names(no_coords) %in% names(sumstats))]
# create df3 for merging coordinates in LDA.r script
df3 = df3[, which(names(df3) %in% names(sumstats))]
## create shapefile for environmental data extraction
# remove rare and common species
# df2 = df2[,c(1, 2, which(names(df2) %in% names(sumstats)))]
# melt back to long form
df2 = melt(df2, id.vars = c("lon", "lat")) %>% 
  filter(value == 1) %>% 
  dplyr::select(-value)

# add bark traits back to long form df
df = df2 %>% 
  left_join(info, by = c("variable" = "spp_shr")) %>% 
  st_as_sf(coords = c(lon = "lon", lat = "lat"), crs = 4326)
names(df)[1] = "spp_shr"
# add new bark categories
cat = read.csv("data/Horsey_candidate_speciesV.4_types.csv")
cat = cat[,1:6]
df5 = df %>% 
  left_join(cat, by = c("bark1", "bark2", "ribbons"))

# write to disk
write.csv(sample, "data/spp_selection_P-A.csv", row.names = FALSE)
write.csv(df3, "data/spp_selection_P-A_coordinates.csv", row.names = FALSE)
write_sf(df5, "data/spp_selection_forLDA.shp")
