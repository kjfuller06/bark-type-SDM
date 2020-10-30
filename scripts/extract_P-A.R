# script for extracting presence/absence data from species selection records
library(tidyverse)
library(sf)
library(reshape2)

# read records in again for plotting
records = st_read("data/Horsey_sampleV.3.shp") %>% 
  dplyr::select('spp_shr',
                'bark1',
                'bark2',
                'ribbons',
                "geometry") %>% 
  st_transform(crs = 3577)

# combine the points that are within 100m of each other by 2) creating buffers around all points, 3) combining buffers, 4) checking area, so that the polygons aren't gigantic, 5) generating a centroid for each polygon, 6) assigning this as a column in the df for records that occur within each polygon, 7) doing another dcast() to generate presence-absence data
buffers = st_buffer(records, dist = 100)
samplearea = st_union(buffers) %>% 
  st_cast(to = "POLYGON") %>% 
  st_as_sf()
samplearea$ID = c(1:nrow(samplearea))
records = st_join(samplearea, records)
st_geometry(records) = st_centroid(records$x)
records = records %>% 
  st_transform(4326)

# write a column for every unique species in the dataset and populate the columns with presence and absence observations represented as 1 and 0, respectively
df = records
df$lon = st_coordinates(df)[,1]
df$lat = st_coordinates(df)[,2]
df <- st_set_geometry(df, NULL)
df = df %>% 
  dplyr::select('spp_shr',
                'bark1',
                'bark2',
                'ribbons',
                'lon',
                'lat')
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

# write to disk
write.csv(sample, "data/spp_selection_P-A.csv", row.names = FALSE)
write.csv(df3, "data/spp_selection_P-A_coordinates.csv", row.names = FALSE)
write_sf(df, "data/spp_selection_forLDA.shp")
