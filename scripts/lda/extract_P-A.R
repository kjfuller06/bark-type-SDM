# script for extracting presence/absence data from species selection records
library(tidyverse)
library(sf)
library(reshape2)

# read records in again for plotting
records = st_read("data/Horsey_sampleV.2.shp") %>% 
  dplyr::select(
    "spp_shr",
    "geometry") %>% 
  st_transform(crs = 3577)

# combine the points that are within 100m of each other by 2) creating buffers around all points, 3) combining buffers, 4) checking area, so that the polygons aren't gigantic, 5) generating a centroid for each polygon, 6) assigning this as a column in the df for records that occur within each polygon, 7) doing another dcast() to generate presence-absence data
buffers = st_buffer(records, dist = 100)
samplearea = st_union(buffers) %>% 
  st_cast(to = "POLYGON") %>% 
  st_as_sf()
samplearea$ID = c(1:nrow(samplearea))
records = st_join(samplearea, records)
records$cent = st_centroid(records$x)

# write a column for every unique species in the dataset and populate the columns with presence and absence observations represented as 1 and 0, respectively
df = sample
df$lon = st_coordinates(df)[,1]
df$lat = st_coordinates(df)[,2]
df <- st_set_geometry(df, NULL)
df = dcast(df, lon + lat ~ spp_shr, fill = 0, length)

# remove species that are in less than 1% of samples and greater than 50% of samples
no_coords = df[,c(3:ncol(df))]
sumstats = colSums(no_coords)/nrow(no_coords)
sumstats <- sumstats[which(sumstats > 0.01 & sumstats < 0.5)]
sample = no_coords[,which(names(no_coords) %in% names(sumstats))]

# write to disk
write.csv(sample, "data/spp_selection_P-A.csv", row.names = FALSE)
write.csv(df, "data/spp_selection_P-A_coordinates.csv", row.names = FALSE)
