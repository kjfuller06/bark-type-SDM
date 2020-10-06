# script for extracting presence/absence data from species selection records
library(raster)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(RColorBrewer)
library(reshape2)

# read records in again for plotting
records = st_read("data/Horsey_sampleV.2.shp") %>% 
  dplyr::select(
    "spp_shr",
    "geometry")

# write columns for each species of interest
spp = unique(records$spp_shr)

# let's work with a subsample for now
sample = records[c(1:200),]

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
