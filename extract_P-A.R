# script for visualising species selections
library(raster)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(RColorBrewer)

# read records in again for plotting
records = st_read("data/Horsey_sampleV.2.shp") %>% 
  dplyr::select(
    "spp_shr",
    "geometry")

# write columns for each species of interest
spp = unique(records$spp_shr)

sample = records[c(1:20),]

for(i in c(1:98)){
  sample[, i+2] = NA
  names(sample)[i+2] = spp[i]
  for(k in c(1:nrow(sample))){
    if(sample[k,][[1]] == names(sample)[i+2]){
      sample[k,][[i+2]] = 1
    } else {
      sample[k,][[i+2]] = 0
    }
  }
}

st_geometry(sample) = NULL
sample = sample %>% 
  dplyr::select(-spp_shr)

sumstats = colSums(sample)/nrow(sample)
sumstats <- sumstats[which(sumstats > 0.03 & sumstats < 0.5)]
sample = sample[,which(names(sample) %in% names(sumstats))]

