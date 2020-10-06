# script for extracting presence/absence data from species selection records
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

# let's work with a subsample for now
sample = records[c(1:20),]

# write a column for every unique species in the dataset and then populate the columns with presence and absence observations represented as 1 and 0, respectively
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

# simplify dataframe
st_geometry(sample) = NULL
sample = sample %>% 
  dplyr::select(-spp_shr)

# remove species that are in less than 1% of samples and greater than 50% of samples
sumstats = colSums(sample)/nrow(sample)
sumstats <- sumstats[which(sumstats > 0.01 & sumstats < 0.5)]
sample = sample[,which(names(sample) %in% names(sumstats))]

# write to disk
write.csv(sample, "data/spp_selection_P-A.csv")
