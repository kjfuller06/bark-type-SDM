library(sf)
library(tidyverse)
library(plotly)
library(shiny)
library(ggplot2)

# read records in again for plotting
records = st_read("data/HorseyV.2_extracted_dataV.1.shp")
st_geometry(records) = NULL
records = drop_na(records)

# plan ####
# the structure I eventually want is a list of dataframes. The dataframes are named each of the environmental variables. They contain columns of 1) the species names, 2) the x values of the density functions, 3) the y values of the density functions, each scaled to 1.
#   1) split records like always
#   2) create a df of length 512 with columns: species, environmental variable, density(df[[1]][,9])$x and density(df[[1]][,9])$y/sum(density(df[[1]][,9])$y)
#   3) loop through variables, doing this:
#       a) create df with the same dimensions as above
#       b) insert the same values
#       c) rbind to the densities df
#   4) split densities df by environmental variable

# 1b. development ####
nom = read.csv("data/env_variable_labels.csv")
nom = c(paste(nom$labels, nom$labels2, sep = "\n"))

# reorder species in alphabetical order
records = records[order(records$spp_shr),]
# split df by species for calculations
df = split(records, records$spp_shr)
# create dummy df
densities = as.data.frame(matrix(ncol = 4,
                 nrow = 512))
names(densities) = c("species", "variable", "x", "y")
# insert values for first species and variable density
densities$species = paste(df[[1]]$spp_shr[1])
densities$variable = paste(names(df[[1]])[9])
densities$x = density(df[[1]][,9])$x
densities$y = density(df[[1]][,9])$y/sum(density(df[[1]][,9])$y)
# rbind values for first species
for(envvar in c(10:ncol(df[[1]]))){
  # create dummy df
  k = as.data.frame(matrix(ncol = 4,
                           nrow = 512))
  names(k) = c("species", "variable", "x", "y")
  # insert values for species and variable density combos
  a = density(df[[1]][,envvar])
  k$species = paste(df[[1]]$spp_shr[1])
  k$variable = paste(names(df[[1]])[envvar])
  k$x = density(df[[1]][,envvar])$x
  k$y = density(df[[1]][,envvar])$y/sum(density(df[[1]][,envvar])$y)
  # rbind to densities df
  densities = rbind(densities, k)
}

# now rbind for all other combinations
for(envvar in c(9:ncol(df[[1]]))){
  for(spp in c(2:length(df))){
    # create dummy df
    k = as.data.frame(matrix(ncol = 4,
                                     nrow = 512))
    names(k) = c("species", "variable", "x", "y")
    # insert values for species and variable density combos
    a = density(df[[spp]][,envvar])
    k$species = paste(df[[spp]]$spp_shr[1])
    k$variable = paste(names(df[[spp]])[envvar])
    k$x = density(df[[spp]][,envvar])$x
    k$y = density(df[[spp]][,envvar])$y/sum(density(df[[spp]][,envvar])$y)
    # rbind to densities df
    densities = rbind(densities, k)
  }
}

# write to disk
write.csv(densities, "outputs/densities.csv")