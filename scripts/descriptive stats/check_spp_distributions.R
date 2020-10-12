library(raster)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(RColorBrewer)

## load dataset
records = st_read("data/spp_selection_forLDA.shp")
