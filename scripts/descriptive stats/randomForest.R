library(raster)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(tmap)
library(randomForest)

# testing random forest ####
records = st_read("data/spp_selection_P-A.shp")


