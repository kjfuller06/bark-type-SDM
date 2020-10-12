library(raster)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(RColorBrewer)
library(tmap)

## load dataset
records = st_read("data/spp_selection_forLDA.shp")
tmap_mode("view")
qtm(records[records$spp_shr == "E.agglomerata",])
