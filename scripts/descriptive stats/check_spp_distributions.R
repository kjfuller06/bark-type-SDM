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

# read records in again for plotting
records = st_read("data/HorseyV.2_extracted_dataV.1.shp")
tmap_mode("view")
tm_shape(records[records$bark1 == "smooth",])+tm_dots(col = "spp_shr")

tm_shape(records[records$bark1 == "subfibrous - box",])+tm_dots(col = "spp_shr")

tm_shape(records[records$bark1 == "subfibrous - tessellated",])+tm_dots(col = "spp_shr")

tm_shape(records[records$bark1 == "subfibrous - rough",])+tm_dots(col = "spp_shr")

tm_shape(records[records$bark1 == "stringybark",])+tm_dots(col = "spp_shr")

tm_shape(records[records$bark1 == "smooth - short stocking",])+tm_dots(col = "spp_shr")

tm_shape(records[records$bark1 == "half bark",])+tm_dots(col = "spp_shr")

tm_shape(records[records$bark1 == "ironbark",])+tm_dots(col = "spp_shr")

tm_shape(records[records$bark1 == "subfibrous - stringy",])+tm_dots(col = "spp_shr")

tm_shape(records[records$bark1 == "subfibrous - peppermint",])+tm_dots(col = "spp_shr")
