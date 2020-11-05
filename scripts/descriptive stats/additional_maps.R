# script for visualising species selections
library(raster)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(RColorBrewer)
library(tmap)
library(jpeg)
library(png)
library(colorspace)

# load nsw border for maps
nsw = st_read("data/NSW_sans_islands.shp") %>% 
  st_set_crs(4326)

# read records in again for plotting
records_sf = st_read("data/HorseyV.4_extracted_dataV.3.shp")

# create map of all categories
maps = tm_shape(nsw) + 
  tm_borders() + 
  tm_shape(records_sf[records_sf$vrt_cnt != "n",]) + 
  tm_dots("vrt_cnt", 
             palette = c(high = adjustcolor(col = "red", alpha = 0.25), low = adjustcolor(col = "red", alpha = 0.05)), legend.show = FALSE, size = 0.25) +
  tm_shape(records_sf[records_sf$shrt_sp != "n",]) + 
  tm_dots("shrt_sp", 
          palette = c(high = adjustcolor(col = "royalblue", alpha = 0.25), low = adjustcolor(col = "royalblue", alpha = 0.05)), legend.show = FALSE, size = 0.25) +
  tm_shape(records_sf[records_sf$lng_spt != "n",]) + 
  tm_dots("lng_spt", 
          palette = c(high = adjustcolor(col = "yellow", alpha = 0.25), low = adjustcolor(col = "yellow", alpha = 0.05)), legend.show = FALSE, size = 0.25) +
  tm_layout(legend.position = c("right", "bottom"))
# save image to file
tmap_save(maps, filename = "outputs/all_fuelsV.1.png")

# next ####
# read records in again for plotting
records = records_sf
st_geometry(records) = NULL
records = drop_na(records)

# 1f. vertical fuel contribution
# select only relevant categories
vert_rec = records[records$vrt_cnt != "n",]
vert_rec_sf = records_sf[records_sf$vrt_cnt != "n",]
# create map
map_vert = tm_shape(nsw) + 
  tm_borders() + 
  tm_shape(vert_rec_sf[vert_rec_sf$vrt_cnt == "low",]) + 
  tm_dots("spp_shr", palette = sequential_hcl(length(unique(vert_rec$spp_shr[vert_rec$vrt_cnt == "low"])), palette = "Greens 3"), legend.show = TRUE, size = 0.25) +
  tm_layout(legend.position = c("right", "bottom"), legend.outside = TRUE) +
  tm_shape(vert_rec_sf[vert_rec_sf$vrt_cnt == "high",]) + 
  tm_dots("spp_shr", palette = sequential_hcl(length(unique(vert_rec$spp_shr[vert_rec$vrt_cnt == "high"])), palette = "OrRd"), legend.show = TRUE, size = 0.25) +
  tm_layout(title = "Vertical Fuel Contribution", legend.position = c("left", "bottom"))
map_vert
tmap_save(map_vert, filename = "outputs/vert_sppV.1.tiff")
