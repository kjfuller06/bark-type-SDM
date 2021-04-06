# script for visualising species selections
library(raster)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(RColorBrewer)
# library(rJava)
library(tmap)
library(tmaptools)
# library(OpenStreetMap)
library(jpeg)
library(png)
library(colorspace)
library(spData)
library(randomForest)

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


# map for #30daymap challenge
# aus = getData(name = "GADM", country = "AUS", level = 1, download = TRUE) %>% 
#   st_as_sf()
# check out WorldClim data
r2.5 = getData('worldclim', var = 'bio', res = 2.5, path = "data/")
precip = r2.5[['bio12']]
tm_shape(precip) + tm_raster(palette = sequential_hcl(20, palette = "Teal"))


# testing random forest ####
# stck is from layer_check.R
stck
records = st_read("data/spp_selection_forLDA.shp") %>% 
  st_transform(crs = st_crs(stck))
records = cbind(records, raster::extract(stck, st_coordinates(records), methods = 'simple'))

# remove wetlands, grasslands, alpine habitat, urban, etc.
types = c(42:46, 52:74)
records = records %>% 
  filter(!(fuels_reproj %in% types))

# map for AGU poster ####
veg = raster("data/fuels_reproj.tif")
labels = read.csv("data/fuels_AGU_legend.csv")
names(labels)[1] = "FuelType"
veg = subs(veg, labels, by = 1, which = 2)
labs = unique(labels$FuelGroupName)
types = c(rev(brewer.pal(5, name = "Blues")), rev(brewer.pal(7, name = "YlGn")), brewer.pal(3, "YlOrRd"), brewer.pal(5, name = "Purples"))
types = types[c(1:3, 6:15, 16, 18, 20)]

tmap_mode('view')

# read_osm(veg, ext = 1.1)
map = tm_shape(veg)+tm_raster(palette = types, style = "cat", labels = labs)+tm_scale_bar()+tm_layout(legend.title.size = 1, legend.text.size = 0.5)

                                                                                          