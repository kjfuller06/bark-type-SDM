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
tmap_save(map_vert, filename = "outputs/vertV.1.png")

# 1g. short-distance spotting ####
# select only relevant categories
shortspot = records[records$shrt_sp != "n",]
shortspot_sf = records_sf[records_sf$shrt_sp != "n",]
# create map for final panel
map_short = tm_shape(nsw) + 
  tm_borders() + 
  tm_shape(shortspot_sf) + 
  tm_dots("shrt_sp", palette = c(high = colours[1], low = colours[2]), legend.show = FALSE, size = 0.25) +
  tm_layout(legend.position = c("right", "bottom"))
# save image to file
tmap_save(map_short, filename = "outputs/shortV.1.png")
# load image back and create plot
# load back the image as an R object with the "PNG" package
my_image <- readPNG("outputs/shortV.1.png")

# split records by categorical variable.
df = split(shortspot, shortspot$shrt_sp)

# select colours
colours <- rainbow(length(df))

# write to disk
tiff(file = "outputs/short_spotV.1.tiff", width =2200, height = 1100, units = "px", res = 200)
layout.matrix = matrix(c(1, 1, 2, 3, 4, 5, 6,
                         1, 1, 7, 8, 9, 10, 11,
                         12, 13, 14, 15, 16, 17, 18,
                         19, 20, 21, 22, 23, 24, 25
),
nrow = 4, ncol = 7,
byrow = TRUE)
layout(mat = layout.matrix,
       heights = 1,
       widths = 1)

# map
par(mar = c(2, 1.5, 2, 0.5))
plot(1:2, type='n', main="", xaxt = "n", yaxt = 'n', axes = FALSE, ann = FALSE)
# Get the plot information so the image will fill the plot box, and draw it
lim <- par()
rasterImage(my_image, 
            xleft=1, xright=2, 
            ybottom=1, ytop=2)

# legend
plot(1, type="n", xaxt = 'n', yaxt = 'n', bty = 'n', xlim=c(0, 0.01), ylim=c(0, 0.01), ann = FALSE)
par(mar = c(4, 0, 0.5, 0))
legend("center", legend = levels(as.factor(shortspot$shrt_sp)), col = c(colours[1:length(df)]), lty = 1, lwd = 5)

withlabels(8)
withoutlabels(c(9:11))
withlabels(12)
withoutlabels(13:16)
withlabels(17)
withoutlabels(18:23)
withlabels(24)
withoutlabels(25:28)

dev.off()

# 1h. long-distance spotting ####
# select only relevant categories
longspot = records[records$lng_spt != "n",]
longspot_sf = records_sf[records_sf$lng_spt != "n",]
# create map for final panel
map_long = tm_shape(nsw) + 
  tm_borders() + 
  tm_shape(longspot_sf) + 
  tm_dots("lng_spt", palette = c(high = colours[1], low = colours[2]), legend.show = FALSE, size = 0.25) +
  tm_layout(legend.position = c("right", "bottom"))
# save image to file
tmap_save(map_long, filename = "outputs/longV.1.png")
# load image back and create plot
# load back the image as an R object with the "PNG" package
my_image <- readPNG("outputs/longV.1.png")

# split records by categorical variable.
df = split(longspot, longspot$lng_spt)

# select colours
colours <- rainbow(length(df))

# write to disk
tiff(file = "outputs/long_spotV.1.tiff", width =2200, height = 1100, units = "px", res = 200)
layout.matrix = matrix(c(1, 1, 2, 3, 4, 5, 6,
                         1, 1, 7, 8, 9, 10, 11,
                         12, 13, 14, 15, 16, 17, 18,
                         19, 20, 21, 22, 23, 24, 25
),
nrow = 4, ncol = 7,
byrow = TRUE)
layout(mat = layout.matrix,
       heights = 1,
       widths = 1)

# map
par(mar = c(2, 1.5, 2, 0.5))
plot(1:2, type='n', main="", xaxt = "n", yaxt = 'n', axes = FALSE, ann = FALSE)
# Get the plot information so the image will fill the plot box, and draw it
lim <- par()
rasterImage(my_image, 
            xleft=1, xright=2, 
            ybottom=1, ytop=2)

# legend
plot(1, type="n", xaxt = 'n', yaxt = 'n', bty = 'n', xlim=c(0, 0.01), ylim=c(0, 0.01), ann = FALSE)
par(mar = c(4, 0, 0.5, 0))
legend("center", legend = levels(as.factor(longspot$lng_spt)), col = c(colours[1:length(df)]), lty = 1, lwd = 5)

withlabels(8)
withoutlabels(c(9:11))
withlabels(12)
withoutlabels(13:16)
withlabels(17)
withoutlabels(18:23)
withlabels(24)
withoutlabels(25:28)

dev.off()
