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

# load base maps
nsw = st_read("data/NSW_sans_islands.shp") %>% 
  st_set_crs(4326)
veg = raster("data/fuels_30m.tif")

# read records in again for plotting
records = read.csv("data/dataextract_allsites_30m.csv") %>% 
  dplyr::select(lon,
                lat,
                fueltype,
                proj_fire_final_30m,
                proj_NDVI_30m,
                proj_ai.pet_1,
                proj_ai.pet_14,
                proj_bioclim_4_30m,
                proj_bioclim_12_30m,
                proj_bioclim_5_30m,
                proj_soilAWC_D1_30m,
                proj_soilCLY_D1_30m,
                proj_soilPHC_D1_30m,
                proj_soildes_30m,
                proj_dem_s,
                proj_dem_TPI_30m,
                proj_dem_TWI_30m,
                proj_dem_SRADTot_1215_30m)
traits = read.csv("data/alltraits_site-specific.csv") %>% 
  dplyr::select(Hrsyb1_,
                lon,
                lat)
traits$Hrsyb1_ = as.factor(traits$Hrsyb1_)
levels(traits$Hrsyb1_)[6:10] = c("box-type",
                                 "peppermint",
                                 "fibrous-rough",
                                 "fibrous-stringy",
                                 "tessellated")
records = full_join(records, traits) %>% 
  drop_na()
names(records)[ncol(records)] = "bark1"
records_sf = records %>% 
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(veg)) %>% 
  st_transform(crs = 4326)
records$lon = st_coordinates(records_sf)[,1]
records$lat = st_coordinates(records_sf)[,2]

# visualisations ####
#   1) proportion of points occurring along an environmental gradient (as in, the proportion of temp observations that occur at each temperature), coloured and shaded by group- these could be curves for single variables and hexbins for two variables
#   2) could also do that precip by temp visualisation Jeff sent me for the oaks project- with the overall distribution of all species in gray and individual species highlighted in red

# 1a. functions ####
# write a loop to plot all three df's in rib. Then loop this through all variables
# create labels for plots
nom = read.csv("data/env_variable_labels2.csv") %>% 
  filter(variable %in% names(records))
nom = nom[match(names(records[c(1:ncol(records)-1)]), nom$variable),]
nom = c(paste(nom$label1, nom$label2, sep = "\n"))
numberofrecordscol = length(names(records)) - length(nom)

# function for plots with yaxt
withlabels = function(frst){
  mins = c()
  maxs = c()
  for(a in c(1:length(df))){
    mins[a] = min(density(df[[a]][,frst])$x)
    maxs[a] = max(density(df[[a]][,frst])$x)
  }
  xmin = min(mins)
  xmax = max(maxs)
  par(mar = c(4, 2, 0.5, 0))
  plot(1, type="n", xlab= nom[frst], ylab="", xlim=c(xmin, xmax), ylim=c(0, 0.015))
  for(i in c(1:length(df))){
    a = density(df[[i]][,frst])
    a$y = a$y/sum(a$y)
    lenya = length(a$y)
    xmaxa = max(a$x)
    xmina = min(a$x)
    polygon(c(seq(xmaxa, xmina, length.out = lenya), seq(xmina, xmaxa, length.out = lenya)), c(rep(0, lenya), a$y), col = adjustcolor(colours[i], alpha = 0.5), border = colours[i])
  }
}

# function for plots without yaxt
withoutlabels = function(allothers){
  for(k in c(allothers)){
    mins = c()
    maxs = c()
    for(a in c(1:length(df))){
      mins[a] = min(density(df[[a]][,k])$x)
      maxs[a] = max(density(df[[a]][,k])$x)
    }
    xmin = min(mins)
    xmax = max(maxs)
    par(mar = c(4, 0, 0.5, 0))
    plot(1, type="n", xlab= nom[k], ylab="", yaxt = 'n', xlim=c(xmin, xmax), ylim=c(0, 0.015))
    for(i in c(1:length(df))){
      a = density(df[[i]][,k])
      a$y = a$y/sum(a$y)
      lenya = length(a$y)
      xmaxa = max(a$x)
      xmina = min(a$x)
      polygon(c(seq(xmaxa, xmina, length.out = lenya), seq(xmina, xmaxa, length.out = lenya)), c(rep(0, lenya), a$y), col = adjustcolor(colours[i], alpha = 0.5), border = colours[i])
    }
  }
}

# 1b. ribbons ####
# split records by categorical variable. 
df = split(records, records$ribbons)

# select colours
colours <- rainbow(length(df))

# write multipanel tiff to disk
tiff(file = "outputs/ribboning3.tiff", width =2200, height = 1100, units = "px", res = 200)
par(mfrow = c(3, 7))

# legend
plot(1, type="n", xaxt = 'n', yaxt = 'n', bty = 'n', xlim=c(0, 0.01), ylim=c(0, 0.01), ann = FALSE)
par(mar = c(4, 0, 0.5, 0))
legend("center", legend = c("No ribboning", "Some ribboning", "Prolific ribboning"), col = c(colours[1:3]), lty = 1, lwd = 5, y.intersp = 2)

withlabels(5)
withoutlabels(c(6:10))
withlabels(11)
withoutlabels(12:17)
withlabels(18)
withoutlabels(19:24)
dev.off()


# 1c. species ####
# split records by categorical variable. 
tal = records %>% 
  group_by(spp_shr) %>% 
  tally() %>% 
  filter(n < 5)
df = split(records[!c(records$spp_shr %in% tal$spp_shr),], records[!c(records$spp_shr %in% tal$spp_shr),]$spp_shr)

# select colours
colours <- rainbow(length(df))

# write to disk
tiff(file = "outputs/species4.tiff", width =2200, height = 1100, units = "px", res = 200)
par(mfrow = c(3, 7))

# legend
plot(1, type="n", xaxt = 'n', yaxt = 'n', bty = 'n', xlim=c(0, 0.01), ylim=c(0, 0.01), ann = FALSE)
par(mar = c(4, 0, 0.5, 0))
legend("center", legend = levels(as.factor(records$spp_shr)), col = c(colours[1:length(df)]), lty = 1, lwd = 5, cex = 0.5)

withlabels(5)
withoutlabels(c(6:10))
withlabels(11)
withoutlabels(12:17)
withlabels(18)
withoutlabels(19:24)
dev.off()

# 1d. bark types 1 ####
# split records by categorical variable. 
df = split(records, records$bark1)

# select colours
colours <- rainbow(length(df))

# create map for plots
map_b1 = tm_shape(nsw) + 
  tm_borders() + 
  tm_shape(records_sf) + 
  tm_dots("bark1", palette = c("smooth" = colours[1],
                               "smooth with stocking" = colours[2], 
                               "halfbark" = colours[3],
                               "ironbark" = colours[4],  
                               "tessellated" = colours[5],
                               "box-type" = colours[6], 
                               "fibrous-rough" = colours[7], 
                               "peppermint" = colours[8],  
                               "fibrous-stringy" = colours[9],
                               "stringybark" = colours[10]),
          legend.show = FALSE, size = 0.25) +
  tm_layout(legend.position = c("right", "bottom"))
# save image to file
tmap_save(map_b1, filename = "outputs/b1V.3.png")
# load image back and create plot
# load back the image as an R object with the "PNG" package
my_image <- readPNG("outputs/b1V.3.png")

# write to disk
tiff(file = "outputs/bark1.poster2.tiff", width =4400, height = 2200, units = "px", res = 400)
layout.matrix = matrix(c(1, 1, 2, 3, 4, 5,  
                         1, 1, 2, 6, 7, 8,
                         9, 10, 11, 12, 13, 14,
                         15, 16, 17, 18, 19, 20
),
nrow = 4, ncol = 6,
byrow = TRUE)
layout(mat = layout.matrix,
       heights = 1,
       widths = 1)

# map
par(mar = c(1.5, 1.75, 2, 0.75))
plot(1:2, type='n', main="", xaxt = "n", yaxt = 'n', axes = FALSE, ann = FALSE)
# Get the plot information so the image will fill the plot box, and draw it
lim <- par()
rasterImage(my_image, 
            xleft=1, xright=2, 
            ybottom=1, ytop=2)

# legend
par(mar = c(0, 0, 0, 0))
plot(1, type="n", xaxt = 'n', yaxt = 'n', bty = 'n', xlim=c(0, 0.01), ylim=c(0, 0.01), ann = FALSE)
legend("center", 
       legend = c("smooth",
                  "smooth with stocking", 
                  "halfbark",
                  "ironbark",  
                  "tessellated",
                  "box-type", 
                  "fibrous-rough", 
                  "peppermint",  
                  "fibrous-stringy",
                  "stringybark"), 
       col = c(colours[1:length(df)]), 
       lty = 1, 
       lwd = 5, 
       cex = 1,
       bty = "n")

par(mar = c(4, 0, 0.5, 0))
withlabels(1)
withoutlabels(2:3)
withlabels(4)
withoutlabels(5:6)
withlabels(7)
withoutlabels(8:12)
withlabels(13)
withoutlabels(14:18)

dev.off()


# 1e. bark types 2 ####
# split records by categorical variable. 
df = split(records, records$bark2)

# select colours
colours <- rainbow(length(df))

# write to disk
tiff(file = "outputs/bark2_V.4.tiff", width =2200, height = 1100, units = "px", res = 200)
par(mfrow = c(3, 7))

# legend
plot(1, type="n", xaxt = 'n', yaxt = 'n', bty = 'n', xlim=c(0, 0.01), ylim=c(0, 0.01), ann = FALSE)
par(mar = c(4, 0, 0.5, 0))
legend("center", legend = levels(as.factor(records$bark2)), col = c(colours[1:length(df)]), lty = 1, lwd = 5)

withlabels(5)
withoutlabels(c(6:10))
withlabels(11)
withoutlabels(12:17)
withlabels(18)
withoutlabels(19:24)
dev.off()

# 1f. vertical fuel contribution ####
# select only relevant categories
vert_rec = records[records$vrt_cnt != "n",]
vert_rec_sf = records_sf[records_sf$vrt_cnt != "n",]
# select colours
colours <- rainbow(length(df))
# create map for final panel
map_vert = tm_shape(nsw) + 
  tm_borders() + 
  tm_shape(vert_rec_sf) + 
  tm_dots("vrt_cnt", palette = c(high = colours[1], low = colours[2]), legend.show = FALSE, size = 0.25) +
  tm_layout(legend.position = c("right", "bottom"))
# save image to file
tmap_save(map_vert, filename = "outputs/vertV.1.png")
# load image back and create plot
# load back the image as an R object with the "PNG" package
my_image <- readPNG("outputs/vertV.1.png")

# split records by categorical variable.
df = split(vert_rec, vert_rec$vrt_cnt)

# select colours
colours <- rainbow(length(df))

# write to disk
tiff(file = "outputs/vert_contribV.1.tiff", width =2200, height = 1100, units = "px", res = 200)
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
legend("center", legend = levels(as.factor(vert_rec$vrt_cnt)), col = c(colours[1:length(df)]), lty = 1, lwd = 5)

withlabels(8)
withoutlabels(c(9:11))
withlabels(12)
withoutlabels(13:16)
withlabels(17)
withoutlabels(18:23)
withlabels(24)
withoutlabels(25:28)

dev.off()

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
