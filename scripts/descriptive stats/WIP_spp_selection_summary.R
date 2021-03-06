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
records = records_sf
st_geometry(records) = NULL
records = drop_na(records)
# remove outlier species
# records = records %>% 
#   filter(spp_shr != "E.populneasubsp.bimbil" & spp_shr != "E.largiflorens")

# visualisations ####
#   1) proportion of points occurring along an environmental gradient (as in, the proportion of temp observations that occur at each temperature), coloured and shaded by group- these could be curves for single variables and hexbins for two variables
#   2) could also do that precip by temp visualisation Jeff sent me for the oaks project- with the overall distribution of all species in gray and individual species highlighted in red

# 1a. functions ####
# write a loop to plot all three df's in rib. Then loop this through all variables
# create labels for plots
nom = read.csv("data/env_variable_labels.csv")
nom = c(paste(nom$labels, nom$labels2, sep = "\n"))
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
  plot(1, type="n", xlab= nom[frst - numberofrecordscol], ylab="", xlim=c(xmin, xmax), ylim=c(0, 0.015))
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
    plot(1, type="n", xlab= nom[k - numberofrecordscol], ylab="", yaxt = 'n', xlim=c(xmin, xmax), ylim=c(0, 0.015))
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

# 1f. vertical fuel contribution ####
# select only relevant categories
vert_rec = records[records$vrt_cnt != "n",]
vert_rec_sf = records_sf[records_sf$vrt_cnt != "n",]
# create map for final panel
map_vert = tm_shape(nsw) + 
  tm_borders() + 
  tm_shape(vert_rec_sf[vert_rec_sf$vrt_cnt == "low",]) + 
  tm_dots("spp_shr", palette = sequential_hcl(length(unique(vert_rec$spp_shr[vert_rec$vrt_cnt == "low"])), palette = "Teal"), legend.show = TRUE, size = 0.25) +
  tm_layout(legend.position = c("right", "bottom")) +
  tm_shape(vert_rec_sf[vert_rec_sf$vrt_cnt == "high",]) + 
  tm_dots("spp_shr", palette = sequential_hcl(length(unique(vert_rec$spp_shr[vert_rec$vrt_cnt == "high"])), palette = "OrRd"), legend.show = TRUE, size = 0.25) +
  tm_layout(legend.position = c("left", "bottom"))
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
