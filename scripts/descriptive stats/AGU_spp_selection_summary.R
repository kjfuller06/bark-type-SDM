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
records_sf = st_read("data/HorseyV.4_extracted_dataV.4.shp")

# create df with no geometry column
records = records_sf
st_geometry(records) = NULL
records = drop_na(records)

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

# 1d. bark types 1 ####
# split records by categorical variable. 
df = split(records, records$bark1)

# select colours
colours <- rainbow(length(df))

# create map for plots
map_b1 = tm_shape(nsw) + 
  tm_borders() + 
  tm_shape(records_sf) + 
  tm_dots("bark1", palette = c("half bark" = colours[1],
                               "ironbark" = colours[2], 
                               "smooth" = colours[3],
                               "smooth with stocking" = colours[4],    
                               "stringybark" = colours[5],
                               "subfibrous - box" = colours[6],        
                               "subfibrous - peppermint" = colours[7],
                               "subfibrous - rough" = colours[8],     
                               "subfibrous - stringy" = colours[9],
                               "subfibrous - tessellated" = colours[10]),
          legend.show = FALSE, size = 0.25) +
  tm_layout(legend.position = c("right", "bottom"))
# save image to file
tmap_save(map_b1, filename = "outputs/b1V.2.png")
# load image back and create plot
# load back the image as an R object with the "PNG" package
my_image <- readPNG("outputs/b1V.2.png")

# write to disk
tiff(file = "outputs/bark1.V.7.tiff", width =1100, height = 1100, units = "px", res = 200)
layout.matrix = matrix(c(1, 1, 2, 2,
                         1, 1, 2, 2,
                         3, 4, 5, 6,
                         7, 8, 9, 10
),
nrow = 4, ncol = 4,
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
par(mar = c(0, 0, 0, 0))
legend("center", title = "Bark Types", legend = levels(as.factor(records$bark1)), col = c(colours[1:length(df)]), lty = 1, lwd = 5, cex = 1.5)

par(mar = c(4, 0, 0.5, 0))
withlabels(9)
withoutlabels(c(12, 14, 15))
withlabels(17)
withoutlabels(c(18, 24, 27))


dev.off()
