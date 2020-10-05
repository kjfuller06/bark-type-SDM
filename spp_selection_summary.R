# script for visualising species selections
library(raster)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(RColorBrewer)

# read records in again for plotting
records = st_read("data/HorseyV.2_extracted_dataV.1.shp")
st_geometry(records) = NULL
records = drop_na(records)

# visualisations:
#   1) proportion of points occurring along an environmental gradient (as in, the proportion of temp observations that occur at each temperature), coloured and shaded by group- these could be curves for single variables and hexbins for two variables
#   2) could also do that precip by temp visualisation Jeff sent me for the oaks project- with the overall distribution of all species in gray and individual species highlighted in red

# 1. ####
# split records by ribboning type. the new df will be a list of three df's, named rib[['n']], rib[['s']] and rib[['y']]
rib = split(records, records$ribbons)

# write a loop to plot all three df's in rib. Then loop this through all variables
colours <- brewer.pal(3, "Dark2")
nom = c("Annual Mean Temperature",
        "Mean Diurnal Range (Mean of\nmonthly (max temp - min temp))",
        "Isothermality\n(BIO2/BIO7) (×100)",
        "Temperature Seasonality\n(standard deviation ×100)",
        "Max Temperature of\nWarmest Month",
        "Min Temperature of\nColdest Month",
        "Temperature Annual\nRange (BIO5-BIO6)",
        "Mean Temperature of\nWettest Quarter",
        "Mean Temperature of\nDriest Quarter",
        "Mean Temperature of\nWarmest Quarter",
        "Mean Temperature of\nColdest Quarter",
        "Annual Precipitation",
        "Precipitation of\nWettest Month",
        "Precipitation of\nDriest Month",
        "Precipitation Seasonality\n(Coefficient of Variation)",
        "Precipitation of\nWettest Quarter",
        "Precipitation of\nDriest Quarter",
        "Precipitation of\nWarmest Quarter",
        "Precipitation of\nColdest Quarter",
        "Aridity")

# write to disk
tiff(file = "ribboning1.tiff", width =2200, height = 1100, units = "px", res = 200)
par(mfrow = c(3, 7))

# legend
plot(1, type="n", xaxt = 'n', yaxt = 'n', bty = 'n', xlim=c(xmin, xmax), ylim=c(0, 0.01), ann = FALSE)
par(mar = c(4, 0, 0.5, 0))
legend("center", legend = c("No ribboning", "Some ribboning", "Prolific ribboning"), col = c(colours[1:3]), lty = 1, lwd = 5, y.intersp = 2)

# plot with yaxt
withlabels = function(frst){
  xmin = min(density(rib[[1]][,frst])$x, density(rib[[2]][,frst])$x, density(rib[[3]][,frst])$x)
  xmax = max(density(rib[[1]][,frst])$x, density(rib[[2]][,frst])$x, density(rib[[3]][,frst])$x)
  par(mar = c(4, 2, 0.5, 0))
  plot(1, type="n", xlab= nom[1], ylab="", xlim=c(xmin, xmax), ylim=c(0, 0.015))
  for(i in c(1:3)){
    a = density(rib[[i]][,frst])
    a$y = a$y/sum(a$y)
    lenya = length(a$y)
    xmaxa = max(a$x)
    xmina = min(a$x)
    polygon(c(seq(xmaxa, xmina, length.out = lenya), seq(xmina, xmaxa, length.out = lenya)), c(rep(0, lenya), a$y), col = adjustcolor(colours[i], alpha = 0.5), border = colours[i])
  }
}
withoutlabels = function(allothers){
  for(k in c(allothers)){
    xmin = min(density(rib[[1]][,k])$x, density(rib[[2]][,k])$x, density(rib[[3]][,k])$x)
    xmax = max(density(rib[[1]][,k])$x, density(rib[[2]][,k])$x, density(rib[[3]][,k])$x)
    par(mar = c(4, 0, 0.5, 0))
    plot(1, type="n", xlab= nom[k-8], ylab="", yaxt = 'n', xlim=c(xmin, xmax), ylim=c(0, 0.015))
    for(i in c(1:3)){
      a = density(rib[[i]][,k])
      a$y = a$y/sum(a$y)
      lenya = length(a$y)
      xmaxa = max(a$x)
      xmina = min(a$x)
      polygon(c(seq(xmaxa, xmina, length.out = lenya), seq(xmina, xmaxa, length.out = lenya)), c(rep(0, lenya), a$y), col = adjustcolor(colours[i], alpha = 0.5), border = colours[i])
    }
  }
}

withlabels(9)
withoutlabels(c(10:14))
withlabels(15)
withoutlabels(16:21)
withlabels(22)
withoutlabels(23:28)
dev.off()

## Still need to scale the density data to 1: divide all values by the sum of all values.
