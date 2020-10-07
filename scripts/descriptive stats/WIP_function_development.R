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

# 1a. functions ####
vis1 = function(records, var){
  # split records by categorical variable. 
  df = split(records, var)
  # select colours
  colours <- rainbow(length(df))
  # create labels for plots
  nom = read.csv("data/env_variable_labels.csv")
  nom = c(paste(nom$labels, nom$labels2, sep = "\n"))
  round(sqrt(nrow(nom)))
  
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
    plot(1, type="n", xlab= nom[frst-8], ylab="", xlim=c(xmin, xmax), ylim=c(0, 0.015))
    for(i in c(1:3)){
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
      plot(1, type="n", xlab= nom[k-8], ylab="", yaxt = 'n', xlim=c(xmin, xmax), ylim=c(0, 0.015))
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
}

# 1b. ribbons ####
# write multipanel tiff to disk
tiff(file = "outputs/ribboning1.tiff", width =2200, height = 1100, units = "px", res = 200)
par(mfrow = c(3, 7))

# legend
plot(1, type="n", xaxt = 'n', yaxt = 'n', bty = 'n', xlim=c(0, 0.01), ylim=c(0, 0.01), ann = FALSE)
par(mar = c(4, 0, 0.5, 0))
legend("center", legend = c("No ribboning", "Some ribboning", "Prolific ribboning"), col = c(colours[1:3]), lty = 1, lwd = 5, y.intersp = 2)

withlabels(9)
withoutlabels(c(10:14))
withlabels(15)
withoutlabels(16:21)
withlabels(22)
withoutlabels(23:28)
dev.off()

# 1c. species ####
tiff(file = "outputs/ribboning1.tiff", width =2200, height = 1100, units = "px", res = 200)
par(mfrow = c(3, 7))

# legend
plot(1, type="n", xaxt = 'n', yaxt = 'n', bty = 'n', xlim=c(0, 0.01), ylim=c(0, 0.01), ann = FALSE)
par(mar = c(4, 0, 0.5, 0))
legend("center", legend = c("No ribboning", "Some ribboning", "Prolific ribboning"), col = c(colours[1:3]), lty = 1, lwd = 5, y.intersp = 2)

withlabels(9)
withoutlabels(c(10:14))
withlabels(15)
withoutlabels(16:21)
withlabels(22)
withoutlabels(23:28)
dev.off()