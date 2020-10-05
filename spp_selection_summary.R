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

# do some environmental plotting
r = records$ribbons
b1 = records$bark1
b2 = records$bark2

# visualisations:
#   1) proportion of points occurring along an environmental gradient (as in, the proportion of temp observations that occur at each temperature), coloured and shaded by group- these could be curves for single variables and hexbins for two variables
#   2) could also do that precip by temp visualisation Jeff sent me for the oaks project- with the overall distribution of all species in gray and individual species highlighted in red

# 1. ####
# split records by ribboning type. the new df will be a list of three df's, named rib[['n']], rib[['s']] and rib[['y']]
rib = split(records, records$ribbons)

# write a loop to plot all three df's in rib
colours <- brewer.pal(3, "Dark2")
xmin = min(density(rib[[1]]$bio1)$x, density(rib[[2]]$bio1)$x, density(rib[[3]]$bio1)$x)
xmax = max(density(rib[[1]]$bio1)$x, density(rib[[2]]$bio1)$x, density(rib[[3]]$bio1)$x)
plot(1, type="n", xlab= "Annual Mean Temperature", ylab="Density", xlim=c(xmin, xmax), ylim=c(0, 0.008))
legend("topleft", legend = c("No ribboning", "Some ribboning", "Prolific ribboning"), col = c(colours[1:3]), lty = 1)
# polygon(c(seq(xmax1, xmin1, length.out = leny1), seq(xmin1, xmax1, length.out = leny1)), c(rep(0, leny1), m1$y), col = adjustcolor("red", alpha = 0.5), border = "red")
# ^ this doesn't work. Needs fixing.
for(i in c(1:3)){
  a = density(rib[[i]]$bio1)
  a$y = a$y/sum(a$y)
  lenya = length(a$y)
  xmaxa = max(a$x)
  xmina = min(a$x)
  polygon(c(seq(xmaxa, xmina, length.out = lenya), seq(xmina, xmaxa, length.out = lenya)), c(rep(0, lenya), a$y), col = adjustcolor(colours[i], alpha = 0.5), border = colours[i])
}
dev.off()

## Still need to scale the density data to 1: divide all values by the sum of all values.
