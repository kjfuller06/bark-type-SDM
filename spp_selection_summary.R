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
with(rib[['n']], plot(density(bio1)$y, type = 'l'))

# write a loop to plot all three df's in rib
colours <- brewer.pal(3, "Dark2")
colours[3] = "white"
plot(1, type="n", xlab= "Annual Mean Temperature", ylab="Density", xlim=c(min(b$x), max(b$x)), ylim=c(0, 0.03))
legend("topleft", legend = c("No ribboning", "Some ribboning", "Prolific ribboning"), col = c(colours[1:2], 1), lty = 1)
d = density(records$bio1)
points(d, type = 'l', col = "red")
# polygon(c(seq(max(d$x), min(d$x), length.out = length(d$y)), seq(min(d$x), max(d$x), length.out = length(d$y)), c(rep(0, length(d$y)), d$y), col = "red"))
# ^ this doesn't work. Needs fixing.
for(i in c(1:3)){
  a = density(rib[[i]]$bio1)
  why = length(a$y)
  ex = max(a$x)
  exx = min(a$x)
  points(a, type = 'l', col = colours[i])
  # with(rib[[i]], plot(density(bio1)$y, type = 'h', col = adjustcolor(colours[i], alpha = 0.5)))
  polygon(c(seq(ex, exx, length.out = why), seq(exx, ex, length.out = why)), c(rep(0, why), a$y), col = adjustcolor(colours[i], alpha = 0.5))
}
dev.off()

## Still need to scale the density data to 1: divide all values by the sum of all values.
