library(data.table)
library(sf)
library(tidyverse)
library(raster)
library(parallel)
library(snowfall)
library(tmap)

setwd("D:/chapter1/bark-type-SDM/data")
veg = raster("fuels_30m.tif")
veg = raster::aggregate(veg, fact = 30)
nsw = st_read("NSW_sans_islands.shp") %>% 
  st_transform(crs = st_crs(veg))
setwd("D:/chapter1/other_data/Final Masked/bioclim")
bio1 = raster("mask_proj_bioclim_1_30m.tif")
bio1 = raster::aggregate(bio1, fact = 30)
writeRaster(bio1, "bio1_900m.tif")
bio12 = raster("mask_proj_bioclim_12_30m.tif")
bio12 = raster::aggregate(bio12, fact = 30)
writeRaster(bio12, "bio12_900m.tif")

df = data.frame()
for(i in c(1:51)){
  r = veg
  r = r %in% i
  df2 = as.data.frame(rasterToPoints(r), xy = TRUE)
  df2 = df2[df2[3] == 1,]
  df2[3] = i
  df = rbind(df, df2)
}
df = st_as_sf(df, coords = c("x", "y"), crs = st_crs(veg))

df = cbind(df, 
           temp = raster::extract(bio1, st_coordinates(df)),
           precip = raster::extract(bio12, st_coordinates(df)))

names(df)[1] = "veg_type"
df = na.omit(df)
df$temp = df$temp/10

plotfun = function(min, max, label){
  par(oma = c(0,0,0,0))
  par(mfrow = c(1, 2))
  plot(st_geometry(nsw))
  plot(st_geometry(df), col = "grey80", add = TRUE)
  plot(st_geometry(df[df$veg_type >= min & df$veg_type <= max,]), col = "red", add = TRUE)
  with(df, plot(precip ~ temp,
                col = "grey80",
                xlab = "Temperature",
                ylab = "Precipitation"))
  with(df[df$veg_type >= min & df$veg_type <= max,], points(precip ~ temp,
                                     col = "red"))
  mtext(label, side=3, outer=TRUE, line=-3)
}
setwd("D:/chapter1/bark-type-SDM/outputs")

tiff("veg1_rainforests.tiff", width = 1600, height = 900, res = 200, units = "px")
plotfun(1, 1, "Rainforest")
dev.off()

tiff("veg2_wetscler_shrub.tiff", width = 1600, height = 900, res = 200, units = "px")
plotfun(2, 5, "Wet Sclerophyll Forest (shrubby)")
dev.off()

tiff("veg3_wetscler_grass.tiff", width = 1600, height = 900, res = 200, units = "px")
plotfun(6, 10, "Wet Sclerophyll Forest (grassy)")
dev.off()

tiff("veg4_dryscler_shrub.grass.tiff", width = 1600, height = 900, res = 200, units = "px")
plotfun(11, 20, "Dry Sclerophyll Forest (shrubby/grassy)")
dev.off()

tiff("veg5_dryscler_shrub.tiff", width = 1600, height = 900, res = 200, units = "px")
plotfun(21, 34, "Dry Sclerophyll Forest (shrubby)")
dev.off()

tiff("veg6_grassy_woodlands.tiff", width = 1600, height = 900, res = 200, units = "px")
plotfun(35, 41, "Grassy Woodlands")
dev.off()

tiff("veg7_heathlands.tiff", width = 1600, height = 900, res = 200, units = "px")
plotfun(42, 44, "Heathlands")
dev.off()

tiff("veg8_alpine.tiff", width = 1600, height = 900, res = 200, units = "px")
plotfun(45, 46, "Alpine")
dev.off()

tiff("veg9_semi-arid_grassy.tiff", width = 1600, height = 900, res = 200, units = "px")
plotfun(47, 47, "Semi-Arid Woodlands (grassy)")
dev.off()

tiff("veg10_semi-arid_shrub.tiff", width = 1600, height = 900, res = 200, units = "px")
plotfun(48, 51, "Semi-Arid Woodlands (shrubby)")
dev.off()

