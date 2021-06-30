library(tidyverse)
library(data.table)
library(raster)
library(sf)
library(GSIF)
library(rgdal)
library(gdalUtils)

setwd("/glade/scratch/kjfuller/data")
veg = raster("for_fuels_30m.tif")
nsw = st_read("NSW_sans_islands.shp") %>%
  st_transform(crs = st_crs(veg))

# assign number of cores and read in data
setDTthreads(18)
t1 = system.time({
  df = data.table::fread("PCA_values1-14.csv", select = c(1:2, 4))
  df = as.data.frame(df)
  df = st_as_sf(df, coords = c("x", "y"), crs = st_crs(veg))
  nom = names(df)
  n1 = nrow(df)
})[[3]]

tiff("df_check.tiff", width = 500, height = 500, res = 100, units = "px")
plot(st_geometry(df))
dev.off()

obj = rgdal::GDALinfo("for_fuels_30m.tif")
t.spatial = GSIF::getSpatialTiles(obj, block.x = 150000, return.SpatialPolygons = TRUE)
t.non = GSIF::getSpatialTiles(obj, block.x = 150000, return.SpatialPolygons = FALSE)
tiles = SpatialPolygonsDataFrame(t.spatial, t.non)
tiles$id = c(1:nrow(tiles))
t.sf = st_as_sf(tiles)
t.sf = t.sf[nsw,]
tiles = tiles[tiles$id %in% t.sf$id,]
n = nrow(tiles)

tiff("tiles_check.tiff", width = 500, height = 500, res = 100, units = "px")
plot(veg)
plot(st_geometry(t.sf), add = TRUE)
dev.off()

rm(t.spatial, t.non, t.sf)

# write metadata outputs to file
capture.output(
  paste0("time to read 3 columns of PCA_values1-14 and convert to sf = ", t1),
  paste0("names of df = ", list(nom)),
  paste0("nrow of df = ", n1),
  file = "PC1_raster_metastats.txt",
  append = TRUE
)

rastfun = function(x){
  tryCatch({
    r = raster(tiles[x,], res = res(veg))
    df3 = rasterize(df, r, field = 1, fun = mean, update = TRUE)
    writeRaster(df3, paste0("PC01_", x, "_field-1.tif"))
    df3 = rasterize(df, r, field = "PC1", fun = mean, update = TRUE)
    writeRaster(df3, paste0("PC01_", x, "_field-PC1.tif"))
  }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
}

beginCluster(n = 18)
t3 = system.time({
  lapply(25, rastfun)
})[[3]]
endCluster()

# write additional metadata outputs to file
capture.output(
  paste0("time to rasterize PC1 values twice and write to file = ", t3), 
  file = "PC1_raster_metastats.txt",
  append = TRUE
)
