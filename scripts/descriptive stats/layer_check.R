# this script is for collecting and examining data layers for analysis
## errors occurred when trying to download the organic carbon stocks for NSW from ISRIC
library(raster)
library(tidyverse)
library(sf)
library(rgdal)
library(tmap)
library(gdalUtils)
library(XML)

# check for crs ####
# get codes
codes = make_EPSG()

# load vegetation data
veg = raster("data/FuelTypeV2_FuelLUT1.tif")
# look for crs code in "codes"
# lcc = codes[grep("proj=lcc", codes$prj4),]
# lcc = lcc[grep("ellps=GRS80", lcc$prj4),]
# lcc = lcc[grep("+lat_0=-33.25", lcc$prj4),]
# EPSG is either 3308 for GDA94 / NSW Lambert or 8058 for GDA2020 / NSW Lambert
## use 8058

# have to figure out the crs for this file
records = st_read("data/spp_selection_forLDA.shp") %>% 
  st_transform(crs = 3308)

tmap_mode("view")
tm_shape(veg)+tm_raster()+tm_shape(records[records$lng_spt != "n",])+tm_dots(col = "lng_spt")

# bind veg values to records df
records = cbind(records, fuel = raster::extract(veg, st_coordinates(records), methods = 'simple'))

records_df = records
st_geometry(records_df) = NULL
records_df$fuel = as.factor(records_df$fuel)
records_df$spp_shr = as.factor(records_df$spp_shr)
records_df = unique(records_df)

tal = records_df %>% 
  group_by(fuel) %>% 
  tally()

# check out WorldClim data
r2.5 = getData('worldclim', var = 'bio', res = 2.5, path = "data/")
# look for crs code in "codes"
r_check = codes[grep("proj=longlat", codes$prj4),]
r_check = r_check[grep("datum=WGS84", r_check$prj4),]
# use code 4326

# check out the aridity data
arid = raster('data/ai_et0/ai_et0.tif')
# same as above

# examining collinearity ####
# load datasets
r2.5 = getData('worldclim', var = 'bio', res = 2.5, path = "data/")
veg = raster("data/fuels_reproj.tif")
arid = raster("data/aridity_reproj.tif")
nsw = st_read("data/NSW_sans_islands.shp")

# clean WorldClim data
nsw1 = nsw %>% 
  st_transform(crs = st_crs(r2.5))
r2.5.1 = crop(r2.5, extent(nsw1))
r2.5.2 = projectRaster(r2.5.1, crs = crs(veg))

# stack datasets
stck = raster::stack(veg, r2.5.2, arid)

# create pairs plots
# pairs(stck[[c(4, 6:7, 9, 10, 16, 19)]])

# select orthogonal variables
stck = stck[[c(1, 4, 6:7, 9, 10, 16, 19)]]

# create fire history layer ####
## I actually don't know where these originated. They were uploaded to the Hub's CloudStor by Trent Penman
r1919 = raster("data/fireyeartifs/fireyear1919.tif")
r1925 = raster("data/fireyeartifs/fireyear1925.tif")
r_add = r1919 + r1925

#pattern = "*.tif$" - filters for main raster files only and skips any associated files (e.g. world files)
grids <- list.files("./data/fireyeartifs" , pattern = "*.tif$")

#create a raster stack from the input raster files 
r_all <- raster::stack(paste0("./data/fireyeartifs/", grids))

# count the number of occurrences in which each pixel is greater than 1
firehist = sum(!is.na(r_all)) 

writeRaster(firehist, "data/firehistory.tif")

## get soil data ####
# bulk density ####
voi = "bdod" # variable of interest
depth = "0-5cm"
quantile = "mean"

voi_layer = paste(voi,depth,quantile, sep="_") # layer of interest 

wcs_path = paste0("https://maps.isric.org/mapserv?map=/map/",voi,".map") # Path to the WCS. See maps.isric.org
wcs_service = "SERVICE=WCS"
wcs_version = "VERSION=2.0.1" # This works for gdal >=2.3; "VERSION=1.1.1" works with gdal < 2.3.

bb=c(15676000,-3134000,16918000,-4176000) # bounding box (homolosine)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection

wcs = paste(wcs_path,wcs_service,wcs_version,sep="&") # This works for gdal >= 2.3

l1 <- newXMLNode("WCS_GDAL")
l1.s <- newXMLNode("ServiceURL", wcs, parent=l1)
l1.l <- newXMLNode("CoverageName", "bdod_0-5cm_mean", parent=l1)

# Save to local disk
xml.out = "./data/sg.xml"
saveXML(l1, file = xml.out)

# Download raster as GeoTIFF (Warning: it can be large!)
file.out <- './data/bulkdensity.tif'

gdal_translate(xml.out, file.out,
               tr=c(250,250), projwin=bb,
               projwin_srs =igh, co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"),
               verbose=TRUE
)

bdod = raster("data/bulkdensity.tif")
nsw = st_read("data/NSW_sans_islands.shp") %>% 
  st_transform(crs = crs(bdod))

# CEC ####
voi = "cec" # variable of interest
depth = "0-5cm"
quantile = "mean"

voi_layer = paste(voi,depth,quantile, sep="_") # layer of interest 

wcs_path = paste0("https://maps.isric.org/mapserv?map=/map/",voi,".map") # Path to the WCS. See maps.isric.org
wcs_service = "SERVICE=WCS"
wcs_version = "VERSION=2.0.1" # This works for gdal >=2.3; "VERSION=1.1.1" works with gdal < 2.3.

bb=c(15676000,-3134000,16918000,-4176000) # Example bounding box (homolosine)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection

wcs = paste(wcs_path,wcs_service,wcs_version,sep="&") # This works for gdal >= 2.3

l1 <- newXMLNode("WCS_GDAL")
l1.s <- newXMLNode("ServiceURL", wcs, parent=l1)
l1.l <- newXMLNode("CoverageName", "cec_0-5cm_mean", parent=l1)

# Save to local disk
xml.out = "./data/sg.xml"
saveXML(l1, file = xml.out)

# Download raster as GeoTIFF (Warning: it can be large!)
file.out <- './data/cec.tif'

gdal_translate(xml.out, file.out,
               tr=c(250,250), projwin=bb,
               projwin_srs =igh, co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"),
               verbose=TRUE
)

cec = raster("data/cec.tif")

# coarse fragments ####
voi = "cfvo" # variable of interest
depth = "0-5cm"
quantile = "mean"

voi_layer = paste(voi,depth,quantile, sep="_") # layer of interest 

wcs_path = paste0("https://maps.isric.org/mapserv?map=/map/",voi,".map") # Path to the WCS. See maps.isric.org
wcs_service = "SERVICE=WCS"
wcs_version = "VERSION=2.0.1" # This works for gdal >=2.3; "VERSION=1.1.1" works with gdal < 2.3.

bb=c(15676000,-3134000,16918000,-4176000) # Example bounding box (homolosine)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection

wcs = paste(wcs_path,wcs_service,wcs_version,sep="&") # This works for gdal >= 2.3

l1 <- newXMLNode("WCS_GDAL")
l1.s <- newXMLNode("ServiceURL", wcs, parent=l1)
l1.l <- newXMLNode("CoverageName", "cfvo_0-5cm_mean", parent=l1)

# Save to local disk
xml.out = "./data/sg.xml"
saveXML(l1, file = xml.out)

# Download raster as GeoTIFF (Warning: it can be large!)
file.out <- './data/coarsefragments.tif'

gdal_translate(xml.out, file.out,
               tr=c(250,250), projwin=bb,
               projwin_srs =igh, co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"),
               verbose=TRUE
)

cfvo = raster("data/coarsefragments.tif")

# sand fraction ####
voi = "sand" # variable of interest
depth = "0-5cm"
quantile = "mean"

voi_layer = paste(voi,depth,quantile, sep="_") # layer of interest 

wcs_path = paste0("https://maps.isric.org/mapserv?map=/map/",voi,".map") # Path to the WCS. See maps.isric.org
wcs_service = "SERVICE=WCS"
wcs_version = "VERSION=2.0.1" # This works for gdal >=2.3; "VERSION=1.1.1" works with gdal < 2.3.

bb=c(15676000,-3134000,16918000,-4176000) # Example bounding box (homolosine)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection

wcs = paste(wcs_path,wcs_service,wcs_version,sep="&") # This works for gdal >= 2.3

l1 <- newXMLNode("WCS_GDAL")
l1.s <- newXMLNode("ServiceURL", wcs, parent=l1)
l1.l <- newXMLNode("CoverageName", "sand_0-5cm_mean", parent=l1)

# Save to local disk
xml.out = "./data/sg.xml"
saveXML(l1, file = xml.out)

# Download raster as GeoTIFF (Warning: it can be large!)
file.out <- './data/sand.tif'

gdal_translate(xml.out, file.out,
               tr=c(250,250), projwin=bb,
               projwin_srs =igh, co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"),
               verbose=TRUE
)

sand = raster("data/sand.tif")
# silt fraction ####
voi = "silt" # variable of interest
depth = "0-5cm"
quantile = "mean"

voi_layer = paste(voi,depth,quantile, sep="_") # layer of interest 

wcs_path = paste0("https://maps.isric.org/mapserv?map=/map/",voi,".map") # Path to the WCS. See maps.isric.org
wcs_service = "SERVICE=WCS"
wcs_version = "VERSION=2.0.1" # This works for gdal >=2.3; "VERSION=1.1.1" works with gdal < 2.3.

bb=c(15676000,-3134000,16918000,-4176000) # Example bounding box (homolosine)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection

wcs = paste(wcs_path,wcs_service,wcs_version,sep="&") # This works for gdal >= 2.3

l1 <- newXMLNode("WCS_GDAL")
l1.s <- newXMLNode("ServiceURL", wcs, parent=l1)
l1.l <- newXMLNode("CoverageName", "silt_0-5cm_mean", parent=l1)

# Save to local disk
xml.out = "./data/sg.xml"
saveXML(l1, file = xml.out)

# Download raster as GeoTIFF (Warning: it can be large!)
file.out <- './data/silt.tif'

gdal_translate(xml.out, file.out,
               tr=c(250,250), projwin=bb,
               projwin_srs =igh, co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"),
               verbose=TRUE
)

silt = raster("data/silt.tif")


# clay fraction ####
voi = "clay" # variable of interest
depth = "0-5cm"
quantile = "mean"

voi_layer = paste(voi,depth,quantile, sep="_") # layer of interest 

wcs_path = paste0("https://maps.isric.org/mapserv?map=/map/",voi,".map") # Path to the WCS. See maps.isric.org
wcs_service = "SERVICE=WCS"
wcs_version = "VERSION=2.0.1" # This works for gdal >=2.3; "VERSION=1.1.1" works with gdal < 2.3.

bb=c(15676000,-3134000,16918000,-4176000) # Example bounding box (homolosine)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection

wcs = paste(wcs_path,wcs_service,wcs_version,sep="&") # This works for gdal >= 2.3

l1 <- newXMLNode("WCS_GDAL")
l1.s <- newXMLNode("ServiceURL", wcs, parent=l1)
l1.l <- newXMLNode("CoverageName", "clay_0-5cm_mean", parent=l1)

# Save to local disk
xml.out = "./data/sg.xml"
saveXML(l1, file = xml.out)

# Download raster as GeoTIFF (Warning: it can be large!)
file.out <- './data/clay.tif'

gdal_translate(xml.out, file.out,
               tr=c(250,250), projwin=bb,
               projwin_srs =igh, co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"),
               verbose=TRUE
)

clay = raster("data/clay.tif")


# nitrogen ####
voi = "nitrogen" # variable of interest
depth = "0-5cm"
quantile = "mean"

voi_layer = paste(voi,depth,quantile, sep="_") # layer of interest 

wcs_path = paste0("https://maps.isric.org/mapserv?map=/map/",voi,".map") # Path to the WCS. See maps.isric.org
wcs_service = "SERVICE=WCS"
wcs_version = "VERSION=2.0.1" # This works for gdal >=2.3; "VERSION=1.1.1" works with gdal < 2.3.

bb=c(15676000,-3134000,16918000,-4176000) # Example bounding box (homolosine)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection

wcs = paste(wcs_path,wcs_service,wcs_version,sep="&") # This works for gdal >= 2.3

l1 <- newXMLNode("WCS_GDAL")
l1.s <- newXMLNode("ServiceURL", wcs, parent=l1)
l1.l <- newXMLNode("CoverageName", "nitrogen_0-5cm_mean", parent=l1)

# Save to local disk
xml.out = "./data/sg.xml"
saveXML(l1, file = xml.out)

# Download raster as GeoTIFF (Warning: it can be large!)
file.out <- './data/nitrogen.tif'

gdal_translate(xml.out, file.out,
               tr=c(250,250), projwin=bb,
               projwin_srs =igh, co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"),
               verbose=TRUE
)

nitrogen = raster("data/nitrogen.tif")
# pH ####
voi = "phh2o" # variable of interest
depth = "0-5cm"
quantile = "mean"

voi_layer = paste(voi,depth,quantile, sep="_") # layer of interest 

wcs_path = paste0("https://maps.isric.org/mapserv?map=/map/",voi,".map") # Path to the WCS. See maps.isric.org
wcs_service = "SERVICE=WCS"
wcs_version = "VERSION=2.0.1" # This works for gdal >=2.3; "VERSION=1.1.1" works with gdal < 2.3.

bb=c(15676000,-3134000,16918000,-4176000) # Example bounding box (homolosine)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection

wcs = paste(wcs_path,wcs_service,wcs_version,sep="&") # This works for gdal >= 2.3

l1 <- newXMLNode("WCS_GDAL")
l1.s <- newXMLNode("ServiceURL", wcs, parent=l1)
l1.l <- newXMLNode("CoverageName", "phh2o_0-5cm_mean", parent=l1)

# Save to local disk
xml.out = "./data/sg.xml"
saveXML(l1, file = xml.out)

# Download raster as GeoTIFF (Warning: it can be large!)
file.out <- './data/pH.tif'

gdal_translate(xml.out, file.out,
               tr=c(250,250), projwin=bb,
               projwin_srs =igh, co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"),
               verbose=TRUE
)

ph = raster("data/pH.tif")


# soil organic carbon ####
voi = "soc" # variable of interest
depth = "0-5cm"
quantile = "mean"

voi_layer = paste(voi,depth,quantile, sep="_") # layer of interest 

wcs_path = paste0("https://maps.isric.org/mapserv?map=/map/",voi,".map") # Path to the WCS. See maps.isric.org
wcs_service = "SERVICE=WCS"
wcs_version = "VERSION=2.0.1" # This works for gdal >=2.3; "VERSION=1.1.1" works with gdal < 2.3.

bb=c(15676000,-3134000,16918000,-4176000) # Example bounding box (homolosine)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection

wcs = paste(wcs_path,wcs_service,wcs_version,sep="&") # This works for gdal >= 2.3

l1 <- newXMLNode("WCS_GDAL")
l1.s <- newXMLNode("ServiceURL", wcs, parent=l1)
l1.l <- newXMLNode("CoverageName", "soc_0-5cm_mean", parent=l1)

# Save to local disk
xml.out = "./data/sg.xml"
saveXML(l1, file = xml.out)

# Download raster as GeoTIFF (Warning: it can be large!)
file.out <- './data/soc.tif'

gdal_translate(xml.out, file.out,
               tr=c(250,250), projwin=bb,
               projwin_srs =igh, co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"),
               verbose=TRUE
)

soc = raster("data/soc.tif")


# organic carbon density ####
voi = "ocd" # variable of interest
depth = "0-5cm"
quantile = "mean"

voi_layer = paste(voi,depth,quantile, sep="_") # layer of interest 

wcs_path = paste0("https://maps.isric.org/mapserv?map=/map/",voi,".map") # Path to the WCS. See maps.isric.org
wcs_service = "SERVICE=WCS"
wcs_version = "VERSION=2.0.1" # This works for gdal >=2.3; "VERSION=1.1.1" works with gdal < 2.3.

bb=c(15676000,-3134000,16918000,-4176000) # Example bounding box (homolosine)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection

wcs = paste(wcs_path,wcs_service,wcs_version,sep="&") # This works for gdal >= 2.3

l1 <- newXMLNode("WCS_GDAL")
l1.s <- newXMLNode("ServiceURL", wcs, parent=l1)
l1.l <- newXMLNode("CoverageName", "ocd_0-5cm_mean", parent=l1)

# Save to local disk
xml.out = "./data/sg.xml"
saveXML(l1, file = xml.out)

# Download raster as GeoTIFF (Warning: it can be large!)
file.out <- './data/ocd.tif'

gdal_translate(xml.out, file.out,
               tr=c(250,250), projwin=bb,
               projwin_srs =igh, co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"),
               verbose=TRUE
)

ocd = raster("data/ocd.tif")


# organic carbon stocks- errors ####
voi = "ocs" # variable of interest
depth = "0-5cm"
quantile = "mean"

voi_layer = paste(voi,depth,quantile, sep="_") # layer of interest 

wcs_path = paste0("https://maps.isric.org/mapserv?map=/map/",voi,".map") # Path to the WCS. See maps.isric.org
wcs_service = "SERVICE=WCS"
wcs_version = "VERSION=2.0.1" # This works for gdal >=2.3; "VERSION=1.1.1" works with gdal < 2.3.

bb=c(15676000,-3134000,16918000,-4176000) # Example bounding box (homolosine)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection

wcs = paste(wcs_path,wcs_service,wcs_version,sep="&") # This works for gdal >= 2.3

l1 <- newXMLNode("WCS_GDAL")
l1.s <- newXMLNode("ServiceURL", wcs, parent=l1)
l1.l <- newXMLNode("CoverageName", "ocs_0-5cm_mean", parent=l1)

# Save to local disk
xml.out = "./data/sg.xml"
saveXML(l1, file = xml.out)

# Download raster as GeoTIFF (Warning: it can be large!)
file.out <- './data/ocs.tif'

gdal_translate(xml.out, file.out,
               tr=c(250,250), projwin=bb,
               projwin_srs =igh, co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"),
               verbose=TRUE
)

ocs = raster("data/ocs.tif")


# check out some fire history polygons from RFS ####
fires2 = st_read("data/NSWRFS_FireHistory_shapefiles_20200326/PFH_20200326.shp")
tm_shape(fires2)+tm_borders()

# and some other datasets I got from SEED ####
## polygons of prescribed and wild fires
fires3 = st_read("data/firenpwsfirehistory/NPWSFireHistory_17122020.shp")
## this is fire severity for the 2019-2020 fire season
fires4 = raster("data/fesm20200420/cvmsre_NSW_20200420_ag7l0.tif")

