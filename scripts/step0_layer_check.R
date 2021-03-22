# this script is for collecting and examining data layers for analysis
## errors occurred when trying to download the organic carbon stocks for NSW from ISRIC
library(raster)
library(tidyverse)
library(tidync)
library(sf)
library(rgdal)
library(tmap)
library(ncdf4)
library(RNetCDF)
library(stars)
library(gdalUtils)
library(ncdf)
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

# get ISRIC soil data ####
# bulk density ###
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

# CEC ###
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

# coarse fragments ###
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

# sand fraction ###
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
# silt fraction ###
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


# clay fraction ###
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


# nitrogen ###
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
# pH ###
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


# soil organic carbon ###
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


# organic carbon density ###
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


# organic carbon stocks- errors ###
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


# DEM-H ####
dem = raster("data/DEM/71498/a05f7893-0050-7506-e044-00144fdd4fa6/hdr.adf")
dem

# crop DEM with NSW extent shapefile
# reproject shapefile
nsw = nsw %>% 
  st_transform(crs = st_crs(dem))
bb = extent(nsw)

# add ~11km of space around the NSW extent so slopes can be calculated accurately
bb[1] = bb[1] - 0.1
bb[2] = bb[2] + 0.1
bb[3] = bb[3] - 0.1
bb[4] = bb[4] + 0.1
dem = crop(dem, bb)
tm_shape(dem)+tm_raster()+tm_shape(nsw)+tm_borders()

# write to disk
writeRaster(dem, "data/DEM_nsw.tif", overwrite = TRUE)

# post-transfer-through-FileZilla layer check ####
setwd("D:/chapter1")
nsw = st_read("NSW_sans_islands.shp")

# CSIRO soils ####
# no good, plus the script for downloading them is already written for parallel computing so can go ahead and submit that- just modify it to run all soil data at once on all cores

# CGIARCSI PET and aridity ####
# stil in original extent and crs
setwd("D:/PhD data")

arid_annual = raster("CGIARCSI/ai_et0/ai_et0.tif")
# tmap_mode("view")
tm_shape(arid_annual)+tm_raster()
res(arid_annual)
crs(arid_annual)

PET_annual = raster("CGIARCSI/et0_yr/et0_yr.tif")
# tmap_mode("view")
tm_shape(PET_annual)+tm_raster()
res(PET_annual)
crs(PET_annual)

PET_mo = raster("CGIARCSI/et0_mo/et0_12.tif")
# tmap_mode("view")
tm_shape(PET_mo)+tm_raster()
res(PET_mo)
crs(PET_mo)

# DEM-H ####
# variables invalid
# dem = raster("DEM-H/DEM_nsw.sgrd")
# dem = raster("DEM-H/71498/a05f7893-0050-7506-e044-00144fdd4fa6/hdr.adf")
# DEM-H broken, downloading DEM-S

# DEM-S ####
# crop DEM with NSW extent shapefile
nsw = st_read("NSW_sans_islands.shp")
nsw = nsw %>% 
  st_transform(crs = st_crs(dem))
bb = extent(nsw)

# add ~11km of space around the NSW extent so slopes can be calculated accurately
gdalwarp(te = c(139.9995, -38.50506, 154.6387, -27.15702),
         srcfile = "70715/aac46307-fce8-449d-e044-00144fdd4fa6/hdr.adf",
         dstfile = "70715/dem_s.tif")

dem = raster("70715/dem_s.tif")

# fuel types ####
fuels = raster("RFS Fuel Type/fuels_30m.tif")
tm_shape(fuels)+tm_raster()
for_fuels = raster("RFS Fuel Type/for_fuels_30m.tif")
# tmap_mode("view")
tm_shape(for_fuels)+tm_raster()

# land tenure ####
## check overlap with forest fuels layer
tenure = st_read("landnswlanduse2013/Land_NSW_Landuse_2013/Data/Shapefile/NSW_Landuse_2013.shp")
# 1043185 features
# tm_shape(tenure)+tm_borders()
extent(tenure)
extent(nsw)
tenure$SecondaryA = as.numeric(tenure$SecondaryA)
natural = tenure %>% 
  filter(SecondaryA < 210)
# 103934 features
# plot(natural["geometry"])
# rast_natural = natural %>% 
#   group_by(SecondaryA) %>% 
#   st_union()
# backup = rast_natural
# # create dummy raster
# ex_ext = extent(fuels)
# ex_ext[1] = extent(fuels)[1] + (extent(fuels)[2]-extent(fuels)[1])/2.0001
# ex_ext[2] = extent(fuels)[2] - (extent(fuels)[2]-extent(fuels)[1])/2.0001
# ex_ext[3] = extent(fuels)[3] + (extent(fuels)[4]-extent(fuels)[3])/2.0001
# ex_ext[4] = extent(fuels)[4] - (extent(fuels)[4]-extent(fuels)[3])/2.0001
# # crop and reproject ideal dataset to get desired res for land tenure layer
# example = crop(fuels, ex_ext)
# example = projectRaster(example, crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs", method = 'ngb')
# 
# st_bbox(rast_natural)
# example2 = raster(extent(rast_natural), resolution = res(example), crs = crs(rast_natural))
# rast_natural = rasterize(rast_natural, example)
# 
# gdal_rasterize(src_datasource = "landnswlanduse2013/Land_NSW_Landuse_2013/Data/Shapefile/NSW_Landuse_2013.shp", 
#          dst_filename = "landnswlanduse2013/tenure_raster.tif",
#          b = c(120, 130, 140),
#          burn = c(1, 2, 3),
#          te = st_bbox(natural),
#          tr = res(example))
# 
# # fire history ####
# fires = raster("fireyeartifs/firehistory.tif")
# tm_shape(fires)+tm_raster()
# fires80 = raster("fireyeartifs/fire_reproj_80m.tif")
# tm_shape(fires80)+tm_raster()
## doesn't work. rasterizing takes too long

tmap_mode("view")
tm_shape(fuels)+tm_raster()+tm_shape(natural)+tm_borders()
## timed out

# WorldClim vars ####
bioclim = mosaic(raster("wc0.5/bio_310/bio1_310.bil"),
                 raster("wc0.5/bio_311/bio1_311.bil"),
                 raster("wc0.5/bio_410/bio1_410.bil"),
                 raster("wc0.5/bio_411/bio1_411.bil"), fun = mean)
names(bioclim)[1] = "bio1"
for(i in c(2:19)){
  x = mosaic(raster(paste("wc0.5/bio_310/bio", i, "_310.bil", sep = "")),
             raster(paste("wc0.5/bio_311/bio", i, "_311.bil", sep = "")),
             raster(paste("wc0.5/bio_410/bio", i, "_410.bil", sep = "")),
             raster(paste("wc0.5/bio_411/bio", i, "_411.bil", sep = "")), fun = mean)
  bioclim = raster::stack(bioclim, x)
  names(bioclim)[i] = paste0("bio", i, sep = "")
}
crs(bioclim) = st_crs(4326)$proj4string
nsw = st_read("NSW_sans_islands.shp")
nsw = nsw %>% 
  st_transform(crs = st_crs(bioclim))
bb = extent(nsw)
bioclim2 = crop(bioclim, bb)
writeRaster(bioclim2, "wc0.5/bioclim_cropped.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
bioclim2 = stack("wc0.5/bioclim_cropped.grd")

# NDVI ####
## all file extents set to:
#     North = -27
#     South = -38
#     West = 139
#     East = 154
## error in the dataset- May repeated, no December file; added Dec, 2011 to complete the year

files = list.files("./NDVI" , pattern = "*.nc$", full.names = TRUE)
name = "ndvi-1km-1month"

ndvi = nc_open(files[1])

# extract lon, lat and time data
lon = ncvar_get(ndvi, "longitude")
lat = ncvar_get(ndvi, "latitude")

# extract ndvi values and attributes
var_array = ncvar_get(ndvi, name)
var_nans = ncatt_get(ndvi, name, "_FillValue")

nc_close(ndvi)

# replace fill values with NANs
var_array[var_array == var_nans$value] = NA

# create df
lonlat = as.matrix(expand.grid(lon, lat))
var_vec = as.vector(var_array)
var_df = data.frame(cbind(lonlat, var_vec))
names(var_df) <- c(paste("lon", as.character(1), sep = "_"),
                   paste("lat", as.character(1), sep = "_"),
                   paste(name,as.character(1), sep="_"))

for(i in c(2:length(files))){
  ndvi = nc_open(files[i])
  
  # extract lon, lat and time data
  lon = ncvar_get(ndvi, "longitude")
  lat = ncvar_get(ndvi, "latitude")
  
  # extract ndvi values and attributes
  var_array = ncvar_get(ndvi, name)
  var_nans = ncatt_get(ndvi, name, "_FillValue")
  
  nc_close(ndvi)
  
  # replace fill values with NANs
  var_array[var_array == var_nans$value] = NA
  
  # create df
  lonlat = as.matrix(expand.grid(lon, lat))
  var_vec = as.vector(var_array)
  var_df2 = data.frame(cbind(lonlat, var_vec))
  var_df = cbind(var_df, var_df2)
  names(var_df)[c(1:3)+(i-1)*3] <- c(paste("lon", as.character(i), sep = "_"),
                                 paste("lat", as.character(i), sep = "_"),
                                 paste(name,as.character(i), sep="_"))
}

# for(i in c(1:12)){
#   print(any(var_df[,c(1+(i-1)*3)] != var_df[,c(1+(i-1)*3+3)]))
# }
# for(i in c(1:12)){
#   print(any(var_df[,c(2+(i-1)*3)] != var_df[,c(5+(i-1)*3)]))
# }
# print(any(var_df[,c(1+(13-1)*3)] != var_df[,c(1)]))
# print(any(var_df[,c(2+(13-1)*3)] != var_df[,c(2)]))
## all lon/lat data equal for NDVI files
names(var_df)
var_df = var_df[,c(1, 2, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36, 39)]
names(var_df) = c("lon", "lat", "Dec11", "Jan12", "Feb12", "Mar12", "Apr12", "May12", "May12.2", "Jun12", "Jul12", "Aug12", "Sep12", "Oct12", "Nov12")

# check out file 6- dates listed same as file 5
ndvi = nc_open(files[6])
print(ndvi)
t5 <- ncvar_get(ndvi,"time")
ndvi = nc_open(files[7])
t6 <- ncvar_get(ndvi,"time")
## times are equal
## values are equal


# ndvi_min = data.frame(lon = var_df$lon, lat = var_df$lat, var_min = apply(var_df[,c(3:7,9:15)], 1, FUN = min, na.rm = FALSE))
# 
# code = codes[codes$code == 3112,]$prj4
# ndvi_min_r = raster(ndvi_min$var_min, xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat), crs = CRS(code))
# 
# coordinates(ndvi_min) = ~ lon + lat
# gridded(ndvi_min) = TRUE

## doesn't work. Need to go directly from netCDF to raster to avoid issues with coordinates

files = list.files("./NDVI" , pattern = "*.nc$", full.names = TRUE)
files = files[c(1:7, 9:13)]
name = "ndvi-1km-1month"
ndvi = raster(files[1])
names(ndvi)[1] = paste0("mo",1)

for(i in c(2:12)){
  x = raster(files[i])
  names(x) = paste0("mo",i)
  ndvi = stack(ndvi, x)
}

ndvi_min = stackApply(ndvi, indices = 1, fun = min, na.rm = TRUE)
names(ndvi_min) = "min"
ndvi_max = stackApply(ndvi, indices = 1, fun = max, na.rm = TRUE)
names(ndvi_max) = "max"
ndvi_mean = stackApply(ndvi, indices = 1, fun = mean, na.rm = TRUE)
names(ndvi_mean) = "mean"
ndvi = stack(ndvi_min, ndvi_mean, ndvi_max)

nsw1 = nsw %>% 
  st_transform(crs = crs(ndvi))
ndvi_final = crop(ndvi, nsw1)

writeRaster(ndvi_final, "NDVI/NDVI_cropped.grd", format = "raster", options = "COMPRESS=DEFLATE", overwrite = TRUE)
