library(raster)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(RColorBrewer)

# Extract environmental data for species records ####
# load dataset and map of Australia
## need to select only columns of interest from "records". Some Aus columns are in there from the join in the previous script. I'm going back to look at all the processing steps to double-check everything.
records = st_read("data/Horsey_sampleV.2.shp") %>%  
  dplyr::select('ID',
                'Assg_SN',
                'DatFrst',
                'DateLst',
                'spp_shr',
                'bark1',
                'bark2',
                'ribbons',
                'geometry')
aus = st_read("data/australia.shp")

# plot
# ggplot(records)+
#   geom_sf(data = aus)+
#   geom_sf(aes(colour = Assg_SN))+
#   theme(legend.position = "none")

# get WorldClim data
# WorldClim metadata notes are in my data layers metadata xlsx
r2.5 = getData('worldclim', var = 'bio', res = 2.5, path = "data/")
# reduce data size by cropping raster by extent of NSW
nsw = st_read("data/NSW_sans_islands.shp") %>% 
  st_set_crs(4326)
# use nsw bbox to crop the raster image
r2.5_crop = crop(x = r2.5, y = nsw)
# plot(r2.5_crop[['bio1']])
# extract WorldClim values at species occurrence locations and cbind to records df
records = cbind(records, raster::extract(r2.5_crop, st_coordinates(records), methods = 'simple'))

# perform same processing for aridity data. Data were downloading by following this link: https://cgiarcsi.community/2019/01/24/global-aridity-index-and-potential-evapotranspiration-climate-database-v2/
arid = raster('data/ai_et0/ai_et0.tif')
# use nsw bbox to crop the raster image
arid_crop = crop(x = arid, y = nsw)
# plot(arid_crop)
# extract aridity values at species occurrence locations and cbind to records df
records = cbind(records, aridity = raster::extract(arid_crop, st_coordinates(records), methods = 'simple'))

# can't do any prediction from here. Data are at different spatial res and extent. But this gives me data for plotting species envelopes.

# Plot environmental axes for each species ####
tiff(file = "frequency_HorseyV.2_records_by_species.tiff", width =2200, height = 750, units = "px", res = 200)
# counts
ggplot(data.frame(records), aes(x=spp_shr)) +
  geom_bar()+
  theme(axis.text.x = element_text(angle = 45,
                                   size = 7,
                                   hjust = 1))+ 
  coord_cartesian(ylim=c(0, 50))
dev.off()
## some species have only 2-3 records

# write shapefile to disk and convert sf to df, only because I'm so bad at this with sf's
st_write(records, "data/HorseyV.2_extracted_dataV.1.shp", delete_layer = TRUE)

# read records in again for plotting
records = st_read("data/HorseyV.2_extracted_dataV.1.shp")
st_geometry(records) = NULL

# rename WorldClim variables by description
# names(records)[9:27] = c(
#   "Annual Mean Temperature",
#   "Mean Diurnal Range (Mean of monthly (max temp - min temp))",
#   "Isothermality (BIO2/BIO7) (×100)",
#   "Temperature Seasonality (standard deviation ×100)",
#   "Max Temperature of Warmest Month",
#   "Min Temperature of Coldest Month",
#   "Temperature Annual Range (BIO5-BIO6)",
#   "Mean Temperature of Wettest Quarter",
#   "Mean Temperature of Driest Quarter",
#   "Mean Temperature of Warmest Quarter",
#   "Mean Temperature of Coldest Quarter",
#   "Annual Precipitation",
#   "Precipitation of Wettest Month",
#   "Precipitation of Driest Month",
#   "Precipitation Seasonality (Coefficient of Variation)",
#   "Precipitation of Wettest Quarter",
#   "Precipitation of Driest Quarter",
#   "Precipitation of Warmest Quarter",
#   "Precipitation of Coldest Quarter"
# )
# ^ this was a terrible idea

# do some environmental plotting
r = records$ribbons
b1 = records$bark1
b2 = records$bark2

# simplest metrics first
b1.12 = function(x) {
  ggplot(records, aes(bio1, bio12, color = x)) + 
    geom_point()+
    xlab("Annual Mean Temperature")+
    ylab("Annual Precipitation")}
b1.12(r)
b1.12(b1)
b1.12(b2)
