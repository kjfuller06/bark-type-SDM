library(raster)
library(tmap)
library(slga)
library(sf)
options(stringsAsFactors = FALSE)

nsw = st_read("data/NSW_sans_islands.shp")

doParallel::registerDoParallel()
soc = get_soils_data(product = 'NAT',
                 attribute = 'SOC',
                 component = "ALL",
                 depth = 1,
                 aoi = extent(nsw),
                 'value')

soc = stack(soc)

qtm(soc)
