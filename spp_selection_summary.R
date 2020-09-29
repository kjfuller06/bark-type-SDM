# script for visualising species selections

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
