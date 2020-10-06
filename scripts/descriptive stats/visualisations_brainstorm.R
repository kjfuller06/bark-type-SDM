# script for visualising species selections

library(raster)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(survminer)

# read records in again for plotting
records = st_read("data/HorseyV.2_extracted_dataV.1.shp")
st_geometry(records) = NULL
records = drop_na(records)

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
spp = records$spp_shr

# simplest metrics first
b1.12 = function(x) {
  ggplot(records, aes(bio4, aridity, color = x)) + 
    geom_point()+
    xlab("Temperature Seasonality (standard deviation ×100)")+
    ylab("Aridity")}
ggarrange(b1.12(r), b1.12(b1), b1.12(b2), b1.12(spp) + 
               theme(legend.position = "none"),
             ncol = 2,
             nrow = 2,
             align = "v")

ggplot(records, aes(bio1, bio12, group = r)) + 
  geom_point(aes(color = b1), shape = r, size = 3)+
  xlab("Annual Mean Temperature")+
  ylab("Annual Precipitation")

# let's shift gears. I'll do some density stuff instead
with(records, plot(bio12 ~ prop.table(ribbons)))

tab = prop.table(table(data$sexo,), margin=1)
tab = as.data.frame(tab)

ggplot(tab,aes(x=Var2,y=Freq,fill=Var1)) + geom_col()

barplot(prop.table(table(data$sexo,data$pais), margin=1))


hist(records$bio1)
records$bark1 = as.factor(records$bark1)
records %>% 
  group_by(bark1) %>% 
  tally()


newdf<-records[,9:28]
newdf$bio6<-newdf$bio6-min(newdf$bio6)
library(vegan)
newdf<-decostand(newdf, method = "hellinger")
dukebutt<-prcomp(newdf)
biplot(dukebutt)
sites<-as.data.frame(dukebutt$x)
newnew = cbind(records[1:8], sites)

with(newnew, plot(PC2 ~ PC1, col = ribbons))

ggplot(newnew, aes(PC1, PC2, color = ribbons))+
  geom_point()
ggplot(newnew, aes(PC1, PC2, color = bark1))+
  geom_point()
ggplot(newnew, aes(PC1, PC2, color = bark2))+
  geom_point()
ggplot(newnew, aes(PC1, PC2, color = spp_shr))+
  geom_point()+
  theme(legend.position = "none")

library()
newdf<-decostand

ggplot(records, aes(bio4, aridity, color = ribbons)) + 
  geom_point()+
  xlab("Temperature Seasonality (standard deviation ×100)")+
  ylab("Aridity")


# density function against each environmental variable
