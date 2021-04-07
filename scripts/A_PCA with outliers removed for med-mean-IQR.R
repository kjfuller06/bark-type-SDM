library(plotrix); library(vegan); library(Hmisc)
setwd("D:/Oak biogeography data from Kate/oaks_biogeography/oaks_biogeography")

rm(list=ls())   # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
dev.off()       # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
cat("\f")       # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

###############################################################
###############################################################

df<-read.csv("recordsV.1_extractedvalues_annual.csv")
df<-with(df, data.frame(species, prism_ppt, prism_tavg, prism_tmin, prism_tmax,
                        prism_vpdmax, CGIARCSI_aridity, CGIARCSI_PET))
df<-df[complete.cases(df),]


# remove 1st and 99th on clim variables
# chap ####
dat<-subset(df, species == "Quercus chapmanii")
qq<-data.frame(quantile(dat$prism_ppt, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_ppt < hi); dat<-subset(dat, prism_ppt > lo)
qq<-data.frame(quantile(dat$prism_tavg, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_tavg < hi); dat<-subset(dat, prism_tavg > lo)
qq<-data.frame(quantile(dat$prism_tmin, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_tmin < hi); dat<-subset(dat, prism_tmin > lo)
qq<-data.frame(quantile(dat$prism_tmax, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_tmax < hi); dat<-subset(dat, prism_tmax > lo)
qq<-data.frame(quantile(dat$prism_vpdmax, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_vpdmax < hi); dat<-subset(dat, prism_vpdmax > lo)
qq<-data.frame(quantile(dat$CGIARCSI_aridity, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, CGIARCSI_aridity < hi); dat<-subset(dat, CGIARCSI_aridity > lo)
qq<-data.frame(quantile(dat$CGIARCSI_PET, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, CGIARCSI_PET < hi); dat<-subset(dat, CGIARCSI_PET > lo)
chap<-dat

# gemi ####
dat<-subset(df, species == "Quercus geminata")
qq<-data.frame(quantile(dat$prism_ppt, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_ppt < hi); dat<-subset(dat, prism_ppt > lo)
qq<-data.frame(quantile(dat$prism_tavg, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_tavg < hi); dat<-subset(dat, prism_tavg > lo)
qq<-data.frame(quantile(dat$prism_tmin, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_tmin < hi); dat<-subset(dat, prism_tmin > lo)
qq<-data.frame(quantile(dat$prism_tmax, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_tmax < hi); dat<-subset(dat, prism_tmax > lo)
qq<-data.frame(quantile(dat$prism_vpdmax, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_vpdmax < hi); dat<-subset(dat, prism_vpdmax > lo)
qq<-data.frame(quantile(dat$CGIARCSI_aridity, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, CGIARCSI_aridity < hi); dat<-subset(dat, CGIARCSI_aridity > lo)
qq<-data.frame(quantile(dat$CGIARCSI_PET, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, CGIARCSI_PET < hi); dat<-subset(dat, CGIARCSI_PET > lo)
gemi<-dat

# virg  ####
dat<-subset(df, species == "Quercus virginiana")
qq<-data.frame(quantile(dat$prism_ppt, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_ppt < hi); dat<-subset(dat, prism_ppt > lo)
qq<-data.frame(quantile(dat$prism_tavg, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_tavg < hi); dat<-subset(dat, prism_tavg > lo)
qq<-data.frame(quantile(dat$prism_tmin, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_tmin < hi); dat<-subset(dat, prism_tmin > lo)
qq<-data.frame(quantile(dat$prism_tmax, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_tmax < hi); dat<-subset(dat, prism_tmax > lo)
qq<-data.frame(quantile(dat$prism_vpdmax, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_vpdmax < hi); dat<-subset(dat, prism_vpdmax > lo)
qq<-data.frame(quantile(dat$CGIARCSI_aridity, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, CGIARCSI_aridity < hi); dat<-subset(dat, CGIARCSI_aridity > lo)
qq<-data.frame(quantile(dat$CGIARCSI_PET, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, CGIARCSI_PET < hi); dat<-subset(dat, CGIARCSI_PET > lo)
virg<-dat

# marg ####
dat<-subset(df, species == "Quercus margarettae")
qq<-data.frame(quantile(dat$prism_ppt, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_ppt < hi); dat<-subset(dat, prism_ppt > lo)
qq<-data.frame(quantile(dat$prism_tavg, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_tavg < hi); dat<-subset(dat, prism_tavg > lo)
qq<-data.frame(quantile(dat$prism_tmin, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_tmin < hi); dat<-subset(dat, prism_tmin > lo)
qq<-data.frame(quantile(dat$prism_tmax, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_tmax < hi); dat<-subset(dat, prism_tmax > lo)
qq<-data.frame(quantile(dat$prism_vpdmax, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_vpdmax < hi); dat<-subset(dat, prism_vpdmax > lo)
qq<-data.frame(quantile(dat$CGIARCSI_aridity, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, CGIARCSI_aridity < hi); dat<-subset(dat, CGIARCSI_aridity > lo)
qq<-data.frame(quantile(dat$CGIARCSI_PET, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, CGIARCSI_PET < hi); dat<-subset(dat, CGIARCSI_PET > lo)
marg<-dat

# hemi ####
dat<-subset(df, species == "Quercus hemisphaerica")
qq<-data.frame(quantile(dat$prism_ppt, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_ppt < hi); dat<-subset(dat, prism_ppt > lo)
qq<-data.frame(quantile(dat$prism_tavg, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_tavg < hi); dat<-subset(dat, prism_tavg > lo)
qq<-data.frame(quantile(dat$prism_tmin, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_tmin < hi); dat<-subset(dat, prism_tmin > lo)
qq<-data.frame(quantile(dat$prism_tmax, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_tmax < hi); dat<-subset(dat, prism_tmax > lo)
qq<-data.frame(quantile(dat$prism_vpdmax, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_vpdmax < hi); dat<-subset(dat, prism_vpdmax > lo)
qq<-data.frame(quantile(dat$CGIARCSI_aridity, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, CGIARCSI_aridity < hi); dat<-subset(dat, CGIARCSI_aridity > lo)
qq<-data.frame(quantile(dat$CGIARCSI_PET, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, CGIARCSI_PET < hi); dat<-subset(dat, CGIARCSI_PET > lo)
hemi<-dat

# phel ####
dat<-subset(df, species == "Quercus phellos")
qq<-data.frame(quantile(dat$prism_ppt, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_ppt < hi); dat<-subset(dat, prism_ppt > lo)
qq<-data.frame(quantile(dat$prism_tavg, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_tavg < hi); dat<-subset(dat, prism_tavg > lo)
qq<-data.frame(quantile(dat$prism_tmin, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_tmin < hi); dat<-subset(dat, prism_tmin > lo)
qq<-data.frame(quantile(dat$prism_tmax, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_tmax < hi); dat<-subset(dat, prism_tmax > lo)
qq<-data.frame(quantile(dat$prism_vpdmax, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_vpdmax < hi); dat<-subset(dat, prism_vpdmax > lo)
qq<-data.frame(quantile(dat$CGIARCSI_aridity, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, CGIARCSI_aridity < hi); dat<-subset(dat, CGIARCSI_aridity > lo)
qq<-data.frame(quantile(dat$CGIARCSI_PET, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, CGIARCSI_PET < hi); dat<-subset(dat, CGIARCSI_PET > lo)
phel<-dat

# shum ####
dat<-subset(df, species == "Quercus shumardii")
qq<-data.frame(quantile(dat$prism_ppt, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_ppt < hi); dat<-subset(dat, prism_ppt > lo)
qq<-data.frame(quantile(dat$prism_tavg, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_tavg < hi); dat<-subset(dat, prism_tavg > lo)
qq<-data.frame(quantile(dat$prism_tmin, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_tmin < hi); dat<-subset(dat, prism_tmin > lo)
qq<-data.frame(quantile(dat$prism_tmax, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_tmax < hi); dat<-subset(dat, prism_tmax > lo)
qq<-data.frame(quantile(dat$prism_vpdmax, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_vpdmax < hi); dat<-subset(dat, prism_vpdmax > lo)
qq<-data.frame(quantile(dat$CGIARCSI_aridity, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, CGIARCSI_aridity < hi); dat<-subset(dat, CGIARCSI_aridity > lo)
qq<-data.frame(quantile(dat$CGIARCSI_PET, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, CGIARCSI_PET < hi); dat<-subset(dat, CGIARCSI_PET > lo)
shum<-dat

# stel ####
dat<-subset(df, species == "Quercus stellata")
qq<-data.frame(quantile(dat$prism_ppt, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_ppt < hi); dat<-subset(dat, prism_ppt > lo)
qq<-data.frame(quantile(dat$prism_tavg, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_tavg < hi); dat<-subset(dat, prism_tavg > lo)
qq<-data.frame(quantile(dat$prism_tmin, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_tmin < hi); dat<-subset(dat, prism_tmin > lo)
qq<-data.frame(quantile(dat$prism_tmax, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_tmax < hi); dat<-subset(dat, prism_tmax > lo)
qq<-data.frame(quantile(dat$prism_vpdmax, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_vpdmax < hi); dat<-subset(dat, prism_vpdmax > lo)
qq<-data.frame(quantile(dat$CGIARCSI_aridity, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, CGIARCSI_aridity < hi); dat<-subset(dat, CGIARCSI_aridity > lo)
qq<-data.frame(quantile(dat$CGIARCSI_PET, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, CGIARCSI_PET < hi); dat<-subset(dat, CGIARCSI_PET > lo)
stel<-dat

# velu ####
dat<-subset(df, species == "Quercus velutina")
qq<-data.frame(quantile(dat$prism_ppt, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_ppt < hi); dat<-subset(dat, prism_ppt > lo)
qq<-data.frame(quantile(dat$prism_tavg, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_tavg < hi); dat<-subset(dat, prism_tavg > lo)
qq<-data.frame(quantile(dat$prism_tmin, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_tmin < hi); dat<-subset(dat, prism_tmin > lo)
qq<-data.frame(quantile(dat$prism_tmax, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_tmax < hi); dat<-subset(dat, prism_tmax > lo)
qq<-data.frame(quantile(dat$prism_vpdmax, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, prism_vpdmax < hi); dat<-subset(dat, prism_vpdmax > lo)
qq<-data.frame(quantile(dat$CGIARCSI_aridity, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, CGIARCSI_aridity < hi); dat<-subset(dat, CGIARCSI_aridity > lo)
qq<-data.frame(quantile(dat$CGIARCSI_PET, seq(0,1,0.01)))
lo<-qq[[1]][1]; hi<-qq[[1]][100]
dat<-subset(dat, CGIARCSI_PET < hi); dat<-subset(dat, CGIARCSI_PET > lo)
velu<-dat

# bind them ####
df<-rbind(chap, gemi, hemi, marg, phel, shum, stel, velu, virg)
# rm(chap, gemi, hemi, marg, phel, shum, stel, velu, virg, qq, hi, lo)

# PCA ####

dat<-decostand(df[,2:8], method = "range")
mod<-prcomp(dat, scale = T)
# biplot(mod); abline(v=0, h=0 ,lty=2)

sco<-data.frame(scores(mod))
PC1<-data.frame(sco$PC1)
PC2<-data.frame(sco$PC2)
rm(sco)
df<-cbind(df, PC1)
df<-cbind(df, PC2)
rm(dat, PC1, PC2)

# spp selection####
cat("\f")
dat<-subset(df, species == "Quercus velutina")
# extract 'em ####

x1<-mean(dat$sco.PC1)
d1<-median(dat$sco.PC1)
q125<-as.numeric(quantile(dat$sco.PC1)[2]); q175<-as.numeric(quantile(dat$sco.PC1)[4]); abs(q175-q125)
x2<-mean(dat$sco.PC2)
d2<-median(dat$sco.PC2)
q225<-as.numeric(quantile(dat$sco.PC2)[2]); q275<-as.numeric(quantile(dat$sco.PC2)[4]); abs(q275-q225)
hi1<-as.numeric(quantile(dat$sco.PC1,seq(0,1,0.01))[100])
lo1<-as.numeric(quantile(dat$sco.PC1,seq(0,1,0.01))[2])
hi2<-as.numeric(quantile(dat$sco.PC2,seq(0,1,0.01))[100])
lo2<-as.numeric(quantile(dat$sco.PC2,seq(0,1,0.01))[2])

rcorr(df$sco.PC1, df$CGIARCSI_PET)

# PC summaries ####
x1
d1
abs(q175-q125)
x2
d2
abs(q275-q225)
abs(hi1-lo1)
abs(hi2-lo2)



# Raw clim summaries ####

tq5<-as.numeric(quantile(dat$prism_tavg, seq(0,1,0.05))[2])
tq50<-as.numeric(quantile(dat$prism_tavg, seq(0,1,0.05))[11])
tq95<-as.numeric(quantile(dat$prism_tavg, seq(0,1,0.05))[20])
rq5<-as.numeric(quantile(dat$prism_ppt, seq(0,1,0.05))[2])
rq50<-as.numeric(quantile(dat$prism_ppt, seq(0,1,0.05))[11])
rq95<-as.numeric(quantile(dat$prism_ppt, seq(0,1,0.05))[20])

tq5
tq50
tq95
tq95-tq5
rq5
rq50
rq95
rq95-rq5


# Graph of correlations ####

dd<-read.csv("OakSpp_Full climate data.csv")

par(mfrow = c(7,7), mar = c(1,1,1,1))

