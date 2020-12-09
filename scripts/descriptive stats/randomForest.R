library(raster)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(tmap)
library(randomForest)

# testing random forest ####
records = read.csv("data/HorseyV.4_extracted_dataV.5_P-A_allenv.csv")

records = drop_na(records)
# records$fueltype = as.factor(records$fueltype)
### can't use fuel type yet because it has too many categories for a categorical variable

# run random forest for a selected species
rf_sp1 = randomForest( x=records[,118:148], y=as.factor(records[,1]), ntree=1000, nodesize=10, importance =T)
rf_sp1
# check importance of variables
importance(rf_sp1)

df = data.frame(sp = seq(1:116), "correct0" = seq(1:116), "false1" = seq(1:116), "false0" = seq(1:116), "correct1" = seq(1:116), class.error0 = seq(1:116), class.error1 = seq(1:116))

df = data.frame(sp = names(records[52]), "correct0" = rf_sp1$confusion[1, 1], "false1" = rf_sp1$confusion[1, 2], "false0" = rf_sp1$confusion[2, 1], "correct1" = rf_sp1$confusion[2, 2], class.error0 = rf_sp1$confusion[1, 3], class.error1 = rf_sp1$confusion[2, 3])

write.csv(df, "outputs/randomForest_HorseyV.4_extracted_dataV.5_P-!_allenv.csv", row.names = FALSE)

# let's look at a bark type
# add all observations of the following species to create the stringybark category
records$stringy = records$Eucalyptus.agglomerata + 
  records$Eucalyptus.baileyana + 
  records$Eucalyptus.blaxlandii + 
  records$Eucalyptus.caliginosa + 
  records$Eucalyptus.cameronii + 
  records$Eucalyptus.capitellata + 
  records$Eucalyptus.cinerea + 
  records$Eucalyptus.eugenioides + 
  records$Eucalyptus.fastigata + 
  records$Eucalyptus.globoidea + 
  records$Eucalyptus.laevopinea + 
  records$Eucalyptus.macrorhyncha + 
  records$Eucalyptus.microcorys + 
  records$Eucalyptus.muelleriana + 
  records$Eucalyptus.obliqua + 
  records$Eucalyptus.resinifera + 
  records$Eucalyptus.sparsifolia + 
  records$Eucalyptus.tindaliae + 
  records$Eucalyptus.williamsiana + 
  records$Eucalyptus.youmanii + 
  records$Syncarpia.glomulifera
# replace non-zero observations with 1
records$stringy[records$stringy != 0] = 1

# run random forest for a selected species
rf_b1 = randomForest( x=records[,118:148], y=as.factor(records[,151]), ntree=1000, nodesize=10, importance =T)
rf_b1
# check importance of variables
importance(rf_b1)
