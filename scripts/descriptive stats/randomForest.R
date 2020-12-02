library(raster)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(tmap)
library(randomForest)

# testing random forest ####
records = read.csv("data/HorseyV.4_extracted_dataV.4_P-A_allenv.csv")

records = drop_na(records)
# records$fueltype = as.factor(records$fueltype)
### can't use fuel type yet because it has too many categories for a categorical variable

# run random forest for a selected species
rf_sp1 = randomForest( x=records[,118:137], y=as.factor(records[,52]), ntree=1000, nodesize=10, importance =T)
# check importance of variables
importance(rf_sp1)


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
rf_b1 = randomForest( x=records[,118:137], y=as.factor(records[,140]), ntree=1000, nodesize=10, importance =T)
# check importance of variables
importance(rf_b1)
