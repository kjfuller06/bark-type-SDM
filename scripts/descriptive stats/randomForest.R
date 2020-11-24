library(raster)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(tmap)
library(randomForest)

# testing random forest ####
records = read.csv("data/HorseyV.4_extracted_dataV.4_P-A.csv")

(m_rf <- randomForest( x=records[,114:133], y=records[,90], ntree=1000, nodesize=10, importance =T))

importance(m_rf,type=1)
