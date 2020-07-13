# Assign library path
.libPaths("C:/Users/90946112/R/win-library/3.6.2")
library(tidyverse)
library(dplyr)

euks_EUCLID = read.csv("eucalyptus_barktypes.csv", colClasses = c("character", "character"))
euks_BioNet = read.csv("euks_bionet.csv", col.names = "eucalypts", colClasses = "character")

euks_joined = left_join(euks_BioNet,euks_EUCLID[,c(2:3)])
write.csv(euks_joined, "eucalyptus_joined.csv")
