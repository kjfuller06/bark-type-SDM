library(tidyverse)
library(dplyr)

# Load datasets
EUCLID = read.csv("APC_name_check_EUCLID.csv", colClasses = c("NULL", rep("character", 5), "NULL", "character", rep("NULL", 16)))
BioNet = read.csv("APC_name_check_BioNet.csv", colClasses = c("NULL", rep("character", 5), "NULL", "character", rep("NULL", 16)))
All = read.csv("EUCLID_manual_BioNet_joinV.3_checking-1.csv", colClasses = c("NULL", rep("character", 6)))

# Join APC search with BioNet and EUCLID accepted names
All = left_join(All, BioNet, by = c("BioNet_assigned" = "Search.term"))
All = left_join(All, EUCLID, by = c("EUCLID_checked" = "Search.term"))
names(All)[7:16] = c("Census.BioNet",
                     "Matched.name.BioNet",
                     "Name.status.BioNet",
                     "Name.type.BioNet",
                     "canonicalName.EUCLID",
                     "Census.EUCLID",
                     "Matched.name.EUCLID",
                     "Name.status.EUCLID",
                     "Name.type.EUCLID",
                     "canonicalName.EUCLID")

# write csv
write.csv(All, "EUCLID_manual_BioNet_joinV.4_checking-1.csv")
