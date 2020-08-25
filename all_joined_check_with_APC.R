library(tidyverse)
library(dplyr)

# Load datasets
EUCLID = read.csv("APC_name_check_EUCLID.csv", colClasses = c("NULL", rep("character", 5), "NULL", "character", rep("NULL", 16)))
BioNet = read.csv("APC_name_check_BioNet.csv", colClasses = c("NULL", rep("character", 5), "NULL", "character", rep("NULL", 16)))
All = read.csv("EUCLID_manual_BioNet_joinV.3_checking-1.csv", colClasses = c("NULL", rep("character", 6)))
All = All %>% 
  filter(EUCLID_checked != "Absent in NSW")

# Join APC search with BioNet and EUCLID accepted names
All = full_join(BioNet, All, by = c("Search.term" = "BioNet_assigned"))
All = full_join(All, EUCLID, by = c("EUCLID_checked" = "Search.term"))
All = All %>% 
  dplyr::select(-"Census.x", -"Census.y", -"secondHybridParentNameID", -"Modification.type", -"Notes", -"Name.status")
names(All)[c(4, 5, 9, 10)] = c("Matched.name.BioNet",
                              "Name.type.BioNet",
                              "Matched.name.EUCLID",
                              "Name.type.EUCLID")
All = unique(All)

# write csv
write.csv(All, "EUCLID_manual_BioNet_joinV.4_checking-1.csv")
