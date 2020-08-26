library(tidyverse)
library(dplyr)

# Load datasets
# APC name search using EUCLID_checked names
EUCLID = read.csv("APC_name_check_EUCLID.csv", colClasses = c("NULL", rep("factor", 2), rep("NULL", 21)))
# APC name search using BioNet_assigned names
BioNet = read.csv("APC_name_check_BioNet.csv", colClasses = c("NULL", "factor", "NULL", "factor", rep("NULL", 21)))
# File of naming agreements between BioNet and EUCLID (manual web search)
All = read.csv("EUCLID_manual_BioNet_joinV.3_checking-1.csv", colClasses = c("NULL", rep("factor", 4), rep("NULL", 2)))

# Allow NA to be a factor level to allow for filtering
All$EUCLID_checked = addNA(All$EUCLID_checked)

# Remove all records that do not occur in NSW
All = All %>% 
  dplyr::filter(EUCLID_checked != "Absent in NSW" & EUCLID_checked != "Match found")

# Join APC search with BioNet and EUCLID accepted names
All = full_join(BioNet, All, by = c("Search.term" = "BioNet_assigned"))
All = full_join(All, EUCLID, by = c("EUCLID_checked" = "Search.term"))
All = All %>% 
  dplyr::select("Confirmed_name", "Search.term", "BioNet_unassigned", "EUCLID_checked", "EUCLID_no_match")
names(All)[1:2] = c("APC_confirmed_name", "BioNet_assigned")

# Allow NA to be a factor level to allow for filtering
All$APC_confirmed_name = addNA(All$APC_confirmed_name)
All$BioNet_assigned = addNA(All$BioNet_assigned)

# Remove 
All = All %>%
  dplyr::filter(APC_confirmed_name != "Excluded from APC" &
                  BioNet_assigned != "Angophora spp." &
                  BioNet_assigned != "Corymbia spp." &
                  BioNet_assigned != "Eucalyptus spp.")

# write csv
write.csv(All, "EUCLID_BioNet_APC_joinV.1.csv", row.names = FALSE)
