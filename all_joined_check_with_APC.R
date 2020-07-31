library(tidyverse)
library(dplyr)

# Load datasets
EUCLID = read.csv("APC_name_check_EUCLID.csv", colClasses = c("NULL", rep("character", 7), rep("NULL", 16)))
BioNet = read.csv("APC_name_check_BioNet.csv", colClasses = c("NULL", rep("character", 7), rep("NULL", 16)))
All = read.csv("EUCLID_manual_BioNet_joinV.3_checking-1.csv", colClasses = c("NULL", rep("character", 6)))

backup = All
# Join EUCLID data before joining with BioNet
All = left_join(All, BioNet, by = c("BioNet_assigned" = "Search.term"))

