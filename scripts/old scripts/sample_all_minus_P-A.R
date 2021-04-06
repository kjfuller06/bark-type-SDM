# assign library path
.libPaths("C:/Users/90946112/R/win-library/3.6.2")
library(tidyverse)

# load dataset
flora <- read.delim("data samples/BioNet_allflorasurvey.txt", header = TRUE, sep = "\t", dec = ".")

# select observations with information in 5 select columns- those related to cover estimates/abundance- and records of eucalypt species only
sample2 <- flora %>% 
  dplyr::filter(is.na(CoverScore)==FALSE|is.na(AbundanceScore)==FALSE|is.na(PercentCover)==FALSE|is.na(LowerHeight)==FALSE|is.na(UpperHeight)==FALSE) %>% 
  dplyr::filter(grepl("Eucalyptus", Assgn_ScientificName))
sample2 <- droplevels(sample2)

# write to csv
write.csv(sample2,"data samples/all_minus_P-A_data.csv")

