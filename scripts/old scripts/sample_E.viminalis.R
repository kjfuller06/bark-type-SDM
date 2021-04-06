# assign library path
.libPaths("C:/Users/90946112/R/win-library/3.6.2")
library(tidyverse)

flora <- read.delim("data samples/BioNet_allflorasurvey.txt", header = TRUE, sep = "\t", dec = ".")
sample <- flora %>% 
  dplyr::filter(ScientificName == "Eucalyptus viminalis")
sample <- droplevels(sample)
str(sample)
summary(sample)
write.csv(sample,"data samples/e.viminalis.csv")

