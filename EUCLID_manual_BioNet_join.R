# Assign library path
.libPaths("C:/Users/90946112/R/win-library/3.6.2")
library(tidyverse)
library(dplyr)

# Load EUCLID datasets
EUCLID1 = read.csv("EUCLID_all_bark_traits_concatenated.csv")
EUCLID2 = read.csv("EUCLID_bark_categories_assigned.csv")
# Join EUCLID data before joining with BioNet
EUCLID = full_join(EUCLID1, EUCLID2)

# Load BioNet dataset
BioNetNames = read.csv("BioNet_FloraSurveys_SpeciesList_eucalypts.csv")
# Drop any duplicate names
BioNetNames = unique(BioNetNames)

# Join all data using both assigned and unassigned names
# Create data frame with the right dimensions
joined = BioNetNames
for(i in c(1:10)){
  joined[,(i+2)] = NA
  names(joined)[(i+2)] = names(EUCLID)[i]
}

# Search BioNet data for EUCLID names
#   -> Search correct scientific names first. If these do not match, search incorrect scientific names
for(i in c(1:nrow(EUCLID))){
  if(length(grep(EUCLID$species[i],joined$Assign.Scientific.name)) != 0){
    a = grep(EUCLID$species[i],joined$Assign.Scientific.name)
    for(b in c(1:10)){
      joined[a, (b+2)] = as.character(EUCLID[i, (b)])
    }
    } else{
      if(length(grep(EUCLID$species[i],joined$Scientific.name)) != 0){
        a = grep(EUCLID$species[i],joined$Scientific.name)
        for(b in c(1:10)){
          joined[a, (b+2)] = as.character(EUCLID[i, (b)])
        }
        }
    }
  }

# write.csv(joined, "EUCLID_manual_BioNet_join.csv")

# Create a data frame listing all EUCLID names and bark attributes and which BioNet names they matched, if any
not_joined = EUCLID %>% 
  select(species)
not_joined$matched_assigned = NA
not_joined$matched_unassigned = NA

for(i in c(1:nrow(EUCLID))){
  if(length(grep(EUCLID$species[i],BioNetNames$Assign.Scientific.name)) != 0){
    not_joined$matched_assigned[i] = "TRUE"
  } else{
    if(length(grep(EUCLID$species[i],joined$Scientific.name)) != 0){
      not_joined$matched_unassigned[i] = "TRUE"
    }
  }
}

# write.csv(not_joined, "EUCLID_manual_BioNet_name_check.csv")





