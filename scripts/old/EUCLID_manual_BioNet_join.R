# Assign library path
.libPaths("C:/Users/90946112/R/win-library/3.6.2")
library(tidyverse)
library(dplyr)

# Load EUCLID datasets
EUCLID1 = read.csv("EUCLID_all_bark_traits_concatenated.csv")
EUCLID2 = read.csv("EUCLID_bark_categories_assigned.csv")
# Join EUCLID data before joining with BioNet
EUCLID = full_join(EUCLID1, EUCLID2)
# write.csv(EUCLID, "EUCLID_EUCLID_join.csv")

# Load BioNet dataset
BioNetNames = read.csv("BioNet_FloraSurveys_SpeciesList_eucalypts.csv")
# Drop any duplicate names
BioNetNames = unique(BioNetNames)

# Join all data using both assigned and unassigned names
# Create data frame with the right dimensions
joined = BioNetNames
joined$EUCLID_assigned = NA
joined$EUCLID_unassigned = NA
joined$EUCLID_partial_assigned = NA
joined$EUCLID_partial_unassigned = NA
joined$EUCLID_no_match = NA

no_match = data.frame(Scientific.name = c(1:934), 
                      Assign.Scientific.name = NA, 
                      EUCLID_assigned = NA,
                      EUCLID_unassigned = NA,
                      EUCLID_partial_assigned = NA,
                      EUCLID_partial_unassigned = NA,
                      EUCLID_no_match = NA)
no_match$Scientific.name = NA
# for(i in c(1:10)){
#   joined[,(i+2)] = NA
#   names(joined)[(i+2)] = names(EUCLID)[i]
# }


# Search BioNet data for EUCLID names
#   -> Search correct scientific names first. If these do not match, search incorrect scientific names
e = 1
for(i in c(1:nrow(EUCLID))){
  a = grep(paste("^", EUCLID$species[i], "$", sep = ""), joined$Assign.Scientific.name, perl = TRUE)
  b = grep(paste("^", paste(EUCLID$species[i]), "$", sep = ""), joined$Scientific.name, perl = TRUE)
  c = grep(paste(EUCLID$species[i]), joined$Assign.Scientific.name, fixed = TRUE)
  d = grep(paste(EUCLID$species[i]), joined$Scientific.name, fixed = TRUE)
  
  if(length(grep(paste("^", EUCLID$species[i], "$", sep = ""), joined$Assign.Scientific.name, perl = TRUE)) != 0){
    joined[a, 3] = paste(as.character(EUCLID[i, 1]))
    # for(x in c(1:10)){
    #   joined[a, (x+2)] = as.character(EUCLID[i, (x)])
    #   }
    }
  if(length(grep(paste("^", paste(EUCLID$species[i]), "$", sep = ""), joined$Scientific.name, perl = TRUE)) != 0){
    joined[b, 4] = paste(as.character(EUCLID[i, 1]))
    # for(x in c(1:10)){
    #   joined[a, (x+2)] = as.character(EUCLID[i, (x)])
    #   }
    }
  if(length(grep(paste(EUCLID$species[i]), joined$Assign.Scientific.name, fixed = TRUE)) != 0){
    joined[c, 5] = paste(as.character(EUCLID[i, 1]))
    # for(x in c(2:10)){
    #   joined[a, (x+2)] = as.character(EUCLID[i, (x)])
    #   }
    }
  if(length(grep(paste(EUCLID$species[i]), joined$Scientific.name, fixed = TRUE)) != 0){
    joined[d, 6] = paste(as.character(EUCLID[i, 1]))
    # for(x in c(2:10)){
    #   joined[a, (x+2)] = as.character(EUCLID[i, (x)])
    #   }
  }
  if(length(a) + length(b) + length(c) + length(d) == 0){
    no_match$EUCLID_no_match[e] = paste(as.character(EUCLID[i, 1]))
    e = e + 1
  }
}
no_match = no_match[complete.cases(no_match[,7]),]
joined = rbind(joined, no_match)

names(joined) = c("BioNet_unassigned", "BioNet_assigned", #"EUCLID", "bark_matches_concat", "main_trunk_fibrous", "main_trunk_partly_fibrous", "shedding_any", "shedding_trunk_only", "shedding_branches_only", "ribbons_present_in_branches", "fibrous_and_shedding", "fibrous_and_ribbony", 
                  "EUCLID_assigned", "EUCLID_unassigned", "EUCLID_partial_assigned", "EUCLID_partial_unassigned", "EUCLID_no_match")
# write.csv(joined, "EUCLID_manual_BioNet_join.csv")

# Create a data frame listing all EUCLID names and bark attributes and which BioNet names they matched, if any
# not_joined = EUCLID %>% 
#   select(species)
# not_joined$matched_assigned = NA
# not_joined$matched_unassigned = NA
# not_joined$partial_matched_assigned = NA
# not_joined$partial_matched_unassigned = NA

# for(i in c(1:nrow(EUCLID))){
#   if(length(grep(EUCLID$species[i],BioNetNames$Assign.Scientific.name, fixed = TRUE)) != 0){
#     not_joined$matched_assigned[i] = "TRUE"
#   } else{
#     if(length(grep(EUCLID$species[i],joined$Scientific.name, fixed = TRUE)) != 0){
#       not_joined$matched_unassigned[i] = "TRUE"
#     }
#   }
# }
# BioNetNames_noreplacement = BioNetNames
# 
# for(i in c(1:nrow(EUCLID))){
#   if(length(grep(paste("^", EUCLID$species[i], "$", sep = ""), BioNetNames$Assign.Scientific.name, perl = TRUE)) != 0){
#     not_joined$matched_assigned[i] = paste(BioNetNames$Assign.Scientific.name[grep(paste("^", EUCLID$species[i], "$", sep = ""), BioNetNames$Assign.Scientific.name, perl = TRUE)])
#   } else {
#     if(length(grep(paste("^", paste(EUCLID$species[i]), "$", sep = ""), BioNetNames$Scientific.name, perl = TRUE)) != 0){
#       not_joined$matched_unassigned[i] = "TRUE"
#     } else {
#       if(length(grep(EUCLID$species[i], BioNetNames$Assign.Scientific.name, fixed = TRUE)) != 0){
#         not_joined$partial_matched_assigned[i] = "TRUE"
#       } else {
#         if(length(grep(EUCLID$species[i], BioNetNames$Scientific.name, fixed = TRUE)) != 0){
#           not_joined$partial_matched_assigned[i] = "TRUE"
#         }
#       }
#     }
#   } 
# }

# write.csv(not_joined, "EUCLID_manual_BioNet_name_check.csv")





