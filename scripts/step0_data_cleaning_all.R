## This file is for selecting useful columns and performing some common cleaning processes that could be useful for other scientists at the HIE and outside as well. The workflow is as follows:
#   1. Load dataset
#   2. Drop all columns not relevant to (most) species distribution modeling
#   3. Remove all instances except those in which Accuracy is less than 1km (1000m according to native units)
#   4. Remove all instances except those in which the difference between start and end dates is less than 7 days
#   5. Remove all instances except those listed as "accepted"
#   6. Remove all instances except those in which percent cover is greater than 0%.
#   7. Generate a unique code for every distinct combination of Year + Lat/Lon. BioNet data contain multiple instances in which replicates and/or subplots were surveyed in a given location, listing exactly the same coordinates on the same day.
#       -> The user will need to decide how to address apparent duplicates in a given location.
#   8. Cleaned remaining data using the CoordinateCleaner package for:
#       a. records occurring within 10,000m of the coordinates of a capital city
#       b. records occurring within 1,000m of the centroid of a province or a county polygon,
#       c. records in which the coordinates have equal numerical value, often an indication of user error in data entry
#       d. records at GBIF headquarters, GBIF being a global biodiversity database
#       e. records occurring within 100m of the coordinates of a known biological records institution,
#       f. records occurring in the ocean, according to rnaturalearth data at the finest resolution,
#       g. records with either lat/lon coordinate listed as "zero"
#   9. write resulting dataframe to disk

# assign library path
library(tidyverse)
library(CoordinateCleaner)
library(rnaturalearth)

# 1. & 2. ####
flora <- read.delim("data/BioNet_allflorasurvey.txt", header = TRUE, sep = "\t", dec = ".") %>%
  dplyr::select(Assgn_ScientificName,
                Exotic,
                NSWStatus,
                CommStatus,
                SensitivityClass,
                DateFirst,
                DateLast,
                NumberIndividuals,
                EstimateTypeCode,
                SourceCode,
                ObservationType,
                Status,
                Latitude_GDA94,
                Longitude_GDA94,
                Accuracy,
                Stratum,
                GrowthForm,
                CoverScore,
                AbundanceScore,
                PercentCover,
                LowerHeight,
                UpperHeight)

# list columns that will be converted to factor variables
columns=c("Assgn_ScientificName","Exotic","NSWStatus","CommStatus","SensitivityClass","EstimateTypeCode","SourceCode","ObservationType","Status","Stratum","GrowthForm","CoverScore","AbundanceScore")
# convert columns to factors
flora[columns] = lapply(flora[columns], factor)

# convert columns to date variables
timefunction <- function(x) as.Date(x, format="%d/%m/%Y")
flora[c("DateFirst","DateLast")] = lapply(flora[c("DateFirst", "DateLast")], timefunction)

# 3. ####
flora = flora %>% 
  filter(Accuracy < 1000)

# 4. ####
flora = flora %>% 
  filter(DateLast - DateFirst < 8)

# 5. ####
flora = flora %>% 
  filter(grepl("accepted", Status, ignore.case = TRUE))

# 6. ####
flora = flora %>% 
  filter(PercentCover > 0)

# 7. ####
unique = unique(flora[c("DateFirst", "DateLast", "Latitude_GDA94", "Longitude_GDA94")])
unique$ID = seq_len(nrow(unique))
flora = left_join(flora, unique)

# 8. ####
# May also want to clean using the outliers test. Not implemented here.
backup = flora
backup$ISO = "AUS"
backup = clean_coordinates(backup, 
                           lon = "Longitude_GDA94",
                           lat = "Latitude_GDA94",
                           species = "Assgn_ScientificName",
                           countries = "ISO",
                           country_ref = rnaturalearth:ne_countries(scale = 10),
                           seas_ref = rnaturalearth::ne_download(scale = 10, 
                                                                 type = 'land', 
                                                                 category = 'physical'),
                           seas_scale = 10,
                           tests = c("capitals", 
                                     "centroids", 
                                     "equal", 
                                     "gbif", 
                                     "institutions",
                                     "seas", 
                                     "zeros"),
                           verbose = TRUE)

# remove failed observations and test columns
flora = backup %>% 
  filter(.cap == TRUE & .sea == TRUE & .summary == TRUE) %>% 
  dplyr::select(ID,
                Assgn_ScientificName,
                Exotic,
                NSWStatus,
                CommStatus,
                SensitivityClass,
                DateFirst,
                DateLast,
                NumberIndividuals,
                EstimateTypeCode,
                SourceCode,
                ObservationType,
                Status,
                Latitude_GDA94,
                Longitude_GDA94,
                Accuracy,
                Stratum,
                GrowthForm,
                CoverScore,
                AbundanceScore,
                PercentCover,
                LowerHeight,
                UpperHeight)

# 9. ####
write.csv(flora, file = "outputs/BioNet_allflorasurvey_cleaned.csv")

