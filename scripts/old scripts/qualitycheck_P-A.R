# Script is for checking suspicious data in a subset of BioNet flora survey data concerning cover and abundance estimates of Eucalypts species only. This subset has been coined "P-A" for Presence-Absence, meaning these surveys can be used to generate presence/absence data for species distribution modeling. Workflow is as follows:
#   1. Load dataset
#   2. Determine the number of observations (i.e. non-NA entries) for the various cover and abundance measurements
#   3. Check that start and end dates always match -> they do not
#   4. Examine the dates from 1970- many of these have end dates in the 2000s
#   5. Examine plot IDs to determine what they actually identify and how many unique values there are for each
#     -> 5.1. Look at Stratum to see if any existing ID's map to the ID I want
##        -> No ID maps to unique combinations of dates and lat/lon coordinates
#     -> 5.2. Created unique ID for each combination of dates + lat/lon coordinates + SiteNo, then examine other combinations of variables
##        -> CONCLUSION: native IDs should not be used for this analysis. Unique IDs for date and location info should be used instead. Duplicates should be randomly selected and the rest thrown out. The problem is that I still need to determine what a single survey is so I can select between options.
#     -> 5.3. Created unique ID for each combination of dates + lat/lon coordinates, then examine the duplicates
#   6. Determine if the scientific name columns match

# assign library path
.libPaths("C:/Users/90946112/R/win-library/3.6.2")
library(tidyverse)
library(sf)
library(raster)
library(tmap)
library(vctrs)
library(plyr)

# 1. ####
flora <- read.csv("data samples/all_minus_P-A_data.csv", header = TRUE)

# 2. ####
options = data.frame(types = names(flora[,c(21,45:49)]), obs = c(1, 2, 3, 4, 5, 6))
a = 1
for (i in flora[,c(21,45:49)]){
  b = i
  options[a, 2] = length(b[is.na(b) == FALSE])
  a = a+1
}
options

# 3. ####
# convert Date First to three columns in a data frame
datecheck = as.data.frame(stringr::str_split_fixed(as.character(flora$DateFirst), pattern = "/", n = 3))
length(datecheck$V3[datecheck$V3 == 1970])
## 126 records for 1970
# check for 126 records of the date 01/01/1970
flora2 = flora
flora2$DateFirst <- as.Date(flora2$DateFirst, format = "%d/%m/%Y")
length(flora2$DateFirst[flora2$DateFirst == "1970-01-01"])
## confirmed
flora2$DateLast <- as.Date(flora2$DateLast, format = "%d/%m/%Y")
length(flora2$DateLast[flora2$DateLast == "1970-01-01"])
## 0 records of 1970-01-01

# 4. ####
# This could also be an error resulting from date format conversion. Let's look at the original df
datecheck = flora %>% 
  filter(DateFirst == "01/01/1970") %>% 
  droplevels() %>% 
  st_as_sf(coords = c("Longitude_GDA94", "Latitude_GDA94"), 
           crs = 4283, agr = "identity")
## Not a problem for the original data. 
## All records with a start date of 1970-01-01 have an end date of either 30/06/2011 (2378 records) or 31/01/2000 (9 records)

# Are these return plots or is the date of the survey unknown?
# dplyr::filter(ccodes(), NAME %in% "Australia")
# aus = getData("GADM", country = "AUS", level = 1) %>% 
#   st_as_sf(aus) %>% 
#   filter(NAME_1 == "New South Wales")
# create interactive map of data points from 01-01-1970
# tmap_mode("view")
# qtm(aus) + qtm(datecheck)
## ok, the plots are not clustered

# 5. ####
# What I need for an analysis is a unique identifier for each unique combination of date, location, site, replicate(?) and subplot
sumflora = data.frame(types = c("total", "datasetname", "sightingkey", "date1", "date2", "locationkey", "lat", "lon", "surveyname", "censuskey", "siteno", "replicateno", "subplotid"), obs = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
sumflora[1, 2] = nrow(flora)
a = 2
for (i in c(2, 3, 19, 20, 27, 29, 30, 37, 38, 40:42)){
  sumflora[a, 2] = length(unique(flora[,i]))
  a = a+1
}
sumflora
## unique(sightingkey) is the same length as the whole dataset so that's useless.
## LocationKey is longer than lat/lon. LocationKey is also longer than than the dates
## locationkey is almost = siteno. 
# Check the coordinates. Also check the time indices for there being different time points (could be tied to SiteNo duplicates)
sites = flora %>% 
  dplyr::select(LocationKey, SiteNo)
sites = unique(sites)
a = duplicated(sites$LocationKey)
dup = sites[a,]
dup
b = flora[flora$LocationKey %in% dup$LocationKey,] %>% 
  dplyr::select(LocationKey, SiteNo, Latitude_GDA94, Longitude_GDA94, DateFirst, DateLast)
b
## *SiteNo changes with the date* but lat/lon and LocationKey don't always.

# 5.1. & 5.2
# Extract just the ID variables and Stratum. Look at the length of unique values of each variable for just the Stratum-relevant records
# SightingKey thrown out because it's the same length as flora
# LocationKey also out because it doesn't always change with date and I want discrete measurements
stratdf = flora[flora$Stratum != "-",]
strata = data.frame(types = c("stratum", "datasetname", "date1", "date2", "lat", "lon", "surveyname", "censuskey", "siteno", "replicateno", "subplotid", "test_dp", "test_dps", "test_dpss", "test_all_locIDs"), obs = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
strata[1, 2] = nrow(stratdf)
a = 2
for (i in c(2, 19, 20, 29, 30, 37, 38, 40:42)){
  strata[a, 2] = length(unique(stratdf[,i]))
  a = a+1
}
strata

# Look at combinations of location IDs. What do they mean?
strata[12, 2] = nrow(unique(stratdf[c("DateFirst", "DateLast", "Latitude_GDA94", "Longitude_GDA94")]))
## 14,691
strata[13, 2] = nrow(unique(stratdf[c("DateFirst", "DateLast", "Latitude_GDA94", "Longitude_GDA94", "SiteNo")]))
## 17,178
strata[14, 2] = nrow(unique(stratdf[c("DateFirst", "DateLast", "Latitude_GDA94", "Longitude_GDA94", "SiteNo", "ReplicateNo", "SubplotID", "Stratum")]))
## 19,477
strata[15, 2] = nrow(unique(stratdf[c("ï..DatasetName", "DateFirst", "DateLast", "LocationKey", "Latitude_GDA94", "Longitude_GDA94", "SurveyName", "CensusKey", "SiteNo", "ReplicateNo", "SubplotID", "Stratum")]))
## exactly the same
strata
## I've gotten quite close here. Combination 14 seems to be the smallest denomination outside of actual sighting records

# Add unique values from 13 to CensusKey and look at duplicates
# Create a unique key for the following variable combinations
unique_dps = unique(stratdf[c("DateFirst", "DateLast", "Latitude_GDA94", "Longitude_GDA94", "SiteNo")])
unique_dps$ID = seq_len(nrow(unique_dps))
# select only unique_dps and censuskey and identify duplicates of unique_dps in the new df. Then just look at these.
census_dup = left_join(stratdf, unique_dps) %>% 
  dplyr::select("CensusKey", "ID") %>% 
  unique()
census_dup$ID = as.factor(census_dup$ID)
b = count(census_dup$ID) %>% 
  top_n(2)
census_dup = census_dup %>%
  filter(ID == b$x[1] | ID == b$x[2])
census_dup2 = left_join(census_dup, stratdf)
census_dup2_stats = data.frame(types = c("total", names(census_dup2)), obs = seq(1:52))
census_dup2_stats[1, 2] = nrow(census_dup2)
a = 2
for (i in c(1:51)){
  census_dup2_stats[a, 2] = length(unique(census_dup2[,i]))
  a = a+1
}
census_dup2_stats
# write.csv(census_dup2, file = "data samples/CensusKeyduplicates.csv")
## Not a single variable besides EntryOrder has the same number of unique values as CensusKey. And anyway, EntryOrder still doesn't match up with CensusKey.
## Looks like the duplicates are the result of replicates and subplots

# 5.3.
# Add unique values to flora and examine possible plot info- SiteNo, ReplicateID, SubPlotID and Stratum
# Create a unique key for the following variable combinations
unique_dp = unique(stratdf[c("DateFirst", "DateLast", "Latitude_GDA94", "Longitude_GDA94")])
unique_dp$ID = seq_len(nrow(unique_dp))
census_dup = left_join(stratdf, unique_dp) %>%
  dplyr::select("ID","ï..DatasetName", "DateFirst", "DateLast", "LocationKey", "Latitude_GDA94", "Longitude_GDA94", "SurveyName", "CensusKey", "SiteNo", "ReplicateNo", "SubplotID", "Stratum") %>%
  unique()
# Summarise by ID
census_dup_stats = census_dup %>% 
  dplyr::select("ID", "ï..DatasetName", "LocationKey", "SurveyName", "CensusKey", "SiteNo", "ReplicateNo", "SubplotID", "Stratum")
with(census_dup_stats[c(1:100),], ave(LocationKey, ID, FUN=function(x) length(unique(x))))
## GODDAMN FUCKING FAIL


# 6. ####
# Check scientific names- are there cases where ScientificName and Assgn_ScientificName do not match?
nms = flora %>% 
  dplyr::select(ScientificName, CommonName, Assgn_ScientificName, Assgn_CommonName)
nms2 = data.frame(types = c("tot", names(nms)), obs = c(1, 2, 3, 4, 5))
nms2[1, 2] = nrow(nms)
a = 2
for (i in c(1:4)){
  nms2[a, 2] = length(unique(nms[,i]))
  a = a+1
}
nms2
## slightly different numbers of different names. 

# Let's check out the mis-matches
# all(flora$ScientificName == flora$Assgn_ScientificName)
## returns an error because factor levels differ, which effectly gives the answer
mismatch = flora %>% 
  filter(as.character(ScientificName) != as.character(Assgn_ScientificName))
summary(mismatch)
## scrolled through enough to be confident that Assgn_ScientificName is the accepted/corrected version of whatever was entered in ScientificName

# 7. ####
# do some mapping with recently cleaned data
flora <- read.csv("data samples/Eucalyptus_presence.csv", header = TRUE)

# # convert to simple feature, with crs of GDA94 and the attributes being identifications
map1 = st_as_sf(flora, coords = c("Longitude_GDA94", "Latitude_GDA94"),
         crs = 4283, agr = "identity")
map1 = st_transform(map1, crs = 4326)
# dplyr::filter(ccodes(), NAME %in% "Australia")
aus = st_as_sf(getData("GADM", country = "AUS", level = 1))
nsw = aus %>% 
  filter(NAME_1 == "New South Wales")
# plot(st_geometry(nsw))
# plot(map1["ID"], add = TRUE)
# set tmap mode to interactive viewing
# tmap_mode("view")
# plot nz and nz_height
# qtm(map1, dots.col = "Assgn_ScientificName", max.categories = 216)




