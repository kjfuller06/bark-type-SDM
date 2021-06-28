# This script is for selecting species for analysis from all cleaned BioNet species records.
library(tidyverse)
library(rnaturalearth)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(raster)
library(spData)

#   1. Load datasets
#   2. Keep only records from target species and reassign any growth forms to NA that are not either "tree" or "mallee"
#   3. Remove occurrences of species which exhibit different bark types in different growth forms and for which there is no record of growth form for observations.
#   4. Create trait dataframes that exclude 1) mallee-specific traits and then 2) tree-specific traits
#   5. Join tree bark traits to non-mallee occurrence records
#     -> correction needed where the species is categorized as a mallee in my database but is in the tree dataset because it was not observed as a mallee in BioNet
#   6. Join mallee traits to mallee occurrence records and combine datasets
#     -> correction needed where the species is categorized as a tree in my database but is in the mallee dataset because it was not observed as a tree in BioNet
#   7. Clip records using the boundary of NSW. Even though the data were already cleaned using CoordinateCleaner to remove points occurring in the ocean, this is still necessary because of stray points outside the state.
#   8. Write to file

# 1. ####
flora = read.csv("outputs/BioNet_allfloralsurvey_cleaned2.csv")
sample = read.csv("data/Candidate_speciesV.1.csv")
names(sample) = c("ScientificName", "Assgn_ScientificName", "NicolleName")
traits = read.csv("data/Allspecies_traitsV.1.csv")
names(traits)[1] = "NicolleName"

# 2. ####
flora = flora %>% 
  left_join(sample) %>% 
  drop_na(NicolleName) %>% 
  unique()
flora$GrowthForm[flora$GrowthForm != "Mallee" & flora$GrowthForm != "Tree" & flora$GrowthForm != ""] = ""

# 3. ####
switchers = c("Eucalyptus bakeri", "Eucalyptus elata", "Eucalyptus glaucescens", "Eucalyptus goniocalyx", "Eucalyptus morrisii", "Eucalyptus porosa", "Eucalyptus smithii", "Eucalyptus socialis", "Eucalyptus triflora")
flora2 = flora %>% 
  filter(!NicolleName %in% switchers)
switchers = flora %>% 
  filter(NicolleName %in% switchers) %>% 
  filter(GrowthForm == "Tree" | GrowthForm == "Mallee")
flora = full_join(flora2, switchers)

# 4. ####
# select traits that exclude:
#   -"mallee"- the mallee form of a species that can be either tree or mallee
#   -"yes"- mallee-only species
trees = traits %>% 
  filter(mallee != "mallee" & mallee != "yes")
# select traits that exclude:
#   -"tree"- the tree form of a species that can be either tree or mallee
#   -"no"- tree-only species
mallees = traits %>% 
  filter(mallee != "tree" & mallee != "no")

# 5. ####
flora2 = flora %>%
  filter(GrowthForm != "Mallee") %>% 
  full_join(trees)
flora2 = flora2 %>% 
  filter(!is.na(ID))
correction = flora2[is.na(flora2$mallee) == TRUE,] %>% 
  dplyr::select(-Horseybark1_final,
                -Horseybark2_final,
                -forming.ribbons_final,
                -mallee)
correction = inner_join(correction, mallees)
flora2 = rbind(flora2[is.na(flora2$mallee) == FALSE,], correction)

# 6. ####
flora3 = flora %>% 
  filter(GrowthForm == "Mallee") %>% 
  left_join(mallees)
correction = flora3[is.na(flora3$mallee) == TRUE,] %>% 
  dplyr::select(-Horseybark1_final,
                -Horseybark2_final,
                -forming.ribbons_final,
                -mallee)
correction = inner_join(correction, trees)
flora3 = rbind(flora3[is.na(flora3$mallee) == FALSE,], correction)
flora = rbind(flora2, flora3) %>% 
  unique()
## 21,786 records

# 7. ####
# get Australia layer
aus = getData(name = "GADM", country = "AUS", level = 1, download = TRUE) %>% 
  st_as_sf()

# create NSW layer with ACT included
nsw = aus %>% 
  filter(NAME_1 == "New South Wales" | NAME_1 == "Australian Capital Territory") %>% 
  dplyr::select(geometry)

# create polygon with an extent that hugs the NSW coastline so we can snip off stray islands
# points are introduced in sequence as they would be drawn on paper, with the last coordinates repeated. So a square will have 5 points.
bound = list(c( 154, -38), c(140, -38), c( 140, -28), c( 154, -28), c( 154, -38)) %>%
  unlist() %>%
  matrix(ncol = 2,
         byrow = TRUE) %>% 
  # first convert to a linestring
  st_linestring %>% 
  # then convert to a polygon
  st_cast('POLYGON') %>% 
  st_sfc(crs = 4326)

# now clip the nsw polygon using st_intersection
bound = st_intersection(nsw, bound)

# lastly, clip flora records by nsw boundary- minus islands
flora2 = st_as_sf(flora, coords = c("Longitude_GDA94", "Latitude_GDA94"), crs = 4326)
flora2 = flora2[bound, ]
# 21,557 records

# 8. ####
st_write(flora2, "outputs/species_sampleV.1.shp", delete_layer = TRUE)
# st_write(bound, "outputs/NSW_sans_islands.shp", delete_layer = TRUE)
# st_write(aus, "outputs/australia.shp", delete_layer = TRUE)