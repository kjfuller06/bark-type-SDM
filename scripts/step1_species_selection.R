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
#   3. Create trait dataframes that exclude 1) mallee-specific traits and then 2) tree-specific traits
#   3. Join tree bark traits to non-mallee occurrence records
#   4. Join mallee traits to mallee occurrent records and combine datasets
#   5. Clip records using the boundary of NSW. Even though the data were already cleaned using CoordinateCleaner to remove points occurring in the ocean, this is still necessary because of stray points outside the state.

# 1. ####
flora = read.csv("outputs/BioNet_allfloralsurvey_cleaned2.csv")
sample = read.csv("data/Candidate_speciesV.1.csv")
traits = read.csv("data/Allspecies_traitsV.1.csv")

# 2. ####
flora = flora[flora$Assgn_ScientificName %in% sample$Bionet_assigned,] %>% 
  left_join(sample, by = c("Assgn_ScientificName" = "Bionet_assigned")) %>% 
  dplyr::select(-Assgn_ScientificName,
         -BioNet,
         -X) %>% 
  unique()
flora$GrowthForm[flora$GrowthForm != "Mallee" & flora$GrowthForm != "Tree" & flora$GrowthForm != ""] = ""

# 3. ####
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

# 4. ####
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

# 4. ####
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
## 21,927 records

# 5. ####
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

# 10. ####
st_write(flora2, "outputs/species_sampleV.1.shp", delete_layer = TRUE)
# st_write(bound, "outputs/NSW_sans_islands.shp", delete_layer = TRUE)
# st_write(aus, "outputs/australia.shp", delete_layer = TRUE)