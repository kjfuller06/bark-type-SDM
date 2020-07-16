# Datasets for this script were created manually by using the LUCID eucalypt key at https://apps.lucidcentral.org/euclid/text/intro/index.html
#   Eucalypts were filtered for fire-relevant traits, species names were copied into Excel and the common names were removed. See README file for detailed description of dataset generation.

# Workflow:
#   1) Load datasets by bark trait
#   2) Join all datasets by species
#   3) Define fire-relevant bark categories using EUCLID bark categories
#   4) Assign fire-relevant bark categories to eucalypt species
#       4.1. Start by creating unique variables for each category to ensure there are no conflicts
#       4.2. Combine descriptions into one variable
#       -> User will need to verify trait information using literature

# Assign library path
.libPaths("C:/Users/90946112/R/win-library/3.6.2")
library(tidyverse)
library(dplyr)

# 1. ####
# Branch bark traits
branches1 = read.csv("EUCLID_branches_minnirichi.csv")
branches2 = read.csv("EUCLID_branches_rough.csv")
branches3 = read.csv("EUCLID_branches_smooth.csv")
branches4 = read.csv("EUCLID_no_ribbons_in_branches.csv")
branches5 = read.csv("EUCLID_ribbons_in_branches.csv")

# Main stem bark traits
main1 = read.csv("EUCLID_fibrous.csv")
main2 = read.csv("EUCLID_minnirichi.csv")
main3 = read.csv("EUCLID_partly_rough.csv")
main4 = read.csv("EUCLID_wholly_rough.csv")
main5 = read.csv("EUCLID_wholly_smooth.csv")

# Traits describing just the rough or smooth bark that is present in any of the main stem categories
mainrough1 = read.csv("EUCLID_rough_bark_basal_slabs.csv")
mainrough2 = read.csv("EUCLID_rough_bark_box.csv")
mainrough3 = read.csv("EUCLID_rough_bark_compacted.csv")
mainrough4 = read.csv("EUCLID_rough_bark_imperfectly_shed.csv")
mainrough5 = read.csv("EUCLID_rough_bark_ironbark.csv")
mainrough6 = read.csv("EUCLID_rough_bark_tessellated.csv")
mainsmooth = read.csv("EUCLID_smooth_categories.csv")

# 2. ####
all_traits = full_join(branches1, branches2)
all_traits = full_join(all_traits, branches3)
all_traits = full_join(all_traits, branches4)
all_traits = full_join(all_traits, branches5)
all_traits = full_join(all_traits, main1)
all_traits = full_join(all_traits, main2)
all_traits = full_join(all_traits, main3)
all_traits = full_join(all_traits, main4)
all_traits = full_join(all_traits, main5)
all_traits = full_join(all_traits, mainrough1)
all_traits = full_join(all_traits, mainrough2)
all_traits = full_join(all_traits, mainrough3)
all_traits = full_join(all_traits, mainrough4)
all_traits = full_join(all_traits, mainrough5)
all_traits = full_join(all_traits, mainrough6)
all_traits = full_join(all_traits, mainsmooth)
summary(all_traits)

# write.csv(all_traits, file = "Eucalypt_all_bark_traits.csv")

# 3. ####
# Fibrous traits are defined by the "fibrous" category

# All categories describing non-fibrous bark
any_smooth = c("wholly.smooth",
               "main.minnirichi",
               "partly.rough",
               "rough.bark.loose.basal.slabs",
               "rough.bark.imperfectly.shed",
               "branches.smooth",
               "branches.minnirichi",
               "ribbons.in.branches",
               "partly.or.all.smooth")

# All categories describing shedding bark
shedding_traits = c("ribbons.in.branches",
                    "rough.bark.imperfectly.shed",
                    "rough.bark.loose.basal.slabs",
                    "branches.smooth",
                    "wholly.smooth",
                    "partly.or.all.smooth")

# All categories describing bark shed from branches
branch_shedding = c("ribbons.in.branches",
                    "branches.smooth")

# All categories describing bark shed from trunk
trunk_shedding = c("rough.bark.imperfectly.shed",
                    "rough.bark.loose.basal.slabs",
                    "wholly.smooth",
                    "partly.or.all.smooth")

# All categories describing non-shedding bark
any_rough = c("wholly.rough",
              "fibrous",
              "rough.bark.box.type",
              "rough.bark.compacted",
              "rough.bark.ironbark",
              "rough.bark.tessellated",
              "partly.rough",
              "branches.rough",
              "no.ribbons.in.branches")

# All categories describing non-shedding trunks
trunk_rough = c("wholly.rough",
                "fibrous",
                "rough.bark.box.type",
                "rough.bark.compacted",
                "rough.bark.ironbark",
                "rough.bark.tessellated",
                "partly.rough")

# All categories describing non-shedding bark in branches
branch_nonshedding = c("branches.rough",
              "no.ribbons.in.branches")

# 4. ####
# Identify completely fibrous-barked species
fibre_df = all_traits %>% 
  filter(fibrous == "yes") %>% 
  filter_at(vars(all_of(any_smooth)), all_vars(is.na(.))) %>% 
  mutate(bark_type_fibrous = "wholly fibrous")

# Identify partly fibrous/partly not fibrous species
fibre_df2 = all_traits %>% 
  filter(fibrous == "yes") %>% 
  filter_at(vars(all_of(any_smooth)), any_vars(!is.na(.))) %>% 
  mutate(bark_type_partly_fibrous = "partly fibrous")

# Identify bark-shedding species
# -> Includes all partly-shedding bark species as well
shed_df = all_traits %>% 
  filter_at(vars(all_of(shedding_traits)), any_vars(. == "yes")) %>% 
  mutate(bark_type_any_shedding = "shedding")

# Identify species that shed bark from trunk only
shed_df2 = all_traits %>% 
  filter_at(vars(all_of(shedding_traits)), any_vars(. == "yes")) %>%
  filter_at(vars(all_of(branch_shedding)), all_vars(is.na(.))) %>% 
  mutate(trunk_only_shedding = "bark shed from trunk only")

# Identify species with shedding in branches only
shed_df3 = all_traits %>% 
  filter_at(vars(all_of(branch_shedding)), any_vars(. == "yes")) %>%
  filter_at(vars(all_of(trunk_shedding)), all_vars(is.na(.))) %>% 
  mutate(branches_only_shedding = "bark shed from branches only")

# Identify species with ribbons confirmed- ribbons in branches (this could be augmented with webscraping text from EUCLID and common names from BioNet and EUCLID)
ribbons = all_traits %>% 
  filter(ribbons.in.branches == "yes") %>% 
  mutate(ribbons_in_branches = "ribbons in branches")

# Identify species with both fibrous and shedding qualities
mixed_bark1 = all_traits %>% 
  filter_at(vars(all_of(shedding_traits)), any_vars(. == "yes")) %>% 
  filter(fibrous == "yes") %>% 
  mutate(mixed_bark_shedding = "both fibrous and shedding")

# Identify pecies with both fibrous and ribbon-forming bark
mixed_bark2 = all_traits %>% 
  filter(ribbons.in.branches == "yes") %>% 
  filter(fibrous == "yes") %>% 
  mutate(mixed_bark_ribbons = "both fibrous and ribbony")

# Join data frames and assess traits
all_categories = full_join(fibre_df, fibre_df2)
all_categories = full_join(all_categories, shed_df)
all_categories = full_join(all_categories, shed_df2)
all_categories = full_join(all_categories, shed_df3)
all_categories = full_join(all_categories, ribbons)
all_categories = full_join(all_categories, mixed_bark1)
all_categories = full_join(all_categories, mixed_bark2)
all_categories = as.data.frame(unclass(all_categories))

bark_categories = all_categories %>% 
  dplyr::select("species",
                "bark_type_fibrous",
                "bark_type_partly_fibrous",
                "bark_type_any_shedding",
                "trunk_only_shedding",
                "branches_only_shedding",
                "ribbons_in_branches",
                "mixed_bark_shedding",
                "mixed_bark_ribbons")

# write.csv(bark_categories, "EUCLID_bark_categories_assigned.csv")



