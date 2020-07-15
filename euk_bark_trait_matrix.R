# Datasets for this script were created manually by using the LUCID eucalypt key at https://apps.lucidcentral.org/euclid/text/intro/index.html
#   Eucalypts were filtered for fire-relevant traits, species names were copied into Excel and the common names were removed. See README file for detailed description of dataset generation.

# Workflow:
#   1) Load datasets by bark trait
#   2) Join all datasets by species
#   3) Define fire-relevant bark categories using EUCLID bark categories
#   4) Assign fire-relevant bark categories to eucalypt species
#       4.1. Start by creating unique variables for each category to ensure there are no conflicts
#       4.2. Combine descriptions into one variable

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

# 3. ####
# All categories describing fibrous bark
fibrous_traits = c("fibrous")

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