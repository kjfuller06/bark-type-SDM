# script for running LDA on species selection presence/absence records
library(tidyverse)
library(ggplot2)
library(RColorBrewer)

# load dataset
PA = read.csv("data/spp_selection_P-A.csv")

