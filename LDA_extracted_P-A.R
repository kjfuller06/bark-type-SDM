# script for running LDA on species selection presence/absence records
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(Rlda)

# load dataset
PA = read.csv("data/spp_selection_P-A.csv")

# set the starting number for random number generation to ensure consistent results
set.seed(9842)

# cluster species and generate group stats for locations
LDA1 = rlda.bernoulli(PA, 5, 0.5, 0.5, 0.1, 200, ll_prior = TRUE, display_progress = TRUE)

# Phi represents the probability of species belonging to each group (200spp means there are 200 Phi values per group)
Phis = getPhi.rlda(LDA1)
# Theta represents the proportion of species at each location that are members of different groups (20 groups means there are 20 Theta values per location)
Thetas = getTheta.rlda(LDA1)

# Now, according to Valle et al. 2018, I should remove the groups with thetas <0.5 in 99% of locations
cutoff = nrow(Thetas)*0.01
for(i in c(1:ncol(Thetas))){
  if(sum(Thetas[,i] >= 0.01) <= cutoff){
    print(colnames(Thetas)[i])
  }
}

# ^ this returned one cluster as superfluous. I guess I throw it out? Do I run the LDA again?
# set the starting number for random number generation to ensure consistent results
set.seed(9842)

# cluster species and generate group stats for locations
LDA1 = rlda.bernoulli(PA, 4, 0.5, 0.5, 0.1, 200, ll_prior = TRUE, display_progress = TRUE)

# Phi represents the probability of species belonging to each group (200spp means there are 200 Phi values per group)
Phis = getPhi.rlda(LDA1)
# Theta represents the proportion of species at each location that are members of different groups (20 groups means there are 20 Theta values per location)
Thetas = getTheta.rlda(LDA1)

# Now, according to Valle et al. 2018, I should remove the groups with thetas <0.5 in 99% of locations
cutoff = nrow(Thetas)*0.01
for(i in c(1:ncol(Thetas))){
  if(sum(Thetas[,i] >= 0.01) <= cutoff){
    print(colnames(Thetas)[i])
  }
}

write.csv(Phis, "data/HorseyV.2_LDA_Phis_V.1.csv")
write.csv(Thetas, "data/HorseyV.2_LDA_Thetas_V.1.csv")

