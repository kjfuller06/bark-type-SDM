# script for testing whether the function getTheta.rlda() returns locations in the same order as that of the input df. 
## Confirmed.

library(tidyverse)
library(Rlda)

# load dataset
PA = read.csv("data/spp_selection_P-A.csv")
PA[,c(1:ncol(PA))] = 0
PA[c(1:30),c(1:10)] = 1
PA[c(31:60),c(11:20)] = 1
PA[c(61:93),c(21:31)] = 1

# set the starting number for random number generation to ensure consistent results
set.seed(9842)

# cluster species and generate group stats for locations
LDA1 = rlda.bernoulli(PA, 10, 0.5, 0.5, 0.1, 200, ll_prior = TRUE, display_progress = TRUE)

# Phi represents the probability of species belonging to each group (200spp means there are 200 Phi values per group)
Phis = getPhi.rlda(LDA1)
# Theta represents the proportion of species at each location that are members of different groups (20 groups means there are 20 Theta values per location)
Thetas = getTheta.rlda(LDA1)

# Now, according to Valle et al. 2018, I should remove the groups with thetas <0.5 in 99% of locations
cutoff = nrow(Thetas)*0.01
for(i in c(1:ncol(Thetas))){
  if(sum(Thetas[,i] >= 0.5) <= cutoff){
    print(colnames(Thetas)[i])
  }
}

# write df's to disk and check if locations are retained in order
write.csv(Phis, "data/HorseyV.2_LDA_Phis_testloc.csv")
write.csv(Thetas, "data/HorseyV.2_LDA_Thetas_testloc.csv")
write.csv(PA, "data/HorseyV.2_LDA_testloc.csv")
