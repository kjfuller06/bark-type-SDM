# script for running LDA on species selection presence/absence records
library(tidyverse)
library(Rlda)

# load datasets
PA = read.csv("data/spp_selection_P-A.csv")
PA_coords = read.csv("data/spp_selection_P-A_coordinates.csv")

# remove two outlier species from consideration
PA = PA %>% 
  dplyr::select(-c(E.largiflorens, E.populneasubsp.bimbil))
# ^ this leads to locations with no species present. Need to remove these.

# set the starting number for random number generation to ensure consistent results
set.seed(9842)

# cluster species and generate group stats for locations
groups_n = 20
LDA1 = rlda.bernoulli(PA, groups_n, 0.5, 0.5, 0.1, 300, ll_prior = TRUE, display_progress = TRUE)

# Phi represents the probability of species belonging to each group (200spp means there are 200 Phi values per group)
Phis = getPhi.rlda(LDA1)
# Theta represents the proportion of species at each location that are members of different groups (20 groups means there are 20 Theta values per location)
Thetas = getTheta.rlda(LDA1)
# Apend coordinates of locations to Thetas df
Thetas = cbind(Thetas, PA_coords)

# Now, according to Valle et al. 2018, I should remove the groups with thetas <0.5 in 99% of locations
superfluous = c()
cutoff = nrow(Thetas)*0.01
q = 1
for(i in c(1:groups_n)){
  if(sum(Thetas[,i] >= 0.5) <= cutoff){
    superfluous[q] = colnames(Thetas)[i]
    q = q+1
  }
}

# remove clusters using the above criteria
Phis[row.names(Phis) %in% superfluous,] = NA
Thetas = Thetas[,!(colnames(Thetas) %in% superfluous)]

# write df's to disk
write.csv(Phis, "outputs/HorseyV.2_LDAV.2_PhisV.2.csv")
write.csv(Thetas, "outputs/HorseyV.2_LDAV.2_ThetasV.2.csv")
