# assign library path
.libPaths("C:/Users/90946112/R/win-library/3.6.2")
library(tidyverse)
library(sf)

# load dataset
flora <- read.csv("data samples/all_minus_P-A_data.csv", header = TRUE)

# determine the number of observations in each cover estimate metric
options = data.frame(types = names(flora[,45:49]), obs = c(1, 2, 3, 4, 5))
a = 1
for (i in flora[,45:49]){
  b = i
  options[a, 2] = length(b[is.na(b) == FALSE])
  a = a+1
}
options

# select only the unique plot IDs

# 
# # convert to simple feature, with crs of GDA94 and the attributes being identifications
# map1 = st_as_sf(flora, coords = c("Longitude_GDA94", "Latitude_GDA94"), 
#          crs = 4283, agr = "identity")



