# Assign library path
.libPaths("C:/Users/90946112/R/win-library/3.6.2")
library(tidyverse)
library(dplyr)

# Load dataset
euks <- read.delim("EUCLID_FriPy_webscrape2.0.txt", header = FALSE, sep = ".", col.names = "eucalypts", strip.white = TRUE, colClasses = "character", na.strings=c("", "'", "NA", " "))

# Remove incomplete final row and NAs
euks = na.omit(euks)

backup = euks

# Concatenate rows describing the same species. Some were split because they contained ".", which was used as the separator in read.delim().
a = c()
b = 1
for(i in c(2:nrow(euks))){
  if(startsWith(euks[i,], "'E") == FALSE){
    euks[i-1,] = paste(euks[i-1,], euks[i,])
    a[b] = i
    b = b + 1
  }
}


