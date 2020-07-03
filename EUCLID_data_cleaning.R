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
    if(startsWith(euks[i-1,], "'E") == FALSE){
      if(startsWith(euks[i-2,], "'E") == FALSE){
        if(startsWith(euks[i-3,], "'E") == FALSE){
          euks[i-4,] = paste(euks[i-4,], euks[i-3,], euks[i-2,], euks[i-1,], euks[i,])
          a[b] = i
        }
        euks[i-3,] = paste(euks[i-3,], euks[i-2,], euks[i-1,], euks[i,])
        a[b] = i
      }
      euks[i-2,] = paste(euks[i-2,], euks[i-1,], euks[i,])
      a[b] = i
    }
    euks[i-1,] = paste(euks[i-1,], euks[i,])
    a[b] = i
    b = b + 1
  }
}

euks = euks[-a,, drop = FALSE]
