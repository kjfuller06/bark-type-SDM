# Assign library path
.libPaths("C:/Users/90946112/R/win-library/3.6.2")
library(tidyverse)
library(dplyr)

# Load dataset
euks <- read.delim("EUCLID_FriPy_webscrape2.0.txt", header = FALSE, sep = ".", col.names = "eucalypts", strip.white = TRUE, colClasses = "character", na.strings=c("", "'", "NA", " "))

# Remove incomplete final row and NAs
euks = na.omit(euks)

# Concatenate rows describing the same species. Some were split because they contained ".", which was used as the separator in read.delim(). The nested if statements are there to scoop up sequential rows of split strings
a = c()
b = 1
for(i in c(2:nrow(euks))){
  if(startsWith(euks[i,], "'E") == FALSE){
    if(startsWith(euks[i-1,], "'E") == FALSE){
      if(startsWith(euks[i-2,], "'E") == FALSE){
        if(startsWith(euks[i-3,], "'E") == FALSE){
          euks[i-4,] = paste(euks[i-4,], euks[i-3,], euks[i-2,], euks[i-1,], euks[i,])
          a[b] = i
        } else {
          euks[i-3,] = paste(euks[i-3,], euks[i-2,], euks[i-1,], euks[i,])
        }
      } else {
        euks[i-2,] = paste(euks[i-2,], euks[i-1,], euks[i,])
      }
    } else {
      euks[i-1,] = paste(euks[i-1,], euks[i,])
    }
    a[b] = i
    b = b + 1
  }
}

euks = euks[-a,, drop = FALSE]

backup = euks

euks2 = data.frame(species = 1:nrow(euks), bark = 1:nrow(euks))
euks2[,1] = sapply(strsplit(euks$eucalypts, split=",b", fixed=TRUE), `[`, 1)
euks2[,2] = sapply(strsplit(euks$eucalypts, split=",b", fixed=TRUE), `[`, 2)

# euks2$barksplit = sapply(strsplit(euks2[,2], split="<a", fixed=TRUE), `[`)

