# Assign library path
.libPaths("C:/Users/90946112/R/win-library/3.6.2")
library(tidyverse)
library(dplyr)

# Load dataset
euks <- read.delim("EUCLID_FriPy_webscrape3.0.txt", header = FALSE, sep = "!", col.names = c("eucalypts", "bark"), strip.white = TRUE, colClasses = "character", na.strings=c("", "'", "NA", " "))

# Remove extra text at the beginning of euks$bark
euks2 = data.frame(lapply(euks, function(x) {gsub("^b' ", "", x)}))
# Remove ending punctuation at the end of euks$bark
euks2 = data.frame(lapply(euks2, function(x) {gsub("'$", "", x)}))

# delete all the extra junk
euks2$bark = gsub("<a class=fsf_tooltip href=glossary.htm#EUCLID Glossary_", "", euks2$bark, fixed = TRUE)
backup = euks2

euks2$bark = gsub("[[:punct:]]{2}a[[:punct:]]gt", "", euks2$bark, perl = TRUE)
euks2$bark = gsub(" title[[:punct:]].*?[[:punct:]]{2}a[[:punct:]]", "", euks2$bark, perl = TRUE)
euks2$bark = gsub("[[:punct:]]lt[[:punct:]]", "", euks2$bark, perl = TRUE)
euks2$bark = gsub("[[:punct:]]gt[[:punct:]]", "", euks2$bark, perl = TRUE)
euks2$bark = gsub("\\s{2}", " ", euks2$bark, perl = TRUE)
euks2$bark = gsub("[[:punct:]]xc[0-9][[:punct:]].*93", "-", euks2$bark, perl = TRUE)
euks2$bark = gsub("[[:punct:]]xc2[[:punct:]]xa0", "", euks2$bark, perl = TRUE)
euks2$bark = gsub("\\Sstrong\\S", "", euks2$bark, perl = TRUE)
euks2$bark = gsub("[[:punct:]]br[[:punct:]]{2}", "", euks2$bark, perl = TRUE)

# Note: grep("", euks2$bark) used for searching
## Bark data are missing for a number of euks. At least one has no "Bark" category in its info sheet on EUCLID. For the others, it's not clear why they are missing. Possibly it would be best to get a list of candidate species from BioNet to use as a quality check. (BioNet data will only include euks in NSW)

# write.csv(euks2, file = "textsplash.csv")
