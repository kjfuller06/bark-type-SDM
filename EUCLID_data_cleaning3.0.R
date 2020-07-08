# Assign library path
.libPaths("C:/Users/90946112/R/win-library/3.6.2")
library(tidyverse)
library(dplyr)

# Load dataset
euks <- read.delim("EUCLID_FriPy_webscrape3.0.txt", header = FALSE, sep = "!", col.names = c("eucalypts", "bark"), strip.white = TRUE, colClasses = "character", na.strings=c("", "'", "NA", " "))

# Remove extra text at the beginning of euks$bark
euks2 = data.frame(lapply(euks, function(x) {gsub("b' ", "", x)}))
# Remove ending punctuation at the end of euks$bark
euks2 = data.frame(lapply(euks2, function(x) {gsub(".'", "", x)}))

# pull out all text relating to bark that is in hypertext
euks2$bark = as.character(euks2$bark)
backup1 = euks2

linked_sp = grep("EUCLID Glossary_", euks2$bark, fixed = TRUE)
## forget the links- they aren't useful enough to rely on

euks2$bark = gsub("<a class=fsf_tooltip href=glossary.htm#EUCLID Glossary_", "", euks2$bark, fixed = TRUE)
backup = euks2

euks2$bark = gsub(" title[[:punct:]].*?[[:punct:]]{2}a[[:punct:]][^g]", "", euks2$bark, perl = TRUE)
# euks2$bark = gsub("gt[[:punct:]]\\s.*?[[:punct:]]{2}a[[:punct:]]", "", euks2$bark, perl = TRUE)

# euks2$bark = euks2$bark %>% 
#   strsplit(split= c("title=&lt;p&gt;"), fixed=TRUE)

## this needs work still to get it to catch all the cases.
