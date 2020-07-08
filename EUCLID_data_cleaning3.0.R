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

# string1 = " title=&lt;p&gt;"
# links = euks2 %>%
#   strsplit(euks2$bark, split= string1, fixed=TRUE) %>%
#   strsplit(bark, split=" title=&lt;p&gt;", fixed=TRUE)
# 
# links[1,] = gsub(c("<a class=fsf_tooltip href=glossary.htm#EUCLID Glossary_", " title=&lt;p&gt;", "</a>", "&lt;/p&gt;>"), "", euks2$bark[1], fixed = TRUE)
# gsub(" title=&lt;p&gt;", "", euks2$bark[1], fixed = TRUE) %>%
# gsub("</a>", "", fixed = TRUE)
# gsub("&lt;/p&gt;>", fixed = TRUE)
