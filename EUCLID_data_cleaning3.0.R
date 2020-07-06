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
# euks2$bark = as.character(euks2$bark)
# backup1 = euks2
# links = data.frame()
# for(i in c(1:length(euks2))){
#   
# }
# 
# links = euks2$bark %>% 
#   strsplit(split=" title=&lt;p&gt;", fixed=TRUE) %>% 
#   strsplit(split=" title=&lt;p&gt;", fixed=TRUE)
# 
# 
# links[1,] = gsub(c("<a class=fsf_tooltip href=glossary.htm#EUCLID Glossary_", " title=&lt;p&gt;", "</a>", "&lt;/p&gt;>"), "", euks2$bark[1])
# gsub(" title=&lt;p&gt;", "", euks2$bark[1]) %>% 
# gsub("</a>", "", )
# gsub("&lt;/p&gt;>")
