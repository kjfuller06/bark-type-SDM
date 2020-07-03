# assign library path
.libPaths("C:/Users/90946112/R/win-library/3.6.2")
library(tidyverse)

# load dataset
flora <- read.delim("EUCLID_FriPy_webscrape2.0.txt", header = FALSE, sep = ".", col.names = "Output", strip.white = TRUE)
