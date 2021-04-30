library(tidyverse)

pca = read.csv("data/PCA_varstats.csv")
pca = pca[, c(1, 398:411)]
head(pca)

pca$total.contrib = 
  pca$contrib1 + 
  pca$contrib2 + 
  pca$contrib3 + 
  pca$contrib4 + 
  pca$contrib5 +
  pca$contrib6 +
  pca$contrib7 +
  pca$contrib8 +
  pca$contrib9 +
  pca$contrib10 +
  pca$contrib11 +
  pca$contrib12 +
  pca$contrib13 +
  pca$contrib14

pca = pca %>% 
  dplyr::select(X, total.contrib) %>% 
  arrange(-total.contrib)

write.csv(pca, "total_varcontrib.csv", row.names = FALSE)
