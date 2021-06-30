start = Sys.time()
t0 = system.time({
  library(tidyverse)
  library(data.table)
  library(vegan)
  library(factoextra)
  
  setwd("/glade/scratch/kjfuller/data")
  
  # assign number of cores and read in data
  setDTthreads(36)
  input = data.table::fread("allvalues_forPCA_na.omit.csv")
  input$ID = c(1:nrow(input))
  xyd = input[,c(1, 2, ncol(input))]
})[[3]]

# input stats
x1 = min(input$x)
x2 = max(input$x)
y1 = min(input$y)
y2 = max(input$y)

capture.output(
  paste0("start of script = ", start),
  paste0("setup time, including loading data = ", t0),
  paste0("nrow(input) before scaling = ", nrow(input)),
  paste0("input min(x) = ", x1),
  paste0("input max(x) = ", x2),
  paste0("input min(y) = ", y1),
  paste0("input max(y) = ", y2),
  file = "PCA_notes.txt"
)

# scale values and write to file
set.seed(225)
t1 = system.time({
  scaled = decostand(input[,c(1:(ncol(input)-1))], method = "range")
})[[3]]
rm(input)
index = list(1:14, 15:40, 41:65, 66:90, 91:115, 116:ncol(scaled))

t2 = system.time({
  for(i in c(1:6)){
    setDTthreads(36)
    data.table::fwrite(scaled[,index[[i]]], paste0("PCA_scaledinputs", i, ".csv"))
  }
})[[3]]

# scaled input stats
x1 = min(scaled$x)
x2 = max(scaled$x)
y1 = min(scaled$y)
y2 = max(scaled$y)

capture.output(
  paste0("time to scale variables = ", t1),
  paste0("nrow(input) after scaling = ", nrow(scaled)),
  paste0("scaled input min(x) = ", x1),
  paste0("scaled input max(x) = ", x2),
  paste0("scaled input min(y) = ", y1),
  paste0("scaled input max(y) = ", y2),
  paste0("time to write scaled inputs = ", t2),
  file = "PCA_notes.txt",
  append = TRUE
)

# run PCA
set.seed(225)
t3 = system.time({
  mod = prcomp(scaled, scale = T)
})[[3]]
rm(scaled)

capture.output(
  paste0("time to run model = ", t3),
  file = "PCA_notes.txt",
  append = TRUE
)

# score outputs
t4 = system.time({
  sco = as.data.frame(scores(mod))
  sco = cbind(xyd, sco)
  for(i in c(1:6)){
    setDTthreads(36)
    data.table::fwrite(sco[,index[[i]]], paste0("PCA_values", i, ".csv"))
  }
})[[3]]
rm(sco)

capture.output(
  paste0("time to extract and write scores = ", t4),
  file = "PCA_notes.txt",
  append = TRUE
)

# figures
tiff("PCA_eigvalfig.tiff", width = 500, height = 500, res = 100)
fviz_eig(mod)
dev.off()
tiff("PCA_sitecos2.tiff", width = 500, height = 500, res = 100)
fviz_pca_ind(mod,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
dev.off()
tiff("PCA_varfig.tiff", width = 500, height = 500, res = 100)
fviz_pca_var(mod,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
)
dev.off()

# site stats
t5 = system.time({
  res.ind = get_pca_ind(mod)
  i1 = data.frame(res.ind$contrib)
  i2 = data.frame(res.ind$cos2)
  nom = c("contrib.", "cos2.")
  for(i in c(1:length(names(i1)))){
    names(i1)[i] = paste(nom[1], i)
    names(i2)[i] = paste(nom[2], i)
  }
  i1 = cbind(xyd, i1)
  i2 = cbind(xyd, i2)
  
  setDTthreads(36)
  for(i in c(1:6)){
    data.table::fwrite(i1[,index[[i]]], paste0("PCA_sitecontrib", i, ".csv"))
  }
  setDTthreads(36)
  for(i in c(1:6)){
    data.table::fwrite(i2[,index[[i]]], paste0("PCA_sitecos2", i, ".csv"))
  }
})[[3]]
rm(res.ind, i1, i2)

capture.output(
  paste0("time to extract and write site stats = ", t5),
  file = "PCA_notes.txt",
  append = TRUE
)

## variable stats
t6 = system.time({
  eigval = get_eigenvalue(mod)
  write.csv(eigval, "PCA_eigenvalues.csv")
  rm(eigval)
  res.var = get_pca_var(mod)
  v1 = data.frame(res.var$coord)
  v2 = data.frame(res.var$cor)
  v3 = data.frame(res.var$cos2)
  v4 = data.frame(res.var$contrib)
  vars = list(v1, v2, v3, v4)
  nom = c("coords.", "corr.", "cos2.", "contrib.")
  for(a in c(1:4)){
    for(i in c(1:length(names(v1)))){
      names(vars[[a]])[i] = paste0(nom[a], i)
    }
  }
  for(a in c(1:4)){
    data.table::fwrite(vars[[i]], paste0("PCA_var", nom[a], ".csv"))
  }
  rm(res.var)
  
  # prediction stats
  v1 = data.frame(mod$center)
  v2 = data.frame(mod$scale)
  v3 = data.frame(mod$rotation)
  vars = cbind(v1, v2, v3)
  write.csv(vars, "PCA_predict.csv")
})[[3]]

capture.output(
  paste0("time to extract and write remaining stats = ", t6),
  file = "PCA_notes.txt",
  append = TRUE
)