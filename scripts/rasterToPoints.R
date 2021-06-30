library(raster)
library(sf)
library(parallel)
library(snowfall)

captute.output(
  paste0("libraries loaded"),
  file = "raster_extract_notes.txt"
)

setwd("/glade/scratch/kjfuller/data")

mask = list.files("./", pattern = "^mask", recursive = FALSE, full.names = TRUE)

df_fun = function(x){
  r = raster(mask[x])
  n1 = sum(is.na(values(r)))
  df = as.data.frame(rasterToPoints(r), xy = TRUE)
  n2 = sum(is.na(df))
  lab = substr(mask[x], 14, nchar(mask[x])-4)
  write.csv(df, paste0(lab, "_forPCA.csv"), row.names = FALSE)
  capture.output(
    paste0("nrow of ", lab, " = ", nrow(df)),
    paste0("NaNs in raster of ", lab, " = ", n1),
    paste0("NaNs in df of ", lab, " = ", n2),
    file = paste0("raster_extract_", lab, ".txt")
  )
}

capture.output(
  paste0("mask list and function loaded"),
  paste0("initiating snowfall"),
  file = "raster_extract_notes.txt",
  append = TRUE
)

sfInit(parallel = TRUE, cpus = 36)
sfExport("mask", "df_fun")
sfLibrary(raster)
sfLibrary(sf)

sfLapply(c(1:length(mask)), df_fun)

sfStop()
