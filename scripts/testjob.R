setwd("/glade/scratch/kjfuller/data/")
x = data.frame(a = c(1, 2, 3), b = c(10, 20, 30), c = c("car", "train", "boat"))
write.csv("testdf.csv", row.names = FALSE)