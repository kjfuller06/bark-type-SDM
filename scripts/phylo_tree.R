library("ape")
library("Biostrings")
library("ggplot2")
library("ggtree")
library(treemap)
library(ade4)
library(tidyverse)

x = read.tree("data/tree.txt")
ggtree(x, layout = 'circular')

x = read.csv("data/Nicolle classification_forphylotree.csv", stringsAsFactors = FALSE)

x[is.na(x$SUBSERIES) == FALSE,]$FullName = paste0(x[is.na(x$SUBSERIES) == FALSE,]$FullName, ":1")
test = aggregate(cbind(values = FullName) ~ SUBSERIES + SERIES + SECTION + SUBGENUS + GENUS, b, paste, collapse=",")
b = x %>% 
  filter(is.na(SUBSERIES) == TRUE)


data(taxo.eg)
tax = as.taxo(taxo.eg[[1]])
tax.phy = taxo2phylog(as.taxo(taxo.eg[[1]]))
ggtree(tax.phy) + geom_tiplab() + geom_nodelab(geom='label')

tax2 = read.csv("data/Nicolle classification V4.csv", stringsAsFactors = TRUE)
rownames(tax2) = tax2$FullName
tax2 = tax2 %>% 
  dplyr::select(-FullName,
                -ORDER)
# tax2 = as.taxo(tax2)
as.taxo3 <-                                                             
  function (df) {
    fac <- as.character(df[, nc])
    for (i in (nc - 1):1) fac <- paste(fac,as.character(df[, i]),sep=":")
    df <- df[order(fac), ]
    class(df) <- c("data.frame", "taxo")
    return(df)
  }
tax2 = as.taxo3(tax2)

tax.phy = taxo2phylog(tax2)

url <- paste0("https://raw.githubusercontent.com/TreeViz/",
             "metastyle/master/design/viz_targets_exercise/")

x <- read.tree(text = '(((Rangifer_tarandus:1, Cervus_elaphus:1)Cervidae:1[98], (Bos_taurus:1, Ovis_orientalis:1)Bovidae:1[99])Artiodactyla:1[92], (Suricata_suricatta:2, (Cystophora_cristata:1,Mephitis_mephitis:1)Caniformia:1[98])Carnivora:1[96])Mammalia;')
info <- read.csv(paste0(url, "tip_data.csv"))
d2 <- read.csv(paste0(url, "inode_data.csv"))
p <- ggtree(x) %<+% info + xlim(-.1, 4)
p2 <- p + geom_tiplab(offset = .6, hjust = .5)
p2 %<+% d2 + geom_label(aes(label = vernacularName.y))

tree <- ape::read.tree(text = '(((1:0.5, 2:0.5):1, 3:1)Costatitae_ms:2, ((4:1, 5:1, 6:1, 7:1, 8:1, 9:1)Floribundinae_ms:1, (10:1)Melanoxylon_ms:1, (11:1, 12:1, 13:1)Angophora_ms:1)Angophora:1)Angophora;')
genus = c(rep("Angophora", 13))
species = c("costata~subsp.~euryphylla", 
            "costata~subsp.~costata", 
            "leiocarpa",
            "bakeri",
            "paludosa",
            "woodsiana",
            "floribunda",
            "?inopina",
            "crassifolia",
            "melanoxylon",
            "subvelutina",
            "?robur",
            "hispida")
d = data.frame(label = tree$tip.label, genus = genus, species = species)
d3 = data.frame(newick_label = c("Costatitae_ms", 
                                 "Floribundinae_ms", 
                                 "Melanoxylon_ms", 
                                 "Angophora_ms", 
                                 "Angophora", 
                                 "Angophora"),
                proper_label = c("'Costatitae' ms",
                                 "'Floribundinae' ms",
                                 "'Melanoxylon' ms",
                                 "'Angophora' ms",
                                 "Angophora",
                                 "Angophora"),
                rank = c("Series",
                         "Subseries",
                         "Subseries",
                         "Subseries",
                         "Series",
                         "Genus"))
p2 = ggtree(tree, layout = 'circular') %<+% d + xlim(NA, 6) +
  geom_tiplab(aes(label=paste0('italic(', genus, '~', species, ')')), 
              parse=T) 
p2 %<+% d3 + geom_label(aes(label = proper_label))
p2 + geom_nodelab(geom = 'label')

data("GNI2014", package = "treemap")
n = GNI2014[, c(3, 1)]
n[, 1] = as.character(n[, 1])
n[, 1] = gsub("\\s\\(.*\\)", "", n[, 1])

w = cbind("World", as.character(unique(n[, 1])))

colnames(w) = colnames(n)
edgelist = rbind(n, w)

y = as.phylo(edgelist)
ggtree(y, layout = 'circular') %<+% GNI2014 + 
  aes(color=continent) + geom_tippoint(aes(size=population), alpha=.6) + 
  geom_tiplab(aes(label=country), offset=.1) +
  theme(plot.margin=margin(60,60,60,60))


tree <- read.tree(text = "((a,(b,c)),d);")
genus <- c("Gorilla", "Pan", "Homo", "Pongo")
species <- c("gorilla", "spp.", "sapiens", "pygmaeus")
geo <- c("Africa", "Africa", "World", "Asia")
d <- data.frame(label = tree$tip.label, genus = genus,
                species = species, geo = geo)

p3 <- ggtree(tree) %<+% d + xlim(NA, 6) +
  geom_tiplab(aes(label=paste0('italic(', genus, 
                               ')~bolditalic(', species, ')~', geo)), 
              parse=T)
p3
