library("ape")
library("Biostrings")
library("ggplot2")
library("ggtree")
library(treemap)
library(ade4)
library(tidyverse)
library(phytools)

# manually written tree
x = read.tree("data/tree.txt")
ggtree(x, layout = 'circular')

# generate full phylogeny ####
x = read.csv("data/Nicolle classification_forphylotree.csv", stringsAsFactors = FALSE)

## concatenate to species
# select subspecies and add branch length
s_1 = x[is.na(x$SUBSPECIES) == FALSE,]
s_1$SUBSPECIES = paste0(s_1$SUBSPECIES, ":0.5")
# concatenate entries per species
s_0 = aggregate(cbind(values = SUBSPECIES) ~ SPECIES + GENUS, s_1, paste, collapse=", ")
# add punctuation
s_0$values = paste0("(", s_0$values, ")")
# remove unnecessary columns and combine datasets
x = x %>% 
  dplyr::select(-SUBSPECIES,
                -ORDER)
x = unique(x)
x = x %>%  
  left_join(s_0, by = c("SPECIES", "GENUS"))
x[is.na(x$values) == FALSE,]$SPECIES = x[is.na(x$values) == FALSE,]$values
x = x %>% 
  dplyr::select(-values)

# assign branch lengths to species
x[is.na(x$SUBSERIES) == FALSE,]$SPECIES = paste0(x[is.na(x$SUBSERIES) == FALSE,]$SPECIES, ":1")
x[is.na(x$SUBSERIES) == TRUE & is.na(x$SERIES) == FALSE,]$SPECIES = paste0(x[is.na(x$SUBSERIES) == TRUE & is.na(x$SERIES) == FALSE,]$SPECIES, ":2")
x[is.na(x$SUBSERIES) == TRUE & is.na(x$SERIES) == TRUE & is.na(x$SECTION) == FALSE,]$SPECIES = paste0(x[is.na(x$SUBSERIES) == TRUE & is.na(x$SERIES) == TRUE & is.na(x$SECTION) == FALSE,]$SPECIES, ":3")
x[is.na(x$SUBSERIES) == TRUE & is.na(x$SERIES) == TRUE & is.na(x$SECTION) == TRUE & is.na(x$SUBGENUS) == FALSE,]$SPECIES = paste0(x[is.na(x$SUBSERIES) == TRUE & is.na(x$SERIES) == TRUE & is.na(x$SECTION) == TRUE & is.na(x$SUBGENUS) == FALSE,]$SPECIES, ":4")

## concatenate to subseries
s1 = x[is.na(x$SUBSERIES) == FALSE,]
# concatenate entries per subseries
s1 = aggregate(cbind(final = SPECIES) ~ SUBSERIES + GENUS, s1, paste, collapse=", ")
# add punctuation and branch label
s1$final = paste0("(", s1$final, ")", s1$SUBSERIES)
# add back taxonomic info
pair = x %>% 
  dplyr::select(-SPECIES) %>% 
  unique()
s1 = s1 %>% 
  left_join(pair, by = c("SUBSERIES", "GENUS"))
s1$SUBSERIES = NA
# assign branch lengths to species
s1[is.na(s1$SERIES) == FALSE,]$final = 
  paste0(s1[is.na(s1$SERIES) == FALSE,]$final, ":1")
# cleaning up
x$final = x$SPECIES
x = x %>% 
  dplyr::select(-SPECIES)
x = rbind(x[is.na(x$SUBSERIES) == TRUE,], s1) %>% 
  dplyr::select(-SUBSERIES)

# concatenate to series
s1 = x[is.na(x$SERIES) == FALSE,]
s1 = aggregate(cbind(final = final) ~ SERIES + GENUS, s1, paste, collapse=", ")
# add punctuation and branch label
s1$final = paste0("(", s1$final, ")", s1$SERIES)
# add back taxonomic info
pair = x %>% 
  dplyr::select(-final) %>% 
  unique()
s1 = s1 %>% 
  left_join(pair, by = c("SERIES", "GENUS"))
s1$SERIES = NA
# assign branch lengths to species
s1[is.na(s1$SECTION) == FALSE,]$final = 
  paste0(s1[is.na(s1$SECTION) == FALSE,]$final, ":1")
s1[is.na(s1$SECTION) == TRUE & is.na(s1$SUBGENUS) == TRUE & is.na(s1$GENUS) == FALSE,]$final = 
  paste0(s1[is.na(s1$SECTION) == TRUE & is.na(s1$SUBGENUS) == TRUE & is.na(s1$GENUS) == FALSE,]$final, ":3")
# cleaning up
x = rbind(x[is.na(x$SERIES) == TRUE,], s1) %>% 
  dplyr::select(-SERIES)


## concatenate to section
s1 = x[is.na(x$SECTION) == FALSE,]
s1 = aggregate(cbind(final = final) ~ SECTION + GENUS, s1, paste, collapse=", ")
# add punctuation and branch label
s1$final = paste0("(", s1$final, ")", s1$SECTION)
# add back taxonomic info
pair = x %>% 
  dplyr::select(-final) %>% 
  unique()
s1 = s1 %>% 
  left_join(pair, by = c("SECTION", "GENUS"))
s1$SECTION = NA
# assign branch lengths to species
s1[is.na(s1$SUBGENUS) == FALSE,]$final = 
  paste0(s1[is.na(s1$SUBGENUS) == FALSE,]$final, ":1")
# cleaning up
x = rbind(x[is.na(x$SECTION) == TRUE,], s1) %>% 
  dplyr::select(-SECTION)

## concatenate to subgenus
s1 = x[is.na(x$SUBGENUS) == FALSE,]
s1 = aggregate(cbind(final = final) ~ SUBGENUS + GENUS, s1, paste, collapse=", ")
# add punctuation and branch label
s1$final = paste0("(", s1$final, ")", s1$SUBGENUS)
s1$SUBGENUS = NA
# assign branch lengths to species
s1$final = paste0(s1$final, ":1")
# cleaning up
x = rbind(x[is.na(x$SUBGENUS) == TRUE,], s1) %>% 
  dplyr::select(-SUBGENUS)

backup = x
## concatenate to genus
x = aggregate(cbind(final = final) ~ GENUS, x, paste, collapse=", ")
# add punctuation and branch label
x$final = paste0("(", x$final, ")", x$GENUS)
# assign branch lengths to species
x$final = paste0(x$final, ":1")

backup = x
# final concatenation
x$all = "Eucalypts"
final = aggregate(cbind(final = final) ~ all, x, paste, collapse=", ")
final = paste0("(", final$final, ")Eucalypts;")

# write to disk
write.csv(final, "data/firstphylo.csv")
open = file("data/firstphylo.txt")
writeLines(final, open)
close(open)

# colours and options ####
tree = read.tree("data/firstphylo.txt")
g = ggtree(tree, layout = 'circular', branch.length = "none")
write.tree(tree, file = "firstphylo.nwk")

# colour by genus
tree$node.label
# findMRCA(tree, tips = c(123:1004), type = "node")
# tree = groupClade(tree, .node = c(1006, 1013, 1042), group_name = c("Angophora"))
tree[[6]] = c(rep("Angophora", 13), rep("Corymbia", 109), rep("Eucalyptus", 882))
names(tree)[6] = "group"
groupInfo = split(tree$tip.label, tree$group)
tree2 = groupOTU(tree, groupInfo)
ggtree(tree2, aes(color = group), layout = 'circular', branch.length = "none") +
  labs(color = "Genus") +
  geom_tiplab(size = 1, aes(angle = angle))

# junk ####
data(chiroptera, package="ape")
groupInfo <- split(chiroptera$tip.label, gsub("_\\w+", "", chiroptera$tip.label))
chiroptera <- groupOTU(chiroptera, groupInfo)
ggtree(chiroptera, aes(color=group), layout='circular') + geom_tiplab(size=1, aes(angle=angle))

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
