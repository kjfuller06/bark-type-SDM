# script for creating a shiny app for checking out variables
library(shiny)
library(ggplot2)
library(dplyr)

# load labels
nom = read.csv("data/env_variable_labels.csv")
nom = c(paste(nom$labels, nom$labels2, sep = "\n"))

# split for plotting
densities = read.csv("outputs/densities.csv")
byvar = split(densities, densities$variable)

plotify = function(envvar){
  p = byvar[[envvar]] %>%
    ggplot( aes(x=x, y=y, fill=species, color = species)) +
    geom_line() +
    geom_area(alpha = 0.75) +
    ggtitle(nom[envvar]) +
    xlab("") +
    ylab("Proportional Density") +
    theme_bw() +
    theme(legend.position= "none")
  ggplotly(p, tooltip = "fill")
} 

plotify(1)
