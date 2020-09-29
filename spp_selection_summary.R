# script for visualising species selections

# read records in again for plotting
records = st_read("data/HorseyV.2_extracted_dataV.1.shp")
st_geometry(records) = NULL

# do some environmental plotting
r = records$ribbons
b1 = records$bark1
b2 = records$bark2

# visualisations:
#   1) proportion of points occurring along an environmental gradient (as in, the proportion of temp observations that occur at each temperature), coloured and shaded by group- these could be curves for single variables and hexbins for two variables
#   2) could also do that precip by temp visualisation Jeff sent me for the oaks project- with the overall distribution of all species in gray and individual species highlighted in red
