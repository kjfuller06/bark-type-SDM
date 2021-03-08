# script for creating PC axes for use in random forest analysis
# 1) load and processing datasets to same CRS, extent and res
#     a) choose intermediate resolution between datasets
#     b) environmental data, species records, land tenure
#         - fire history should be treated as categorical when resampling (i.e. method = 'ngb')
# 2) stack raster layers
# 3) generate subsets of records for:
#     a) each species
#     b) each bark type (1)
#     c) each bark type (2)
# 4) select vegetation types where each grouping is found
# 5) mask raster stack with vegetation layer
# 6) scale all variables
# 7) generate PCA
# 8) save values to disk for random forest analysis

