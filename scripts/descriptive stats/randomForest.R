library(raster)
library(tidymodels)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(tmap)
library(randomForest)
library(alookr)

# testing random forest ####
records = read.csv("data/HorseyV.4_extracted_dataV.5_P-A_allenv.csv")

records = drop_na(records)
# records$fueltype = as.factor(records$fueltype)
### can't use fuel type yet because it has too many categories for a categorical variable

# remove non-wooded vegetation types and anthropogenic vegetation types
nonwoody = c(56:74)
records = records %>% 
  filter(!(fueltype %in% nonwoody))

# select only the vegetation types where the species is found to be present
rec1 = records[,c(1, 117:148)]
sp1 = colnames(rec1)[1]
fuels = rec1 %>% 
  filter(rec1[sp1] == 1) %>% 
  dplyr::select(fueltype) %>% 
  unique()
rec1 = rec1[rec1$fueltype %in% fuels$fueltype,] %>% 
  dplyr::select(-fueltype)

# create training and test samples
sb = rec1 %>%
  split_by(paste(sp1), seed = 6534)
attributes(sb)
summary(sb)
test = sb %>% 
  extract_set(set = "test")
train = sb %>% 
  extract_set(set = "train")

# undersample majority class to correct class imbalance
under = sb %>% 
  sampling_target(seed = 1234L)
under %>% count(.[,1])

# oversample minority class
over = sb %>% 
  sampling_target(seed = 1234L, method = "ubOver", k = 73)
over %>% count(.[,1])

# SMOTE? "Synthetic Minority Over-sampling TEchnique"
# SMOTE with percent under-sampling of 150? This yields equal counts of both classes, with 3x the original number in the minority class
smote = sb %>% 
  sampling_target(method = "ubSMOTE", seed = 1234L, perc.under = 150)
smote %>% count(.[,1])
test = smote %>% 
  extract_set(set = "test")

# model recipe
mod_rec = recipe(Eucalyptus.beyeriana ~ ., data = smote)

# tune for the model specifications
tune_spec = rand_forest(
  trees = 1000
) %>% 
  set_mode("classification") %>% 
  set_engine("ranger")

# put specs into a workflow
tune_wf = workflow() %>% 
  add_recipe(mod_rec) %>% 
  add_model(tune_spec)

# create cross-validation resamples to use for tuning
set.seed(234)
trees_fold = vfold_cv(smote)

# tune the hyperparameters
doParallel::registerDoParallel()

set.seed(345)
tune_res = tune_grid(
  tune_wf,
  resamples = trees_fold,
  grid = 20
)
##^this doesn't fucking work

# run random forest for a selected species
rf_sp1 = randomForest(x = smote[,1], y = smote[,c(2:32)], ntree=1000, nodesize=10, importance =T)
rf_sp1
# check importance of variables
importance(rf_sp1)

df = data.frame(sp = seq(1:116), "correct0" = seq(1:116), "false1" = seq(1:116), "false0" = seq(1:116), "correct1" = seq(1:116), class.error0 = seq(1:116), class.error1 = seq(1:116))

df = data.frame(sp = names(records[52]), "correct0" = rf_sp1$confusion[1, 1], "false1" = rf_sp1$confusion[1, 2], "false0" = rf_sp1$confusion[2, 1], "correct1" = rf_sp1$confusion[2, 2], class.error0 = rf_sp1$confusion[1, 3], class.error1 = rf_sp1$confusion[2, 3])

write.csv(df, "outputs/randomForest_HorseyV.4_extracted_dataV.5_P-!_allenv.csv", row.names = FALSE)

# let's look at a bark type
# add all observations of the following species to create the stringybark category
records$stringy = records$Eucalyptus.agglomerata + 
  records$Eucalyptus.baileyana + 
  records$Eucalyptus.blaxlandii + 
  records$Eucalyptus.caliginosa + 
  records$Eucalyptus.cameronii + 
  records$Eucalyptus.capitellata + 
  records$Eucalyptus.cinerea + 
  records$Eucalyptus.eugenioides + 
  records$Eucalyptus.fastigata + 
  records$Eucalyptus.globoidea + 
  records$Eucalyptus.laevopinea + 
  records$Eucalyptus.macrorhyncha + 
  records$Eucalyptus.microcorys + 
  records$Eucalyptus.muelleriana + 
  records$Eucalyptus.obliqua + 
  records$Eucalyptus.resinifera + 
  records$Eucalyptus.sparsifolia + 
  records$Eucalyptus.tindaliae + 
  records$Eucalyptus.williamsiana + 
  records$Eucalyptus.youmanii + 
  records$Syncarpia.glomulifera
# replace non-zero observations with 1
records$stringy[records$stringy != 0] = 1

# run random forest for a selected species
rf_b1 = randomForest( x=records[,118:148], y=as.factor(records[,151]), ntree=1000, nodesize=10, importance =T)
rf_b1
# check importance of variables
importance(rf_b1)
