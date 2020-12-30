# to do: include lat/lon coords as variabes in the RF, then plot correct and incorrect prediction on a map and explore attributes (following Julia Silge tutorial)

library(raster)
library(tidymodels)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(tmap)
library(randomForest)
library(alookr)
library(themis)

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
# test = smote %>% 
#   extract_set(set = "test")
# ^doesn't work. I think because doing a SMOTE resampling generates just the training dataset, not the test dataset. That is still from "sb", extracted above and called "test"

# combine training set (smote) with testing set (test)
test$Eucalyptus.beyeriana = as.factor(test$Eucalyptus.beyeriana)
split_obj = bind_rows(smote, test)
prop = nrow(smote) / (nrow(smote) + nrow(test))
split_obj <- initial_time_split(split_obj, prop = prop)

# model recipe
mod_rec = recipe(Eucalyptus.beyeriana ~ ., data = smote)

# tune for the model specifications
tune_spec = rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
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

# tune the hyperparameters for how many predictors to sample at each split (mtry) and how many observations needed to keep splitting nodes (min_n)
doParallel::registerDoParallel()

set.seed(345)
tune_res = tune_grid(
  tune_wf,
  resamples = trees_fold,
  grid = 20
)
tune_res
# plot results
tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")
# more detailed look at the best value ranges
rf_grid <- grid_regular(
  mtry(range = c(2, 16)),
  min_n(range = c(2, 12)),
  levels = 8
)
rf_grid
# tune parameters again, this time with a more targeted range of hyperparameters
set.seed(456)
regular_res <- tune_grid(
  tune_wf,
  resamples = trees_fold,
  grid = rf_grid
)
regular_res
# plot new results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# select the best values for hyperparameters for final model specs
best_auc <- select_best(regular_res, "roc_auc")

final_rf <- finalize_model(
  tune_spec,
  best_auc
)

final_rf

# now lets run the model and test it on the test dataset
final_wf <- workflow() %>%
  add_recipe(mod_rec) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(split_obj)

# calculate true positives and negatives and error rates
final_res %>%
  collect_metrics()
sum_1 = final_res %>% 
  collect_predictions()
stats = data.frame(pred_acc = c(
  pred0.0 = mean(sum_1$.pred_0[sum_1$Eucalyptus.beyeriana == 0]),
  pred0.1 = mean(sum_1$.pred_1[sum_1$Eucalyptus.beyeriana == 0]),
  pred1.0 = mean(sum_1$.pred_0[sum_1$Eucalyptus.beyeriana == 1]),
  pred1.1 = mean(sum_1$.pred_1[sum_1$Eucalyptus.beyeriana == 1])
))
stats

# stringybarks ####
# add all observations of the following species to create the stringybark category
barks = read.csv("data/Horsey_candidate_speciesV.4_colnames.csv")
stringies = barks %>% 
  filter(bark1 == "stringybark")

stringybarks = records %>% 
  select(stringies$col_names,
         names(records[c(117:148)])) %>% 
  mutate(group_PA = rowSums(records %>% select(stringies$col_names)))

# replace non-zero observations with 1
stringybarks$group_PA[stringybarks$group_PA != 0] = 1

# select only the vegetation types where the species group is found to be present
rec1 = stringybarks[,c(22:54)]
b1 = "group_PA"
fuels = rec1 %>% 
  filter(rec1[b1] == 1) %>% 
  dplyr::select(fueltype) %>% 
  unique()
rec1 = rec1[rec1$fueltype %in% fuels$fueltype,] %>% 
  dplyr::select(-fueltype)

# create training and test samples
sb = rec1 %>%
  split_by(paste(b1), seed = 6534)
attributes(sb)
summary(sb)
test = sb %>% 
  extract_set(set = "test")
train = sb %>% 
  extract_set(set = "train")

# SMOTE? "Synthetic Minority Over-sampling TEchnique"
# SMOTE with percent under-sampling of 150? This yields equal counts of both classes, with 3x the original number in the minority class (actually it doesn't do that this time. This time the minority class goes from 1377 with regular sampling to 2772 with SMOTE)
smote = sb %>% 
  sampling_target(method = "ubSMOTE", seed = 1234L, perc.under = 150)
smote %>% count(.[,length(smote)])
# test = smote %>% 
#   extract_set(set = "test")
# ^doesn't work. I think because doing a SMOTE resampling generates just the training dataset, not the test dataset. That is still from "sb", extracted above and called "test"

# combine training set (smote) with testing set (test)
test$group_PA = as.factor(test$group_PA)
split_obj = bind_rows(smote, test)
prop = nrow(smote) / (nrow(smote) + nrow(test))
split_obj <- initial_time_split(split_obj, prop = prop)

# model recipe
mod_rec = recipe(group_PA ~ ., data = smote)

# tune for the model specifications
tune_spec = rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
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

# tune the hyperparameters for how many predictors to sample at each split (mtry) and how many observations needed to keep splitting nodes (min_n)
doParallel::registerDoParallel()

set.seed(345)
tune_res = tune_grid(
  tune_wf,
  resamples = trees_fold,
  grid = 20
)
tune_res
# plot results
tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")
# more detailed look at the best value ranges
rf_grid <- grid_regular(
  mtry(range = c(1, 10)),
  min_n(range = c(2, 15)),
  levels = 10
)
rf_grid
# tune parameters again, this time with a more targeted range of hyperparameters
set.seed(456)
regular_res <- tune_grid(
  tune_wf,
  resamples = trees_fold,
  grid = rf_grid
)
regular_res
# plot new results
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# select the best values for hyperparameters for final model specs
best_auc <- select_best(regular_res, "roc_auc")

final_rf <- finalize_model(
  tune_spec,
  best_auc
)

final_rf

# now lets run the model and test it on the test dataset
final_wf <- workflow() %>%
  add_recipe(mod_rec) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(split_obj)

# calculate true positives and negatives and error rates
final_res %>%
  collect_metrics()
sum_1 = final_res %>% 
  collect_predictions()
stats = data.frame(pred_acc = c(
  pred0.0 = mean(sum_1$.pred_0[sum_1$group_PA == 0]),
  pred0.1 = mean(sum_1$.pred_1[sum_1$group_PA == 0]),
  pred1.0 = mean(sum_1$.pred_0[sum_1$group_PA == 1]),
  pred1.1 = mean(sum_1$.pred_1[sum_1$group_PA == 1])
))
stats

