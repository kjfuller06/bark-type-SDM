# to do: include lat/lon coords as variabes in the RF, then plot correct and incorrect prediction on a map and explore attributes (following Julia Silge tutorial)
# also, I need to start exporting model inputs and their prediction error values
# create loop for all species and bark types
#     -may not work because of needing to tune the hyperparameters
# create exporting method- best would be markdown but CSV would be fine
# separate model recipe and all standard code to another script and use source()?

library(raster)
library(tidymodels)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(tmap)
library(randomForest)
library(alookr)
library(themis)

# load and filter dataset for RF ####
records = read.csv("data/data_Sp4V6_allenv_80m.csv")
records = drop_na(records)

# remove non-wooded vegetation types and anthropogenic vegetation types
nonwoody = c(56:74)
records = records %>% 
  filter(!(fueltype %in% nonwoody))

Source("scripts/random forest/RF_functions.R")

# species-level random forests####
sp_RF(1)
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
# examine parameters
final_rf
# grab parameters for labeling outputs
label = paste("mtry",
              as.character(final_rf$args$mtry)[2], 
              "trees",
              as.character(final_rf$args$trees)[2],
              "min_n",
              as.character(final_rf$args$min_n)[2],
              sep = "_")


# now lets run the model and test it on the test dataset
final_wf <- workflow() %>%
  add_recipe(mod_rec) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(split_obj)

# calculate true positives and negatives and error rates
metrics = final_res %>%
  collect_metrics()
metrics
sum_1 = final_res %>% 
  collect_predictions()
stats_species = data.frame(species = sp1,
                           sampling = "smote",
                           model = paste(label),  
                           attribute = c("pred0.0",
                                         "pred0.1",
                                         "pred1.0",
                                         "pred1.1",
                                         "accuracy",
                                         "roc_auc",
                                         "resolution"),
                           values = c(mean(sum_1$.pred_0[sum_1$species1 == 0]),
                                      mean(sum_1$.pred_1[sum_1$species1 == 0]),
                                      mean(sum_1$.pred_0[sum_1$species1 == 1]),
                                      mean(sum_1$.pred_1[sum_1$species1 == 1]),
                                      metrics$.estimate[1],
                                      metrics$.estimate[2],
                                      resolution = "~80m"))
stats_species

# bark-level random forests ####
bark_RF(1, "bark1")

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

# examine parameters
final_rf
# grab parameters for labeling outputs
label = paste("mtry",
              as.character(final_rf$args$mtry)[2], 
              "trees",
              as.character(final_rf$args$trees)[2],
              "min_n",
              as.character(final_rf$args$min_n)[2],
              sep = "_")

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
stats_b1 = data.frame(barktype = type,
                      sampling = "smote",
                      model = paste(label), 
                      attribute = c("pred0.0",
                                    "pred0.1",
                                    "pred1.0",
                                    "pred1.1",
                                    "accuracy",
                                    "roc_auc",
                                    "resolution"),
                      values = c(mean(sum_1$.pred_0[sum_1$group_PA == 0]),
                                 mean(sum_1$.pred_1[sum_1$group_PA == 0]),
                                 mean(sum_1$.pred_0[sum_1$group_PA == 1]),
                                 mean(sum_1$.pred_1[sum_1$group_PA == 1]),
                                 metrics$.estimate[1],
                                 metrics$.estimate[2],
                                 resolution = "~80m"))
stats_b1
write.csv(stats_b1, "outputs/dataV4.6_model_stats_80m.csv")
