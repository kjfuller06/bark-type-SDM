# species-level random forests ####
# select only the vegetation types where the species is found to be present
sp_RF = function(num){
  rec1 = records[,c(as.numeric(num), 117:(ncol(records)))]
  sp1 <<- colnames(rec1)[1]
  fuels = rec1 %>% 
    filter(rec1[sp1] == 1) %>% 
    dplyr::select(fueltype) %>% 
    unique()
  rec1 = rec1[rec1$fueltype %in% fuels$fueltype,] %>% 
    dplyr::select(-fueltype)
  # rename species for easier looping
  names(rec1)[1] = "species1"
  
  # create training and test samples
  sb = rec1 %>%
    split_by(species1, seed = 6534)
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
  
  # SMOTE = "Synthetic Minority Over-sampling TEchnique"
  # SMOTE with percent under-sampling of 150? This yields equal counts of both classes, with 3x the original number in the minority class
  smote = sb %>% 
    sampling_target(method = "ubSMOTE", seed = 1234L, perc.under = 150)
  smote %>% count(.[,1])
  ## NOTE: SMOTE resampling generates just the training dataset, not the test dataset. That is still from "sb", extracted above and called "test"
  
  # combine training set (smote) with testing set (test)
  test$species1 = as.factor(test$species1)
  split_obj = bind_rows(smote, test)
  prop = nrow(smote) / (nrow(smote) + nrow(test))
  split_obj <<- initial_time_split(split_obj, prop = prop)
  
  # model recipe
  mod_rec <<- recipe(species1 ~ ., data = smote)
  
  # tune for the model specifications
  tune_spec <<- rand_forest(
    mtry = tune(),
    trees = 1000,
    min_n = tune()
  ) %>% 
    set_mode("classification") %>% 
    set_engine("ranger")
  
  # put specs into a workflow
  tune_wf <<- workflow() %>% 
    add_recipe(mod_rec) %>% 
    add_model(tune_spec)
  
  # create cross-validation resamples to use for tuning
  set.seed(234)
  trees_fold <<- vfold_cv(smote)
  
  # tune the hyperparameters for how many predictors to sample at each split (mtry) and how many observations needed to keep splitting nodes (min_n)
  doParallel::registerDoParallel()
  
  set.seed(345)
  tune_res <<- tune_grid(
    tune_wf,
    resamples = trees_fold,
    grid = 20)
  
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
}

# bark-level random forests ####
bark_RF = function(num, category){
  num = 2
  category = "bark1"
  barks = read.csv("data/Horsey_candidate_speciesV.4_colnamesV.2.csv") %>% 
    filter(col_names != "Angophora.leiocarpa",
           col_names != "Eucalyptus.codonocarpa",
           col_names != "Eucalyptus.fraxinoides",
           col_names != "Eucalyptus.acmenioides",
           col_names != "Eucalyptus.delagatensis.subsp..delegatensis",
           col_names != "Eucalyptus.polyanthemos.subsp..vestita",
           col_names != "Eucalyptus.robersonii")
  colnum = which(names(barks) == (category))
  type <<- unique(barks[,paste(category)])[as.numeric(num)]
  b1 = barks %>% 
    filter(barks[,colnum] %in% paste(type))
  
  subset_b1 = records %>% 
    select(b1$col_names,
           names(records[c(117:ncol(records))])) %>% 
    mutate(group_PA = rowSums(records %>% select(b1$col_names)))
  sp_list = unique(b1$col_names)
  subset_b1 = subset_b1 %>% 
    select(-all_of(sp_list))

  # replace non-zero observations with 1
  subset_b1$group_PA[subset_b1$group_PA != 0] = 1
  
  # select only the vegetation types where the species group is found to be present
  rec1 = subset_b1
  fuels = rec1 %>% 
    filter(rec1$group_PA == 1) %>% 
    dplyr::select(fueltype) %>% 
    unique()
  rec1 = rec1[rec1$fueltype %in% fuels$fueltype,] %>% 
    dplyr::select(-fueltype) %>% 
    drop_na()
  
  # create training and test samples
  sb = rec1 %>%
    split_by(group_PA, seed = 6534)
  attributes(sb)
  summary(sb)
  test = sb %>% 
    extract_set(set = "test")
  train = sb %>% 
    extract_set(set = "train")
  
  # SMOTE = "Synthetic Minority Over-sampling TEchnique"
  # SMOTE with percent under-sampling of 150? This yields equal counts of both classes, with 3x the original number in the minority class (actually it doesn't do that this time. This time the minority class goes from 1377 with regular sampling to 2772 with SMOTE)
  smote = sb %>% 
    sampling_target(method = "ubSMOTE", seed = 1234L, perc.under = 150)
  smote %>% count(.[,length(smote)])
  ## NOTE: SMOTE resampling generates just the training dataset, not the test dataset. That is still from "sb", extracted above and called "test"
  
  # combine training set (smote) with testing set (test)
  test$group_PA = as.factor(test$group_PA)
  split_obj = bind_rows(smote, test)
  prop = nrow(smote) / (nrow(smote) + nrow(test))
  split_obj <<- initial_time_split(split_obj, prop = prop)
  
  # model recipe
  mod_rec <<- recipe(group_PA ~ ., data = smote)
  
  # tune for the model specifications
  tune_spec <<- rand_forest(
    mtry = tune(),
    trees = 1000,
    min_n = tune()
  ) %>% 
    set_mode("classification") %>% 
    set_engine("ranger")
  
  # put specs into a workflow
  tune_wf <<- workflow() %>% 
    add_recipe(mod_rec) %>% 
    add_model(tune_spec)
  
  # create cross-validation resamples to use for tuning
  set.seed(234)
  trees_fold <<- vfold_cv(smote)
  
  # tune the hyperparameters for how many predictors to sample at each split (mtry) and how many observations needed to keep splitting nodes (min_n)
  doParallel::registerDoParallel()
  
  set.seed(345)
  tune_res <<- tune_grid(
    tune_wf,
    resamples = trees_fold,
    grid = 20)
  
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
}