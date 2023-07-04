library(parallel)
library(lightgbm)
library(treesnip)
library(tidyverse)
library(jsonlite)
library(plyr)
library(tidymodels)
library(ggplot2)
library(sf)


path_to_files = "/home/nadja.veigel/data/acs_tract_rds_with_policy_count"

shap_state_files <- list.files(path_to_files, full.names = TRUE, pattern = "_SHAP_2.RData$", recursive = TRUE)
state_names <- substr(list.files(path_to_files, full.names = FALSE, pattern = "_SHAP_2.RData$", recursive = TRUE),0,2)


hyperopt_lightgbm_census <- function(shap_state_files = shap_state_files, path_to_files = "/home/nadja.veigel/data/acs_tract_rds_with_policy_count", state_names = state_names, out_path = "/home/nadja.veigel/data/model_plain", catfile = "/home/nadja.veigel/script/model/hyperopt_plain.out", only_social = FALSE){
  
  if(state_names == "TO"){state_names = "all"}
  
  
  st <- Sys.time()
  load(shap_state_files)

  cat(file = catfile, append = TRUE , paste0("Loading the data done for ", state_names, " \n"))
  cat(file = catfile, append = TRUE , paste0("preprocessing ", state_names, " \n"))
  
  census_data <- st_drop_geometry(census_data)
  census_data$ratio[!is.finite(census_data$ratio)] = NA
  census_data <- census_data[!census_data$ratio > 1,]
  
  census_data <- census_data[sample(nrow(census_data)),]
  census_data <- census_data[,-which(grepl( "*concept_house_heating_fuel$" , names( census_data ))) ]

  
  if(state_names == "all"){ 
    census_data <- census_data %>%
      select(-pt_count, 
             -estimate_total_concept_total_population.x, 
             -area, 
             -estimate_total_concept_unweighted_sample_count_of_the_population,
             -estimate_total_concept_unweighted_sample_housing_units,
             -estimate_total_concept_total_population.y,
             -state)
  }
  
  if(state_names != "all"){ 
    census_data <- census_data %>%
      select(-pt_count, 
             -estimate_total_concept_total_population.x, 
             -area, 
             -estimate_total_concept_unweighted_sample_count_of_the_population,
             -estimate_total_concept_unweighted_sample_housing_units,
             -estimate_total_concept_total_population.y)
  }
  
  if(isTRUE(only_social)){ 
    census_data <- census_data %>%
      select(-mean_floods_per_year, 
             -max_affected_per_flood, 
             -crs, 
             -pop_density,
             -area_A_percent,
             -area_V_percent,
             -building_coverage,
             -contents_coverage)
  }
  

  names_dict <- data.frame("id" = paste0("C", as.character(1:ncol(census_data))), name = names(census_data))
  names_dict$id <- as.character(names_dict$id)
  names_dict$id[which(names_dict$name == "ratio")] <- "ratio"
  names_dict$id[which(names_dict$name == "geoid")] <- "geoid"
  names(census_data) <- names_dict$id
  
  write.csv(names_dict, paste0(out_path,"/names_dict_", state_names,".csv"))
  
  cat(file = catfile, append = TRUE , paste0("Preprocessing the data done for ", state_names, " \n"))

  set.seed(123)
  data_split <- initial_split(census_data)
  
  train_dat <- training(data_split)
  test_dat <- testing(data_split)
 
  mod_rec <- recipe(ratio ~ ., data = train_dat) %>%
    add_role(geoid, new_role = "dont_use")
  
  cat(file = catfile, append = TRUE , paste0("train/test split done for ", state_names, " \n"))
  
  cat(file = catfile, append = TRUE , "Defining model \n")
  
  if(state_names == "all"){
  lgb_spec <- boost_tree(
    trees = tune(), 
    tree_depth = tune(), 
    min_n = tune(), 
    loss_reduction = tune(),    
    mtry = tune(),        
    learn_rate = tune()           
  ) %>% 
    set_engine("lightgbm", objective = "tweedie", nthread = 20, tweedie_variance_power = 1.2) %>% #objective = "mape" #, num_threads = 10 # used to be 1.3 !!!!!!! changed 05.01.2022
    set_mode("regression")
  }else{
    lgb_spec <- boost_tree(
      trees = tune(), 
      tree_depth = tune(), 
      min_n = tune(), 
      loss_reduction = tune(),    
      mtry = tune(),        
      learn_rate = tune()           
    ) %>% 
      set_engine("lightgbm", objective = "tweedie", nthread = 20, tweedie_variance_power = 1.7) %>% #objective = "mape" #, num_threads = 10
      set_mode("regression")
  }
  
  cat(file = catfile, append = TRUE , "Defining grid \n")
  
  
  lgb_grid <- grid_latin_hypercube(
    trees(),
    tree_depth(),
    min_n(),
    loss_reduction(),
    finalize(mtry(), train_dat),
    learn_rate(),
    size = 80 # 80 on cluster
  ) 
  
  lgb_wf <- workflow() %>%
    add_recipe(mod_rec) %>%
    add_model(lgb_spec)
  
  cat(file = catfile, append = TRUE , "Starting tuning \n")
  
  set.seed(123)
  resamples_train <- vfold_cv(train_dat, v = 10) #10 on server
  
  cat(file = catfile, append = TRUE , "Defined 10 folds \n")
  
  set.seed(123)
  lgb_res <- tune_grid(
    lgb_wf,
    resamples = resamples_train,
    grid = lgb_grid,
    metrics = yardstick::metric_set(rmse, rsq, mae),
    control = control_grid(save_pred = TRUE, parallel_over = "resamples")
  )
  
  cat(file = catfile, append = TRUE , "Grid search done \n")
  
  saveRDS(lgb_res, paste0(out_path,"/grid_search_results_", state_names,".rds"))
  
  if(state_names == "all"){ 
  best_auc <- select_best(lgb_res, "rsq")
  }
  if(state_names != "all"){ 
  best_auc <- select_by_one_std_err(lgb_res, metric = "rsq", tree_depth)
  }
  
  write.csv(best_auc, paste0(out_path,"/best_params_", state_names,".csv"))
  
  cat(file = catfile, append = TRUE , "find best params \n")
  
  cat(file = catfile, append = TRUE , "finalize workflow \n")
  
  final_lgb <- finalize_workflow(
    lgb_wf,
    best_auc
  ) 
  
  cat(file = catfile, append = TRUE , "final training \n")
  
  set.seed(123)
  final_lgb <- fit(final_lgb, train_dat)
  
  cat(file = catfile, append = TRUE , "final training done \n")
  
  final_lgb_fit <-  pull_workflow_fit(final_lgb)
  
  save(final_lgb_fit, file = paste0(out_path,"/trained_model_rdata", state_names,".RData"))
  
  lgb.save(final_lgb_fit$fit, paste0(out_path, "/",state_names,"_lightgbm.model"))
  
  
  predictions_test <- final_lgb %>%
    predict(new_data = test_dat) %>%
    bind_cols(test_dat %>% select(ratio)) %>% 
    mutate(train_test = "test", geoid = test_dat$geoid)
  
  
  metrics_test <- final_lgb %>%
    predict(new_data = test_dat) %>%
    bind_cols(test_dat %>% select(ratio)) %>% 
    metric_set(rmse, mae, rsq)(test_dat$ratio, .pred) %>% 
    mutate(train_test = "test")
  
  predictions_train <- final_lgb %>%
    predict(train_dat) %>%
    bind_cols(train_dat %>% select(ratio)) %>% 
    mutate(train_test = "train", geoid = train_dat$geoid)
  
  metrics_train <- final_lgb %>%
    predict(train_dat) %>%
    bind_cols(train_dat %>% select(ratio)) %>%  
    metric_set(rmse, mae, rsq)(train_dat$ratio, .pred) %>% 
    mutate(train_test = "train")
  
  metrics <- rbind(metrics_test, metrics_train)
  predictions <- rbind(predictions_test, predictions_train)
  
  cat(file = catfile, append = TRUE , "export predictions \n")
  
  write.csv(metrics, paste0(out_path,"/test_test_metrics_", state_names,".csv"))
  write.csv(predictions, paste0(out_path,"/test_train_predictions_", state_names,".csv"))
  
  et <- Sys.time()
  
  cat(
    paste("Time diffrence for ", state_names, " : ", difftime(et, st, units = "mins")[[1]], "mins", "\n"),
    file =  catfile,
    append = TRUE,
    sep = "\n"
  )

}


library(slurmR)

# Run parallel computation
Slurm_Map(hyperopt_lightgbm_census,shap_state_files = shap_state_files, state_names = state_names, tmp_path = "/home/nadja.veigel/tmp", plan = "none", njobs = length(shap_state_files), mc.cores = 20)

#Slurm_Map(hyperopt_lightgbm_census,shap_state_files = shap_state_files[42], state_names = state_names[42], tmp_path = "/home/nadja.veigel/tmp/tmp2", plan = "none", njobs = length(shap_state_files[42]), mc.cores = 40)
