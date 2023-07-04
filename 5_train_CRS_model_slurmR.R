library(parallel)
library(lightgbm)
library(treesnip)
library(tidyverse)
library(jsonlite)
library(plyr)
library(tidymodels)
library(ggplot2)
library(sf)

suppressMessages(library(unbalanced))


#path_to_files <- "C:/Users/veigel/Documents/NFIP/NFIP/Preprocessing/server/flood_experience_add_data/data/acs_tract_rds_with_policy_count"
path_to_files = "/home/nadja.veigel/data/acs_tract_rds_with_policy_count"

shap_state_files <- list.files(path_to_files, full.names = TRUE, pattern = "_SHAP_2.RData$", recursive = TRUE)
state_names <- substr(list.files(path_to_files, full.names = FALSE, pattern = "_SHAP_2.RData$", recursive = TRUE),0,2)
#sinkfile <- "C:/Users/veigel/Documents/GitHub/NFIP/Preprocessing/server/sink.txt"
#shap_state_files <- shap_state_files[42]
#state_names <- state_names[42]

shap_state_files <- shap_state_files[42]
state_names <- state_names[42]

hyperopt_lightgbm_census <- function(shap_state_files = shap_state_files, path_to_files = "/home/nadja.veigel/data/acs_tract_rds_with_policy_count", state_names = state_names, out_path = "/home/nadja.veigel/data/model_plain_crs_bin", catfile = "/home/nadja.veigel/script/model/hyperopt_plain_crs_bin.out"){
  
  if(state_names == "TO"){state_names = "all"}
  
  
  st <- Sys.time()
  load(shap_state_files)

  cat(file = catfile, append = TRUE , paste0("Loading the data done for ", state_names, " \n"))
  cat(file = catfile, append = TRUE , paste0("preprocessing ", state_names, " \n"))
  
  census_data <- st_drop_geometry(census_data)
  #write.csv(sf_shap,"C:/Users/veigel/Documents/GitHub/NFIP/sampledata.csv", sep = ";")
  #census_data$ratio[!is.finite(census_data$crs)] = NA
  census_data <- census_data[!is.na(census_data$crs),]
  census_data <- census_data[sample(nrow(census_data)),]
  #census_data <- census_data[,-which(grepl( "*concept_house_heating_fuel$" , names( census_data ))) ]

  census_data$crs <- round(census_data$crs,0)
  census_data$crs <- ifelse(census_data$crs == 0, 0,1)
  census_data$crs <- as.factor(census_data$crs)

  if(state_names == "all"){ 
    census_data <- census_data %>%
      select(-pt_count, 
             -estimate_total_concept_total_population.x, 
             -area, 
             -estimate_total_concept_unweighted_sample_count_of_the_population,
             -estimate_total_concept_unweighted_sample_housing_units,
             -estimate_total_concept_total_population.y,
             -state,
             -ratio,
             -building_coverage,
             -contents_coverage)
  }
  
  if(state_names != "all"){ 
    census_data <- census_data %>%
      select(-pt_count, 
             -estimate_total_concept_total_population.x, 
             -area, 
             -estimate_total_concept_unweighted_sample_count_of_the_population,
             -estimate_total_concept_unweighted_sample_housing_units,
             -estimate_total_concept_total_population.y,
             -ratio,
             -building_coverage,
             -contents_coverage)
  }
  

  names_dict <- data.frame("id" = paste0("C", as.character(1:ncol(census_data))), name = names(census_data))
  names_dict$id <- as.character(names_dict$id)
  names_dict$id[which(names_dict$name == "crs")] <- "crs"
  names_dict$id[which(names_dict$name == "geoid")] <- "geoid"
  names(census_data) <- names_dict$id
  
  write.csv(names_dict, paste0(out_path,"/names_dict_", state_names,".csv"))
  
  cat(file = catfile, append = TRUE , paste0("Preprocessing the data done for ", state_names, " \n"))

  #splitSample <- sample(1:2, size=nrow(census_data), prob=c(0.7,0.3), replace = TRUE)
  #train <- census_data[splitSample==1,]
  #valid <- census_data[splitSample==2,]
  #test <- census_data[splitSample==2,]
  
   NA2mean <- function(x) if(is.numeric(x)){replace(x, is.na(x), mean(x, na.rm = TRUE))}else{replace(x, is.na(x), "0")}
   NAN2NA <- function(x) replace(x, is.nan(x), NA)
  # 
  census_data[] <- as.data.frame(lapply(census_data, NAN2NA))

  set.seed(123)
  data_split <- initial_split(census_data)
  
  train_dat<- as.data.frame(lapply(training(data_split), NA2mean))
  train_dat = na.omit(train_dat)
  train_dat$crs = as.factor(train_dat$crs)
  
  test_dat <- testing(data_split)

  train_dat_undersp <- ubBalance(Y=train_dat$crs, 
                                 X=train_dat[,-which(names(train_dat) == "crs")], 
                                 type='ubUnder',         # Option for undersampling
                                 verbose = TRUE)
  
  train_dat = train_dat_undersp$X
  train_dat$crs = train_dat_undersp$Y
  
  mod_rec <- recipe(crs ~ ., data = train_dat) %>%
    add_role(geoid, new_role = "dont_use")
  
  cat(file = catfile, append = TRUE , paste0("train/test split done for ", state_names, " \n"))
  
  cat(file = catfile, append = TRUE , "Defining model \n")
  
  lgb_spec <- boost_tree(
    trees = tune(), 
    tree_depth = tune(), 
    min_n = tune(), 
    loss_reduction = tune(),    
    mtry = tune(),        
    learn_rate = tune()          
  ) %>% 
    set_engine("lightgbm", nthread = 10
               #objective = "binary"
               ) %>% 
    set_mode("classification")
  
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
    metrics = yardstick::metric_set(roc_auc),
    control = control_grid(save_pred = TRUE, parallel_over = "resamples")
  )
  
  cat(file = catfile, append = TRUE , "Grid search done \n")
  
  saveRDS(lgb_res, paste0(out_path,"/grid_search_results_", state_names,".rds"))
  
  best_auc <- select_best(lgb_res, "roc_auc")
  
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
Slurm_Map(hyperopt_lightgbm_census,shap_state_files = shap_state_files, state_names = state_names, tmp_path = "/home/nadja.veigel/tmp", plan = "none", njobs = length(shap_state_files), mc.cores = 10)

#Slurm_Map(hyperopt_lightgbm_census,shap_state_files = shap_state_files[42], state_names = state_names[42], tmp_path = "/home/nadja.veigel/tmp/tmp2", plan = "none", njobs = length(shap_state_files[42]), mc.cores = 40)
