#-------------------------------------------------------------------------------
#Adding the percentage of insured belongings as well as the Community rating system 
#to the census tracts with policies dataset
#
# 1 - read policies
# 2 - select date of original purchase
# 3 - plot development
#-------------------------------------------------------------------------------

library(lightgbm)
library(treesnip)
library(pvclust)
library(dplyr)
library(tidyverse)
library(jsonlite)
library(plyr)
library(tidymodels)
library(sf)
library(SHAPforxgboost)
library(data.table)
library(viridis)
library(ggthemes)
library(ggpubr)
library(caret)


 out_path = "C:/Users/veigel/Documents/NFIP/NFIP/model/server/results_crs_bin_unbalance/model_plain_crs_bin/"
# 
out_path = "C:/Users/veigel/Documents/NFIP/NFIP/model/server/results_23_02_2022_unbalance_recipe/model_plain_crs_2/"

out_path <-"/home/nadja.veigel/data/model_plain_crs_2/"

path_to_files = "/home/nadja.veigel/data/acs_tract_rds_with_policy_count"
#out_path = "/home/nadja.veigel/data/model_plain_crs"

path_to_files <- "C:/Users/veigel/Documents/NFIP/NFIP/Preprocessing/server/flood_experience_add_data/data/acs_tract_rds_with_policy_count"
#path_to_files = "/home/nadja.veigel/data/acs_tract_rds_with_policy_count"

shap_state_files <- list.files(path_to_files, full.names = TRUE, pattern = "_SHAP_2.RData$", recursive = TRUE)
state_names <- substr(list.files(path_to_files, full.names = FALSE, pattern = "_SHAP_2.RData$", recursive = TRUE),0,2)

shap_state_files <- shap_state_files[42]
state_names <- state_names[42]

# shap_state_files <- shap_state_files[3]
# state_names <- state_names[3]
groupfile = "C:/Users/veigel/Documents/NFIP/NFIP/shap/hierachy.csv"
groupfile2 = "C:/Users/veigel/Documents/NFIP/NFIP/shap/hierachy_positive_negative_groups.csv"
catfile = "C:/Users/veigel/Documents/caty.csv"
bin = TRUE

groupfile = "/home/nadja.veigel/data/shap/hierachy.csv"
groupfile2 = "/home/nadja.veigel/data/shap/hierachy_positive_negative_groups.csv"
catfile = "/home/nadja.veigel/script/shap/shap.out"
#shap.plot.summary <- edit(shap.plot.summary)

shap_lightgbm_census <- function(state_names = state_names, shap_state_files = "/home/nadja.veigel/data/acs_tract_rds_with_policy_count/TOT/all_census_ratio_SHAP_2.RData", out_path = "/home/nadja.veigel/data/model_plain_crs_2/" ,catfile = "/home/nadja.veigel/script/shap/shap.out" , groupfile = "/home/nadja.veigel/data/shap/hierachy.csv",  groupfile2 = "/home/nadja.veigel/data/shap/hierachy_positive_negative_groups.csv", bin = TRUE){


  if(state_names == "TO"){state_names = "all"}

  #-------------------------------------------------------------------------------
  # Preprocessing as done prior to modelling 
  #-------------------------------------------------------------------------------

  set.seed(123)
  st <- Sys.time()
  load(shap_state_files)
  
  cat(file = catfile, append = TRUE , paste0("load done for ", state_names, " \n"))
  

  census_data <- census_data[!is.na(census_data$crs),]
  census_data <- census_data[sample(nrow(census_data)),]
  census_data$crs_raw <- round(census_data$crs,0)
  
  #census_data <- census_data[,-which(grepl( "*concept_house_heating_fuel$" , names( census_data ))) ]
  
  if(isFALSE(bin)){
  census_data$crs <- round(census_data$crs,0)
  census_data <- census_data[-which(census_data$crs == 0),]
  census_data$crs <- ifelse(census_data$crs > 0 & census_data$crs <= 3 , 0, census_data$crs)
  census_data$crs <- ifelse(census_data$crs > 3 & census_data$crs <= 6 , 1, census_data$crs)
  census_data$crs <- ifelse(census_data$crs > 6 , 2, census_data$crs)
  census_data$crs <- as.factor(census_data$crs)}else{
    census_data$crs <- round(census_data$crs,0)
    census_data$crs <- ifelse(census_data$crs == 0, 0,1)
    census_data$crs <- as.factor(census_data$crs)
  }

  
  cat(file = catfile, append = TRUE , paste0("factor done for ", state_names, " \n"))
  
  
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
  
  cat(file = catfile, append = TRUE , paste0("select done for ", state_names, " \n"))
  

  names_dict <- read.csv(paste0(out_path,"/names_dict_", state_names,".csv"))
  
  cat(file = catfile, append = TRUE , paste0("names done for ", state_names, " \n"))
  
  set.seed(123)
  data_split <- initial_split(census_data)
  
  train <- training(data_split)
  test <- testing(data_split)
  
  train_geom <- st_geometry(train)
  test_geom <- st_geometry(test)
  
  train <- st_drop_geometry(train)
  test <- st_drop_geometry(test)
  
  cat(file = catfile, append = TRUE , paste0("split done for ", state_names, " \n"))
  
  mod_rec <- recipe(crs ~ ., data = train) %>%
    add_role(geoid, new_role = "dont_use")
  
  cat(file = catfile, append = TRUE , paste0("recipe done for ", state_names, " \n"))
  
  #-------------------------------------------------------------------------------
  # Load model
  #-------------------------------------------------------------------------------
  
  model = lgb.load( paste0(out_path,state_names,"_lightgbm.model"))
  parsed_model <- jsonlite::fromJSON(
    model$dump_model()
  )
  
  cat(file = catfile, append = TRUE , paste0("model load done for ", state_names, " \n"))
  
  #-------------------------------------------------------------------------------
  # Apply recipe
  #-------------------------------------------------------------------------------
  
  X <- prep(mod_rec, train) %>%
    juice() %>%
    select(-crs, -crs_raw) %>% 
    as.data.frame() %>% 
    data.matrix()
  
  X_test <- prep(mod_rec, test) %>%
    juice() %>%
    select(-crs, -crs_raw) %>% 
    as.data.frame() %>% 
    data.matrix()
  
  cat(file = catfile, append = TRUE , paste0("juice done for ", state_names, " \n"))
  
  #-------------------------------------------------------------------------------
  # Match group names
  #-------------------------------------------------------------------------------
  
  hierachy <- read.csv(groupfile, header = FALSE, sep = ";", skip = 2)
  hierachy = hierachy[-which(!(hierachy$V1 %in% names_dict$name)),]
  hierachy$id <- names_dict$id[which(hierachy$V1 %in% names_dict$name)]
  
  groups_var <- hierachy[match( hierachy$id, names(train)),]
  
  hierachy2 <- read.csv(groupfile2, header = FALSE, sep = ";", skip = 2)
  hierachy2 = hierachy2[-which(!(hierachy2$V1 %in% names_dict$name)),]
  hierachy2$id <- names_dict$id[which(hierachy2$V1 %in% names_dict$name)]
  
  groups_var2 <- hierachy2[match( hierachy2$id, names(train)),]
  
  cat(file = catfile, append = TRUE , paste0("read hierachy done for ", state_names, " \n"))

  #-------------------------------------------------------------------------------
  # prediction & confusion matrix
  #-------------------------------------------------------------------------------

  conf_mat = FALSE
  if(conf_mat == TRUE){
    #prediction & confusion matrix
    p = predict(model,X, reshape = TRUE)
    xgb.pred = as.data.frame(p)
    
    if(bin == TRUE){xgb.pred$prediction = as.factor(ifelse(p>0.5,1,0))
                    xgb.pred$label = train$crs}else{
    colnames(xgb.pred) = levels(train$crs)
    xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
    xgb.pred$label = as.character(train$crs)}
    
    confusion = FALSE
    if(isTRUE(confusion)){
      xgb.pred$raw = round(train$crs_raw,0)

      # The mtcars dataset:
      data <- as.matrix(t(table(as.numeric(xgb.pred$prediction), as.numeric(xgb.pred$raw))))

      # Default Heatmap
      png(paste0(out_path,"plots/misclass",state_names, ".png"), width = 6, height = 4, units = "in", res = 250)
      colSide <- c(rep("blue", 3), rep("dodgerblue",3), rep("lightblue",3))
      heatmap(data, Colv = NA, Rowv = NA, scale="column" , RowSideColors=colSide)
      dev.off()
      }
    
    # Calculate the final accuracy
    result = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
    cat(file = catfile, append = TRUE ,paste("TRAIN Final Accuracy =",sprintf("%1.2f%%", 100*result), "\n"))
    
    cm = confusionMatrix(as.factor(xgb.pred$prediction), as.factor(xgb.pred$label))
    
    # The confusion matrix from a single assessment set (i.e. fold)
    cm2 <- conf_mat(xgb.pred, label, prediction)
    
    autoplot(cm2, type = "heatmap") +
      scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
      ggtitle("Train")+
      labs(caption = paste("Train Final Accuracy =",sprintf("%1.2f%%", 100*result), "\n"))
    ggsave(paste0(out_path,"plots/conf_mat_train",state_names, ".png"), width = 4, height = 4)
    
    #prediction & confusion matrix
    p = predict(model,X_test, reshape = TRUE)
    xgb.pred = as.data.frame(p)

    if(bin == TRUE){xgb.pred$prediction = as.factor(ifelse(p>0.5,1,0))
    xgb.pred$label = test$crs}else{
      colnames(xgb.pred) = levels(test$crs)
      xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
      xgb.pred$label = as.character(test$crs)}
    
    # Calculate the final accuracy
    result = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
    cat(file = catfile, append = TRUE ,paste("TEST Final Accuracy =",sprintf("%1.2f%%", 100*result), "\n"))
    
    cm = confusionMatrix(as.factor(xgb.pred$prediction), as.factor(xgb.pred$label))
    
    # The confusion matrix from a single assessment set (i.e. fold)
    cm2 <- conf_mat(xgb.pred, label, prediction)
    
    autoplot(cm2, type = "heatmap") +
      scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
      ggtitle("test")+
      labs(caption = paste("TEST Final Accuracy =",sprintf("%1.2f%%", 100*result), "\n"))
    ggsave(paste0(out_path,"plots/conf_mat_test",state_names, ".png"), width = 4, height = 4)
  }
  
  

  #-------------------------------------------------------------------------------
  # SHAP
  #-------------------------------------------------------------------------------
  
  nclass = length(levels(train$crs))
  n_vars = ncol(train %>% select(-crs, -crs_raw))
  
  shap_values <- predict(model,X,predcontrib = TRUE)
  
  base_1 = shap_values[,399]
  class_shap = data.frame(shap_values[,1:n_vars])
  names(class_shap) = names(train %>% select(-crs, -crs_raw))
  
#-------------------------------------------------------------------------------
# per group shap
#-------------------------------------------------------------------------------
  
  class_shap$class = train$crs
  
  class_shap_grouped = class_shap %>% 
    gather(feature, shap_value, -class) %>% 
    filter(feature != "geoid") %>% 
    dplyr::group_by(feature) %>% 
    dplyr::mutate(weight = 1/n()) %>% 
    ungroup()
  
  class_shap_grouped$h1 = hierachy$V2[match(class_shap_grouped$feature, hierachy$V1)]
  class_shap_grouped$h2 = hierachy$V3[match(class_shap_grouped$feature, hierachy$V1)]
  class_shap_grouped$h3 = hierachy2$V4[match(class_shap_grouped$feature, hierachy2$V1)]
  
  #sf_shap <- cbind(train_geom, as.data.frame(class_shap))

  #edit(shap.plot.summary)
  
  boxplot_2 = ggplot(class_shap_grouped, aes(x=h3, y=shap_value, fill=h3)) + 
    geom_boxplot() + 
    facet_wrap(~class)+
    scale_fill_viridis(option = "mako",discrete=TRUE)+
    theme_few()+
    theme(legend.position="none")+
    coord_flip()
  
  ggsave(paste0(out_path,"plots/model_boxplot_importance_coarse",state_names, ".png"), boxplot_2 ,width = 10, height = 20) 
  
  #edit(shap.plot.summary)
  
  class_shap_mean = class_shap_grouped %>%
    dplyr::group_by(h3, class) %>%
    #dplyr::summarise(across(everything(), list(mean))) %>%
    dplyr::summarise(across(where(is.numeric),  list( mean = ~mean(., na.rm = TRUE),
                                                      sd = ~sd(., na.rm = TRUE),
                                                      weightedsum = ~ sum(abs(.) * weight)))) %>%
    ungroup() %>%
    #select(-feature_1,-h1_1,-h3_1) %>%
    drop_na()
  
  sum_1 = ggplot(class_shap_mean, aes(x = h3, y = shap_value_weightedsum, fill =class)) +
    geom_bar(stat = 'identity') +
    scale_x_discrete(limits = rev)+
    #facet_wrap(~class)+
    scale_fill_viridis(option = "mako", discrete = TRUE)+
    labs(title ="Weighted sum of SHAP values per group", caption = "aggregate US model") +
    theme(legend.position="bottom",
          plot.title = element_text(color="grey20",hjust=0.5,vjust=1, size=rel(1)),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks.y = element_blank(),
          panel.border = element_blank(), axis.line = element_blank())+
    coord_flip()+
    scale_y_continuous(expand = c(0, 0))
  
  sum_2 = ggplot(class_shap_mean, aes(x = h3, y = shap_value_mean, fill =class)) +
    geom_bar(stat = 'identity') +
    scale_x_discrete(limits = rev)+
    #facet_wrap(~class)+
    scale_fill_viridis(option = "mako", discrete = TRUE)+
    labs(title ="Mean SHAP value per feature group", caption = "aggregate US model") +
    theme(legend.position="bottom",
          plot.title = element_text(color="grey20",hjust=0.5,vjust=1, size=rel(1)),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks.y = element_blank(),
          panel.border = element_blank(), axis.line = element_blank())+
    coord_flip()+
    scale_y_continuous(expand = c(0, 0))
  
  pa33 = ggarrange(sum_1, sum_2, ncol = 2, labels = c("A", "B"), align = "h", widths = c(0.5,0.5))    
  ggsave(paste0(out_path,"plots/model_sum_facet",state_names, ".png"), pa33 ,width = 15, height = 10) 
  
  class_shap = data.frame(shap_values[,1:n_vars])
  names(class_shap) = names(train %>% select(-crs, -crs_raw))

  
  shap_values_long <- shap.prep(xgb_model = model,shap_contrib = class_shap, X_train = X)
  shap_values_long$variable <- hierachy2$V4[match(shap_values_long$variable, hierachy2$V1)]
  shap_values_long$variable[which(shap_values_long$variable == "low vlaue")] = "low value"
  
  select_top_n = shap_values_long %>% 
    dplyr::group_by(variable) %>%
    dplyr::summarise(mean_value = mean(abs(value), na.rm =TRUE)) %>% 
    dplyr::slice_max(mean_value, prop = .5)
  
  shap_values_long_2 = shap_values_long %>% 
    filter(variable %in% select_top_n$variable) %>% 
    filter(variable != "") %>% 
    mutate(variable = as.factor(variable)) %>% 
    drop_na() %>% 
    dplyr::arrange(value)
  
  shp3 = shap_reorder(shap_values_long_2, dilute = 10)
  ggsave(paste0(out_path,"plots/shap12",state_names, ".png"),shp3, width = 12, height = 8)
  
  shap_values_long_2$variable = factor(shap_values_long_2$variable, levels = unique(class_shap_mean2$h3))
  
  
  shp = shap.plot.summary(shap_values_long_2, dilute = 2) + ggtitle(paste("SHAP values for CRS"))+
    scale_color_gradient(low = "blue", high = "red", 
                         breaks = c(0, 1), labels = c(" Low", "High "), guide = guide_colorbar(barwidth = 12, 
                                                                                               barheight = 0.3))+
    theme(axis.text.y = element_text(size = 12))


  class_shap_mean2 = class_shap_mean %>% 
    filter(h3 %in% select_top_n$variable) %>% 
    filter(h3 != "") %>% 
    dplyr::arrange(desc(shap_value_weightedsum))
                   
  sum_11 = ggplot(class_shap_mean2, aes(x = reorder(h3, shap_value_weightedsum,median), y = shap_value_weightedsum, fill =class)) +
    geom_bar(stat = 'identity') +
    #scale_x_discrete(limits = rev)+
    #facet_wrap(~class)+
    scale_fill_viridis(option = "mako", discrete = TRUE)+
    labs(title ="Weighted sum of SHAP values per group", caption = "aggregate US model") +
    theme(legend.position="bottom",
          plot.title = element_text(color="grey20",hjust=0.5,vjust=1, size=rel(1)),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks.y = element_blank(),
          panel.border = element_blank(), axis.line = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          plot.margin = margin(t = 0,  # Top margin
                               r = 0,  # Right margin
                               b = 0,  # Bottom margin
                               l = 0))+
    coord_flip()+
    scale_y_continuous(expand = c(0, 0))
  
  shp2 = ggarrange(shp,sum_11, ncol = 2, align = "h", widths = c(0.8,0.2))
  ggsave(paste0(out_path,"plots/shap11",state_names, ".png"),shp2, width = 12, height = 8)
  
  png(paste0(out_path,"/plots/", state_names,"white_map",".png"), width = 5, height = 4, units = "in", res = 150)
  plot(census_data["estimate_total_white_alone_concept_race"], border = NA)
  dev.off()

  
  cat(file = catfile, append = TRUE , paste0("shap done for ", state_names, " \n"))
  
  n_vars = n_vars+1
  
  class_shap = data.frame(shap_values[,1:n_vars])
  names(class_shap) = c(names(train %>% select(-crs, -crs_raw)), "base_val")
  class_shap$class = 0
  x = n_vars+1
  y = n_vars*2
  class_shap_2 = data.frame(shap_values[,x:y])
  names(class_shap_2) = c(names(train %>% select(-crs, -crs_raw)), "base_val")
  class_shap_2$class = 1
  class_shap = rbind(class_shap, class_shap_2)
  x = n_vars*2+1
  y = n_vars*3
  class_shap_2 = data.frame(shap_values[,x:y])
  names(class_shap_2) = c(names(train %>% select(-crs, -crs_raw)), "base_val")
  class_shap_2$class = 2
  class_shap = rbind(class_shap, class_shap_2)
  
  # save(file = paste0(out_path,"/train_shap_", state_names,".RData"), class_shap)
  
  #-------------------------------------------------------------------------------
  # SHAP plots
  #-------------------------------------------------------------------------------
  
  # class_shap_mean = class_shap %>% 
  #                       dplyr::group_by(class) %>% 
  #                       dplyr::summarise(across(everything(), list(mean)))
  
  class_shap_grouped = class_shap %>% 
          gather(feature, shap_value, -c(class)) %>% 
          filter(feature != "geoid") %>% 
          dplyr::group_by(feature) %>% 
          dplyr::mutate(weight = 1/n()) %>% 
          ungroup()

  class_shap_grouped$h1 = hierachy$V2[match(class_shap_grouped$feature, hierachy$V1)]
  class_shap_grouped$h2 = hierachy$V3[match(class_shap_grouped$feature, hierachy$V1)]
  class_shap_grouped$h3 = hierachy2$V4[match(class_shap_grouped$feature, hierachy2$V1)]
  
  class_shap_mean = class_shap_grouped %>% 
    dplyr::group_by(class, h1) %>% 
    #dplyr::summarise(across(everything(), list(mean))) %>% 
    dplyr::summarise(across(where(is.numeric),  list( mean = ~mean(., na.rm = TRUE),
                                                      sd = ~sd(., na.rm = TRUE),
                                                      weightedsum = ~ sum(abs(.) * weight)))) %>% 
    ungroup() %>% 
    #select(-feature_1,-h1_1,-h3_1) %>% 
    drop_na()

  pa <-ggplot(class_shap_mean, aes(h1,reorder(class, shap_value_weightedsum, median, order=TRUE), fill = shap_value_weightedsum)) + 
    geom_tile(colour="white", size=.6) + 
    scale_fill_gradient(low ="#fff2cc", high = '#B20000', space = 'Lab', na.value = "white") +
    xlab("") + 
    ylab("") +
    #labs(caption = paste( var_dri ,"->",  var_pro, "->",var_impact)) +
    scale_x_discrete(limits = rev, position = "top")+
    scale_y_discrete(limits = rev)+
    #coord_equal()+
    theme(
      #plot.title = element_text(hjust=-1,vjust=1),
      panel.grid.major = element_blank(),
      panel.background = element_rect(fill="white"),
      panel.grid.minor = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(), 
      axis.text.x = element_text(angle = 270, hjust = 1),#, vjust = 0.5), #, hjust=1
      axis.text = element_text(color="grey20", size=rel(1)),
      #axis.text.y  = element_text(hjust=1),
      #axis.text.y  = element_blank(),
      legend.text = element_text(color="grey20", size=rel(1)),
      legend.position = "left",
      #legend.position = "none",
      legend.title = element_text( angle = 270, hjust = 1),
      plot.margin = margin(0, 0, 0, 0),
      plot.caption = element_text(size = rel(1.2))
    )
  
  
  for (group in c("h1", "h2", "h3")){  
  
  gp_col = sym(group)
  
  class_shap_mean = class_shap_grouped %>% 
    dplyr::group_by(class, !!gp_col) %>% 
    #dplyr::summarise(across(everything(), list(mean))) %>% 
    dplyr::summarise(across(where(is.numeric),  list( mean = ~mean(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE),
        weightedsum = ~ sum(abs(.) * weight)))) %>% 
    ungroup() %>% 
    #select(-feature_1,-h1_1,-h3_1) %>% 
    drop_na()
  

  theme_set(
    theme_classic(base_size = 12)
  )
  
  internal = ggplot(class_shap_mean, aes(x = reorder(!!gp_col, abs(shap_value_mean), median, order=TRUE), y = shap_value_mean, fill = as.factor(class))) + 
    geom_bar(stat = 'identity') + 
    #facet_wrap(~ importance)+
   # scale_x_discrete(limits = rev)+
    scale_fill_viridis(option = "mako",discrete=TRUE)+
    labs(title ="     Feature Importances", caption = "aggregate US model") +
    theme(#legend.position="none",
          plot.title = element_text(color="grey20",hjust=0.5,vjust=1, size=rel(1)),
          axis.text.x = element_text(angle = 45, hjust = 1),
          #axis.text.y = element_blank(),
          #axis.title.x = element_blank(),
          #axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.border = element_blank(), axis.line = element_blank())+
    coord_flip()+ 
    scale_y_continuous(expand = c(0, 0))
  ggsave(paste0(out_path,"plots/SHAP_mean", group,state_names, ".png"), width = 6, height = 4)
  
  
  internal = ggplot(class_shap_mean, aes(x = reorder(!!gp_col, abs(shap_value_weightedsum), median, order=TRUE), y = shap_value_weightedsum, fill = as.factor(class))) + 
    geom_bar(stat = 'identity') + 
    #facet_wrap(~ importance)+
    # scale_x_discrete(limits = rev)+
    scale_fill_viridis(option = "mako",discrete=TRUE)+
    labs(title ="     Feature Importances", caption = "aggregate US model") +
    theme(#legend.position="none",
      plot.title = element_text(color="grey20",hjust=0.5,vjust=1, size=rel(1)),
      axis.text.x = element_text(angle = 45, hjust = 1),
      #axis.text.y = element_blank(),
      #axis.title.x = element_blank(),
      #axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.border = element_blank(), axis.line = element_blank())+
    coord_flip()+ 
    scale_y_continuous(expand = c(0, 0))
  ggsave(paste0(out_path,"plots/SHAP_sum",group,state_names, ".png"), width = 6, height = 4)
  
}
  #-------------------------------------------------------------------------------
  # Initialize
  #-------------------------------------------------------------------------------
  
  theme_set(
    theme_classic(base_size = 12)
  )
  
  i = 0
  j = 0
  
  class_shap = data.frame(shap_values[,1:n_vars])
  names(class_shap) = names(train %>% select(-crs, -crs_raw))
  
  shap_values_long <- shap.prep(xgb_model = model,shap_contrib = class_shap, X_train = X)
  
  shap_values_long_3 <- shap.prep(xgb_model = model,shap_contrib = class_shap, X_train = X, top_n = 10)
  shp = shap.plot.summary(shap_values_long_3, dilute = 50) + ggtitle(paste("SHAP values for CRS", levels(census_data$crs)[j+1]))
  ggsave(paste0(out_path,"plots/shap1",j,state_names, ".png"), width = 15, height = 6)
  
  shap_values_long_2 = shap_values_long
  
  shap_values_long_2$variable <- as.factor(hierachy$V2[match(shap_values_long_2$variable, hierachy$V1)])

  shp = shap.plot.summary(shap_values_long_2, dilute = 50) + ggtitle(paste("SHAP values for CRS", levels(census_data$crs)[j+1]))
  ggsave(paste0(out_path,"plots/shap2",j,state_names, ".png"),shp, width = 10, height = 6)
  
  shap_values_long_2 = shap_values_long
  
  shap_values_long_2$variable <- as.factor(hierachy$V3[match(shap_values_long_2$variable, hierachy$V1)])
  
  shap = shap.plot.summary(shap_values_long_2, dilute = 50) + ggtitle(paste("SHAP values for CRS", levels(census_data$crs)[j+1]))
  ggsave(paste0(out_path,"plots/shap3",j,state_names, ".png"), width = 10, height = 6)
  
  shap_values_long_2 = shap_values_long
  
  shap_values_long_2$variable <- as.factor(hierachy2$V4[match(shap_values_long_2$variable, hierachy2$V1)])
  
  shp = shap.plot.summary(shap_values_long_2, dilute = 50) + ggtitle(paste("SHAP values for CRS", levels(census_data$crs)[j+1]))
  ggsave(paste0(out_path,"plots/shap4",j,state_names, ".png"),shp, width = 10, height = 10)
  

  #-------------------------------------------------------------------------------
  # ADD GROUPS
  #-------------------------------------------------------------------------------
if(is.TRUE(bin)){
  
  names(class_shap) = names(train%>%
                              select(-crs, -crs_raw))
  p = predict(model,X, reshape = TRUE)
  p = as.factor(ifelse(p>0.5,1,0))
  
  class_1 = which(p == 1)
  class_shap_1 =as.data.frame(class_shap[class_1,])
  
  class_0 = which(p == 0)
  class_shap_0 = class_shap[class_0,]
  
  shap_values_long_1 <- shap.prep(xgb_model = model,shap_contrib = class_shap_1, X_train = X[class_1,], top_n = 10)
  shp = shap.plot.summary(shap_values_long_3, dilute = 20) + ggtitle(paste("SHAP values for CRS", levels(census_data$crs)[count+1]))
  ggsave(paste0(out_path,"plots/shap1",count,state_names, ".png"),shp, width = 15, height = 6)
  
  shap_values_long_1 <- shap.prep(xgb_model = model,shap_contrib = class_shap_1, X_train = X[class_1,], top_n = 10)
  shp = shap.plot.summary(shap_values_long_3, dilute = 20) + ggtitle(paste("SHAP values for CRS", levels(census_data$crs)[count+1]))
  ggsave(paste0(out_path,"plots/shap1",count,state_names, ".png"),shp, width = 15, height = 6)
  
  }

if(isFALSE(bin)){  
  count = 1
  i = 1
  
  while(count <= length(levels(census_data$crs))-1){
    
    j = i+n_vars-1
    
    class_shap = data.frame(shap_values[,i:j])
    
    names(class_shap) = names(train%>%
                                select(-crs, -crs_raw))
    
    shap_values_long <- shap.prep(xgb_model = model,shap_contrib = class_shap, X_train = X)
    
    shap_values_long_3 <- shap.prep(xgb_model = model,shap_contrib = class_shap, X_train = X, top_n = 10)
    shp = shap.plot.summary(shap_values_long_3, dilute = 50) + ggtitle(paste("SHAP values for CRS", levels(census_data$crs)[count+1]))
    ggsave(paste0(out_path,"plots/shap1",count,state_names, ".png"),shp, width = 15, height = 6)
    
    shap_values_long_2 = shap_values_long
    
    shap_values_long_2$variable <- as.factor(hierachy$V2[match(shap_values_long_2$variable, hierachy$V1)])
    
    shp = shap.plot.summary(shap_values_long_2, dilute = 50) + ggtitle(paste("SHAP values for CRS", levels(census_data$crs)[count+1]))
    ggsave(paste0(out_path,"plots/shap2",count,state_names, ".png"), width = 10, height = 6)
    
    shap_values_long_2 = shap_values_long
    
    shap_values_long_2$variable <- as.factor(hierachy$V3[match(shap_values_long_2$variable, hierachy$V1)])
    
    shp = shap.plot.summary(shap_values_long_2, dilute = 50) + ggtitle(paste("SHAP values for CRS", levels(census_data$crs)[count+1]))
    ggsave(paste0(out_path,"plots/shap3",count,state_names, ".png"),shp, width = 10, height = 6)
    
    shap_values_long_2 = shap_values_long
    
    shap_values_long_2$variable <- as.factor(hierachy2$V4[match(shap_values_long_2$variable, hierachy2$V1)])
    
    shp = shap.plot.summary(shap_values_long_2, dilute = 50) + ggtitle(paste("SHAP values for CRS", levels(census_data$crs)[count+1]))
    ggsave(paste0(out_path,"plots/shap4",count,state_names, ".png"), width = 10, height = 10)
    
    i = i+n_vars
    count = count +1
    
  }
  
  }
}
  
  

library(slurmR)

# Run parallel computation
Slurm_Map(shap_lightgbm_census,state_names = state_names, tmp_path = "/home/nadja.veigel/tmp", plan = "none", njobs = 1, mc.cores = 10) 
  
  