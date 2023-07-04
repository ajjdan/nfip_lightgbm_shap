library(parallel)
library(lightgbm)
#library(treesnip)
library(tidyverse)
library(jsonlite)
library(plyr)
library(tidymodels)
library(ggplot2)
library(sf)
library(SHAPforxgboost)

library(ggpubr)

library(scico)

path_to_model = "C:/Users/veigel/Documents/projects/NFIP_review/interaction/model_plain/all_lightgbm.model"

shap_state_files = "C:/Users/veigel/Documents/projects/NFIP_review/interaction/acs_tract_rds_with_policy_count/TOT/all_census_ratio_SHAP.Rdata"

set.seed(123)
st <- Sys.time()
census_data <- load(shap_state_files)
census_data <- tracts_save
rm(tracts_save)

census_data <- st_drop_geometry(census_data)
census_data$ratio[!is.finite(census_data$ratio)] = NA
census_data <- census_data[!census_data$ratio > 1,]

census_data <- census_data[sample(nrow(census_data)),]

census_data <- census_data %>%
  select(-pt_count, 
         -estimate_total_concept_total_population.x, 
         -area, 
         -estimate_total_concept_unweighted_sample_count_of_the_population,
         -estimate_total_concept_unweighted_sample_housing_units,
         -estimate_total_concept_total_population.y)

names_dict <- data.frame("id" = paste0("C", as.character(1:ncol(census_data))), name = names(census_data))
names_dict$id <- as.character(names_dict$id)
names_dict$id[which(names_dict$name == "ratio")] <- "ratio"
names_dict$id[which(names_dict$name == "geoid")] <- "geoid"
names(census_data) <- names_dict$id


data_split <- initial_split(census_data)

train_dat <- training(data_split)
test_dat <- testing(data_split)

mod_rec <- recipe(ratio ~ ., data = train_dat) %>%
  add_role(geoid, new_role = "dont_use")

#cat(file = catfile, append = TRUE , paste0("split done for ", state_names, " \n"))

#-------------------------------------------------------------------------------
# Load model
#-------------------------------------------------------------------------------

model = lgb.load(path_to_model)
parsed_model <- jsonlite::fromJSON(
  model$dump_model()
)

#-------------------------------------------------------------------------------
# Apply recipe
#-------------------------------------------------------------------------------

X <- prep(mod_rec, train_dat) %>%
  juice() %>%
  #select(-ratio) %>% 
  as.data.frame() %>% 
  data.matrix()

X_test <- prep(mod_rec, test_dat) %>%
  juice() %>%
  #select(-ratio) %>% 
  as.data.frame() %>% 
  data.matrix()

#-------------------------------------------------------------------------------
# Apply recipe
#-------------------------------------------------------------------------------

# shap.prep() returns the long-format SHAP data from either model or
shap_long <- shap.prep(xgb_model = model, X_train = X)

shap_long_plot1 = shap_long %>% 
  filter(variable == "C3" | variable == "C395") #%>% 
  #mutate(rfvalue = log10(rfvalue +1 ))

shap_long_plot2 = shap_long %>% 
  filter(variable == "C2" | variable == "C395") #%>% 
  #mutate(rfvalue = log10(rfvalue +1))

library(ggthemes)
theme_set(theme_few(base_size = 25))

x = shap.plot.dependence(data_long = shap_long_plot1, x="C3",
                     y = "C3", color_feature = "C395", smooth = FALSE)
  
x = x+
  ylab("SHAP value for \n max people affected per flood \n [policies/household unit]")+
  xlab("max people affected per flood \n [n_people/flood]")+
  scale_colour_stepsn(name = "CRS class", colours = scico(10, palette = "berlin"), breaks = 1:11)+
  theme(legend.position = "top", text = element_text(size = 18), legend.text=element_text(size=23), legend.title=element_text(size=23), legend.key.width= unit(2, 'cm'))

ggsave("C:/Users/veigel/Documents/projects/NFIP_review/interaction/interact_1.png", width = 10, height = 5)

y = shap.plot.dependence(data_long = shap_long_plot2, x="C2",
                         y = "C2", color_feature = "C395", smooth = FALSE)
y = y+
    ylab("SHAP value for \n mean floods per year \n [policies/household unit]")+
    xlab("mean floods per year \n [n_floods/year]")+
  scale_colour_stepsn(name = "CRS class", colours = scico(10, palette = "berlin"), breaks = 1:11)+
  theme(legend.position = "none", text = element_text(size = 18))


ggsave("C:/Users/veigel/Documents/projects/NFIP_review/interaction/interact_2.png", width = 10, height = 5)

#-------------------------------------------------------------------------------
# Apply recipe
#-------------------------------------------------------------------------------

shap_long_plot3 = shap_long %>% 
  filter(variable == "C396" | variable == "C395") #%>% 
#mutate(rfvalue = log10(rfvalue +1 ))

shap_long_plot4 = shap_long %>% 
  filter(variable == "C397" | variable == "C395") #%>% 
#mutate(rfvalue = log10(rfvalue +1))

x1 = shap.plot.dependence(data_long = shap_long_plot3, x="C396",
                         y = "C396", color_feature = "C395", smooth = FALSE)

x1 = x1+
  ylab("SHAP value for \n SFHA area Flood Zone A \n [policies/household unit]")+
  xlab("SFHA area Flood Zone A \n fraction of census tract area \n within flood zone [-]")+
  scale_colour_stepsn(name = "CRS class", colours = scico(10, palette = "berlin"), breaks = 1:11)+
  scale_x_continuous(limits= c(0,1))+
  theme(legend.position = "none", text = element_text(size = 18))

ggsave("C:/Users/veigel/Documents/projects/NFIP_review/interaction/interact_3.png", width = 10, height = 5)

y1 = shap.plot.dependence(data_long = shap_long_plot4, x="C397",
                         y = "C397", color_feature = "C395", smooth = FALSE)
y1 = y1+
  ylab("SHAP value for \n SFHA area Flood Zone V \n [policies/household unit]")+
  xlab("SFHA area Flood Zone V \n fraction of census tract area \n within flood zone [-]")+
  scale_colour_stepsn(name = "CRS class", colours = scico(10, palette = "berlin"), breaks = 1:11)+
  theme(legend.position = "none", text = element_text(size = 18))

legend_x = get_legend(x)

ggsave("C:/Users/veigel/Documents/projects/NFIP_review/interaction/legend.png", legend_x, width = 2, height = 1)

grid = ggarrange(ggarrange(ggarrange(x + theme(legend.position = "none"),y, ncol = 2), ggarrange(x1,y1, ncol = 2), nrow = 2, align = "h"), legend_x, nrow = 2, heights = c(10,1))

ggsave("C:/Users/veigel/Documents/projects/NFIP_review/interaction/interact_grid.png", width = 12, height = 10)

#-------------------------------------------------------------------------------
# Apply recipe
#-------------------------------------------------------------------------------

pred = read.csv("C:/Users/veigel/Documents/projects/NFIP_review/interaction/model_plain/test_train_predictions_all.csv")
pred_train = pred[which(pred$train_test == "train"),]

scatter_dat_train = data.frame("target" = pred_train$ratio, "pred" = pred_train$.pred, "state" = substr(pred_train$geoid,1,2))

pred_test = pred[which(pred$train_test == "test"),]

scatter_dat_test = data.frame("target" = pred_test$ratio, "pred" = pred_test$.pred, "state" = substr(pred_test$geoid,1,2))

scatter_dat_train$resid = scatter_dat_train$target - scatter_dat_train$pred
scatter_dat_test$resid = scatter_dat_test$target - scatter_dat_test$pred

scatter_dat_train$group = "test"
scatter_dat_test$group = "train"

plot_dat = rbind(scatter_dat_train, scatter_dat_test)

library(tidycensus)
library(ggthemes)

data(fips_codes)
fips_codes = fips_codes[,-c(4,5)]
fips_codes = unique(fips_codes)


fips_codes$clim = NA
fips_codes$clim[which(fips_codes$state %in% c('WA', 'OR', 'ID'))] = 'NORTHWEST'
fips_codes$clim[which(fips_codes$state %in% c('NV', 'CA'))] = 'WEST'
fips_codes$clim[which(fips_codes$state %in% c('AZ', 'CO', 'NM', 'UT'))] = 'SOUTHWEST'
fips_codes$clim[which(fips_codes$state %in% c('MT', 'NE', 'ND', 'SD', 'WY'))] = 'NORTHEN ROCKIES AND PLAINS'
fips_codes$clim[which(fips_codes$state %in% c('IA', 'MI', 'MN', 'WI'))] = 'UPPER MIDWEST'
fips_codes$clim[which(fips_codes$state %in% c('IL', 'IN', 'KY', 'MO', 'OH', 'TN', 'WV'))] = 'OHIO VALLEY'
fips_codes$clim[which(fips_codes$state %in% c('AR', 'KS', 'LA', 'MS', 'OK', 'TX'))] = 'SOUTH'
fips_codes$clim[which(fips_codes$state %in% c('AL', 'FL', 'GA', 'NC', 'SC', 'VA'))] = 'SOUTHEAST'
fips_codes$clim[which(fips_codes$state %in% c('CT', 'DE', 'ME', 'MD', 'MA', 'NH', 'NJ', 'NY', 'PA', 'RI', 'VT'))] = 'NORTHEAST' 
fips_codes = fips_codes[,-1]

plot_dat = merge(plot_dat, fips_codes, by.x = "state", by.y = "state_code", all.x = TRUE, all.y = FALSE)


theme_set(theme_few(base_size = 25))
# Bottom Right
z = ggplot(na.omit(plot_dat), aes(x=state_name, y=resid, fill=group)) + 
  geom_boxplot()+#, outlier.shape = NA) +
  theme(legend.position="none") +
  scale_fill_scico_d(palette = "berlin")+
  scale_y_continuous("model residual",limits = quantile(plot_dat$resid, c(0.1, 0.9), na.rm = TRUE))+
  #scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  facet_wrap(~clim, scales = "free", nrow = 3, ncol = 3)+
  coord_flip()+
  theme(axis.line = element_line(), legend.position = "top", panel.border = element_blank(), axis.title.y = element_blank(), panel.grid = element_blank())
#axis.text.x = element_text(angle = 45, vjust = .5, hjust = 0),
ggsave("C:/Users/veigel/Documents/projects/NFIP_review/interaction/box_resid_test_grid.png", width = 20, height = 20)

scatter_dat_train_na = na.omit(scatter_dat_train)
scatter_dat_test_na = na.omit(scatter_dat_test)

scatter_dat_train_na$rsq = as.numeric(NA)
scatter_dat_train_na$rmse = as.numeric(NA)
scatter_dat_train_na$mae = as.numeric(NA)

scatter_dat_test_na$rsq = as.numeric(NA)
scatter_dat_test_na$rmse= as.numeric(NA)
scatter_dat_test_na$mae = as.numeric(NA)

for (state_name in unique(scatter_dat_train_na$state)){
  
  data = scatter_dat_train_na[which(scatter_dat_train_na$state == state_name),]
  
  data$rsq = cor(data$pred, data$target)^2
  data$rmse = sqrt(mean((data$pred-data$target)^2))
  data$mae = mean(abs(data$pred-data$target))
  
  scatter_dat_train_na[which(scatter_dat_train_na$state == state_name),]$rsq = data$rsq
  scatter_dat_train_na[which(scatter_dat_train_na$state == state_name),]$rmse = data$rmse
  scatter_dat_train_na[which(scatter_dat_train_na$state == state_name),]$mae = data$mae
  
}

for (state_name in unique(scatter_dat_test_na$state)){
  
  data = scatter_dat_test_na[which(scatter_dat_test_na$state == state_name),]
  
  data$rsq = cor(data$pred, data$target)^2
  data$rmse = sqrt(mean((data$pred-data$target)^2))
  data$mae = mean(abs(data$pred-data$target))
  
  scatter_dat_test_na[which(scatter_dat_test_na$state == state_name),]$rsq = data$rsq
  scatter_dat_test_na[which(scatter_dat_test_na$state == state_name),]$rmse = data$rmse
  scatter_dat_test_na[which(scatter_dat_test_na$state == state_name),]$mae = data$mae
  
}

rsq_train = cor(scatter_dat_train_na$pred, scatter_dat_train_na$target)^2
rmse_train = sqrt(mean((scatter_dat_train_na$pred-scatter_dat_train_na$target)^2))
mae_train = mean(abs(scatter_dat_train_na$pred-scatter_dat_train_na$target))

rsq_test = cor(scatter_dat_test_na$pred, scatter_dat_test_na$target)^2
rmse_test = sqrt(mean((scatter_dat_test_na$pred-scatter_dat_test_na$target)^2))
mae_test = mean(abs(scatter_dat_test_na$pred-scatter_dat_test_na$target))

eval_train = unique(scatter_dat_train_na[, c(3,8,7,6)])
eval_test = unique(scatter_dat_test_na[, c(3,8,7,6)])

colnames(eval_test) = paste0(colnames(eval_test), "test")
colnames(eval_train) = paste0(colnames(eval_train), "train")

final_df = merge(eval_test, eval_train, by.x = "statetest", by.y = "statetrain")

final_df_2= merge(final_df, fips_codes, by.x = "statetest", by.y = "state_code", all.x = TRUE, all.y = FALSE)

final_df_2 = na.omit(final_df_2) %>%  mutate_if(is.numeric, ~round(.,4))

final_df_2$rsq_diff = final_df_2$rsqtrain-final_df_2$rsqtest
final_df_2 <-final_df_2[order(final_df_2$rsq_diff),][,-9]

library(usmap)


final_df$diff = final_df$rsqtrain - final_df$rsqtest
final_df$fips = final_df$statetest

plot_usmap(data = final_df, values = "diff",  color = "grey30", labels=FALSE) + 
  scale_fill_continuous( low = "white", high = "indianred", 
                         name = "overfitting") + 
  theme(legend.position = "right") + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "Difference between testing and training rsq", caption = "")

ee = ggplot(plot_dat, aes(x = pred, y = resid, colour = group)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

ggsave("C:/Users/veigel/Documents/projects/NFIP_review/interaction/scatter_resid_all.png", width = 10, height = 10)
