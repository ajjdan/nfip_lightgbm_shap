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


path_to_model = "all_lightgbm.model"

shap_state_files = "all_census_ratio_SHAP.Rdata"

set.seed(123)
st <- Sys.time()
census_data <- load(shap_state_files)
census_data <- tracts_save
rm(tracts_save)

census_data <- st_drop_geometry(census_data)
census_data$ratio[!is.finite(census_data$ratio)] = NA
census_data <- census_data[!census_data$ratio > 1,]

#census_data <- census_data[sample(10000),]

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

cor_dat = census_data %>% select(-ratio) %>% select(where(is.numeric))
cor_dat = do.call(rbind.data.frame, cor_dat)
mat = as.matrix(cor_dat)

library(Hmisc)
#split the samples up randomly
m2 = mat[,seq(1,ncol(mat),2)]
m1 = mat[,sample(ncol(mat),10000)]

test = rcorr(t(m1), type="pearson")

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    #row = rownames(cormat)[row(cormat)[ut]],
    #column = rownames(cormat)[col(cormat)[ut]],
    cor  = cormat[ut],
    p = pmat[ut]
  )
}

cor_df = flattenCorrMatrix(test$r, test$P)

summary(cor_df$cor)
hist(cor_df$cor)

#-------------------------------------------------------------------------------

xcor10 <- cor(cor_dat, use="pairwise.complete.obs")

xcor10[xcor10 == 1] = NA

library(corrplot)


gc(reset = TRUE)



gc(reset = TRUE)

minimum = min(xcor10, na.rm = TRUE)
gc(reset = TRUE)
maximum = max(xcor10, na.rm = TRUE)
gc(reset = TRUE)
median1 = median(xcor10, na.rm = TRUE)
gc(reset = TRUE)
mean2 = mean(xcor10, na.rm = TRUE)
gc(reset = TRUE)
