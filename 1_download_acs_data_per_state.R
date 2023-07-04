#-------------------------------------------------------------------------------
# Download all variables from the 5 year 2016-2018 community survey
# 1- Get variables
# 2- load tracts for each state
# 3- save locally as geopackage/rds
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Packages
#-------------------------------------------------------------------------------

library(tidycensus)
library(tidyverse)
library(sf)
library(USAboundaries)
library(stringr)

#-------------------------------------------------------------------------------
# Initialize
#-------------------------------------------------------------------------------

# Initiate census API
#census_api_key("b3cb1b4d20d2e3bc0e94f7afd8c32ec5248b6637")

# List of state names and abbreviations to iterate/select 
#usstates <- us_states()
#state_abbr_census <- usstates$state_abb

#-------------------------------------------------------------------------------
# function download and save
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# First attempt: A more or less automated selection
#-------------------------------------------------------------------------------

# get all available variables
#variable_names <- load_variables(2018, "acs5", cache = TRUE)
#mutate(label = str_remove(label, "Estimate")) %>% 
#mutate(label = paste(label, concept, sep = "::"))

#add_male_female <- variable_names[which(variable_names$name == "B01001_026" | variable_names$name == "B01001_002" ),]

# variable_names <- variable_names[!str_detect(str_sub(variable_names$name, 2), "[a-zA-Z]+"),]
# variable_names <- variable_names[!grepl("PEOPLE REPORTING MULTIPLE ANCESTRY", variable_names$concept ),]
# variable_names <- variable_names[!grepl("BY SEX BY", variable_names$concept ),]
# variable_names <- variable_names[!grepl("ALONE", variable_names$concept ),]
# variable_names <- variable_names[!grepl("PEOPLE REPORTING ANCESTRY", variable_names$concept ),]
# variable_names <- variable_names[!grepl("MEDIAN AGE BY NATIVITY AND CITIZENSHIP STATUS BY SEX", variable_names$concept ),]
# variable_names <- variable_names[!grepl("HISPANIC OR LATINO ORIGIN BY SPECIFIC ORIGIN", variable_names$concept ),]
# variable_names <- variable_names[!grepl("BY MEANS OF TRANSPORTATION", variable_names$concept ),]
# variable_names <- variable_names[!grepl("HISPANIC OR LATINO ORIGIN BY RACE", variable_names$concept ),]
# variable_names <- variable_names[!(str_count(variable_names$concept, "BY") >= 2),]
# variable_names <- variable_names[!grepl("PEOPLE REPORTING SINGLE ANCESTRY", variable_names$concept ),]
# variable_names <- variable_names[!grepl("AGE AND NATIVITY", variable_names$concept ),]
# variable_names <- variable_names[!grepl("SELECTED", variable_names$concept ),]
# variable_names <- variable_names[!grepl("BY MARITAL STATUS", variable_names$concept ),]
# 
# 
# variable_names <- variable_names[!grepl("PERIOD OF NATURALIZATION", variable_names$concept ),]
# variable_names <- variable_names[!grepl("PLACE OF BIRTH BY", variable_names$concept ),]
# variable_names <- variable_names[!grepl("MEDIAN AGE BY", variable_names$concept ),]
# variable_names <- variable_names[!grepl("BY MARITAL STATUS", variable_names$concept ),]
# variable_names <- variable_names[!grepl("AGE OF HOUSEHOLDER BY MEALS INCLUDED IN RENT", variable_names$concept ),]
# 
# variable_names <- variable_names[!grepl("AGE BY", variable_names$concept ),]
# variable_names <- variable_names[!grepl("ALLOCATION", variable_names$concept ),]
# variable_names <- variable_names[!grepl("GROUP QUARTERS", variable_names$concept ),]
# variable_names <- variable_names[!grepl("SEX OF", variable_names$concept ),]
# 
# variable_names <- variable_names[!grepl("AGGREGATE GROSS RENT (DOLLARS) BY", variable_names$concept ),]
# variable_names <- variable_names[!grepl("BY", variable_names$concept ),]
# 
# variable_names <- variable_names[!grepl("VETERAN", variable_names$concept ),]
# variable_names <- variable_names[!grepl("FOOD STAMPS", variable_names$concept ),]
# 
# variable_names <- variable_names[!grepl("CIVILIAN EMPLOYED", variable_names$concept ),]
# variable_names <- rbind(variable_names, add_male_female)

# get all available variables
# variable_names <- load_variables(2018, "acs5", cache = TRUE) %>% 
#   mutate(label = str_remove(label, "Estimate")) %>% 
#   mutate(label = paste(label, concept, sep = "::")) %>% 
#   filter(endsWith(name, "002") | endsWith(name, "001") | (str_count(label, "!!") == 1 & !(str_count(label, "\\)" ) >= 1)) & name != "B00002_001" & name != "B00001_001" | name == "B25001_001" | name == "B01003_001") %>% 
#   select(-concept)

#-------------------------------------------------------------------------------
# SECOND ATTEMPT: Very manual selection
# !! This is the current Version
#-------------------------------------------------------------------------------

#  variable_names <- load_variables(2018, "acs5", cache = TRUE)
# 
# # Rent was falsely excluded in the first selection
#  rent_asked <-  variable_names[which(variable_names$concept == "RENT ASKED" ),]
#  variable_names <- variable_names[!grepl("\\(", variable_names$concept),]
# #
# #
#  variable_names <- subset(variable_names, concept !="SEX BY AGE" | name == "B01001_026")
#  variable_names <- variable_names[-c(18:78),]
#  variable_names <- variable_names[-c(18:1015),]
#  variable_names <- subset(variable_names, concept !="RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS BY NATIVITY OF CHILDREN UNDER 18 YEARS IN FAMILIES AND SUBFAMILIES BY LIVING ARRANGEMENTS AND NATIVITY OF PARENTS" | name == "B05010_002")
# #
#  variable_names <- variable_names[(!grepl("*ALLOCATION*", variable_names$concept)| grepl("Median", variable_names$label)), ]
#  variable_names <- variable_names[-c(19:492),]
# #
#  variable_names <- subset(variable_names, !grepl("*GEOGRAPHICAL MOBILITY*", variable_names$concept) | name == "B07403PR_001" | name == "B07403PR_004"  | name == "B07407_016" | name == "B07408_025" )
#  variable_names <- subset(variable_names, !grepl("*MOVERS BETWEEN REGIONS IN THE UNITED STATES*", variable_names$concept) )
# #
#  variable_names <- variable_names[-c(40:75),]
#  variable_names <- variable_names[-c(43:152),]
#  variable_names <- variable_names[-c(49:68),]
#  variable_names <- variable_names[-c(66:799),]
#  variable_names <- variable_names[-c(66:799),]
# # # 
#  variable_names <- variable_names[-c(49:594),]
#  variable_names <- variable_names[-c(68:88),]
# 
#  variable_names <- variable_names[-c(83:879),]
#  variable_names <- variable_names[-c(107:1696),]
#  variable_names <- variable_names[-c(157:768),]
#  variable_names <- variable_names[-c(163:3772),]
# 
#  variable_names <- variable_names[-c(193:201),]
#  variable_names <- variable_names[-c(194:222),]
# #  
# 
#  variable_names <- variable_names[-c(214:283),]
#  variable_names <- variable_names[-c(303:338),]
#  variable_names <- variable_names[-c(315:358),]
# 
#  variable_names <- variable_names[-c(332:406),]
#  variable_names <- variable_names[-c(341:362),]
#  variable_names <- variable_names[-c(363:616),]
#  variable_names <- variable_names[-c(402:466),]
#  variable_names <- variable_names[-c(402:430),]
#  variable_names <- variable_names[-c(420:587),]
# #  
# #  
#  variable_names <- variable_names[-c(436:1338),]
#  variable_names <- variable_names[-c(460:678),]
# 
# 
#  variable_names <- variable_names[-c(468:1366),]
# # # This was excluded after re-examining unique(variable_names$concept)
# 
#  variable_names <- subset(variable_names, !grepl("*RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS BY NATIVITY OF CHILDREN UNDER 18 YEARS IN FAMILIES AND SUBFAMILIES BY LIVING ARRANGEMENTS AND NATIVITY OF PARENTS*", variable_names$concept) )
# 
#  variable_names <- subset(variable_names, !grepl("*TENURE BY PLUMBING FACILITIES*", variable_names$concept) )
# 
#  variable_names <- subset(variable_names, !grepl("*AGE OF HOUSEHOLDER BY MEALS INCLUDED IN RENT*", variable_names$concept) )
# 
#  variable_names <- subset(variable_names, !grepl("*KITCHEN FACILITIES BY MEALS INCLUDED IN RENT*", variable_names$concept) )
# 
#  variable_names <- subset(variable_names, !grepl("*TENURE BY KITCHEN FACILITIES*", variable_names$concept) )
# 
#  variable_names <- subset(variable_names, !grepl("*TENURE BY PLUMBING FACILITIES*", variable_names$concept) )
# 
#  ###
# 
#  variable_names <- subset(variable_names, !grepl("*TENURE BY FAMILIES AND PRESENCE OF OWN CHILDREN*", variable_names$concept) )
# 
#  variable_names <- subset(variable_names, !grepl("*TENURE BY AGE OF HOUSEHOLDER*", variable_names$concept) )
# 
#  variable_names <- subset(variable_names, !grepl("*TENURE BY FAMILIES AND PRESENCE OF OWN CHILDREN*", variable_names$concept) )
# 
#  variable_names <- subset(variable_names, !grepl("*MORTGAGE STATUS BY AGE OF HOUSEHOLDER*", variable_names$concept) )
# 
#  variable_names <- subset(variable_names, !grepl("*GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY SEX FOR RESIDENCE 1 YEAR AGO IN PUERTO RICO*", variable_names$concept) )
# # 
# # # Include rent again
#  
#  variable_names <- rbind(variable_names, rent_asked)
# # # EXPORT
# # 
# write.csv2(variable_names, "C:/Users/veigel/Documents/GitHub/NFIP/Preprocessing/data/census_data/variable_names/variable_names.txt",row.names = FALSE, quote = FALSE)

# B01003_001 TOTAL POPULATION

options(tigris_use_cache = TRUE)

download_all_acs5_variables <- function(state_abbreviations, path_census_rds = "C:/Users/veigel/Documents/GitHub/NFIP/Preprocessing/data/census_data/acs_tract_rds/", path_to_variable_names = "C:/Users/veigel/Documents/GitHub/NFIP/Preprocessing/data/census_data/variable_names/variable_names.txt"){
  
  census_api_key("")
  
  variable_names <- read.csv2(path_to_variable_names)
  #> Getting data from the 2014-2018 5-year ACS
  # Select only the first layer of hierachial data as described here:
  # https://dcl-wrangle.stanford.edu/census.html
  # https://dcl-wrangle.stanford.edu/images/census/acs-variables.png
  
  census_data <- get_acs(geography = "tract",
                         variables = variable_names$name,
                         state = state_abbreviations,
                         survey = "acs5",
                         year = 2018,
                         geometry = TRUE,
                         cb = FALSE,
                         output = "wide" ) 
  # Output data frame is double the length because estimate (prefix E) and margin of error (M)
  
  census_data <- census_data[,c(1,2,which(endsWith(names(census_data), "E")))]
  names(census_data) <- gsub("E", "", names(census_data))
  
  total_population <- census_data$B01003_001
  
  # Remove duplicates
  census_data <- census_data[!duplicated(as.list(census_data))]
  
  # New column names
  variable_names$label <- paste0(variable_names$label,  "_concept_", variable_names$concept)
  
  # Remove double name column
  #census_data <- census_data[, -3]
  
  names(census_data) <- c("GEOID", "NAME", variable_names$label[match(names(st_drop_geometry(census_data)), variable_names$name)][-c(1, 2)], "geometry")
  
  names(census_data) <- make.names(names(census_data)) %>% 
      str_replace_all(   pattern = "[:punct:]", replacement = "_") %>% 
      str_replace_all(   pattern = "___", replacement = "_") %>% 
      str_replace_all(   pattern = "__", replacement = "_") %>% 
      str_to_lower(locale = "en")
  
  #set geometry for rds
  st_geometry(census_data) <- census_data$geometry %>%
    st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs")
  
  census_data$estimate_total_concept_total_population <- total_population
  
  # Percentages instead of n  for selection ---------------------------------
  
  census_data <- census_data %>%
    mutate_at(vars(-estimate_total_concept_total_population, -geoid, -name, - geometry, -estimate_total_concept_housing_units,
                   -contains("median_age"), -contains("median_number_of_rooms"),-contains("median_gross_rent"),
                   -contains("mean_usual_hours"), -contains("occupancy"), -contains("vacancy"),-contains("gini"),-contains("units_in_structure"),
                   -contains("usual_hours_worked"), -contains("median_year_structure_built")),
              ~ ifelse(. != 0 , ((100/estimate_total_concept_total_population)*.)/100,.)) %>% 
   
    mutate_at(vars( contains("vacancy"), contains("occupancy"), contains("units_in_structure")),
           ~ ifelse(. != 0 , ((100/estimate_total_concept_housing_units)*.)/100,.))

  
  saveRDS(census_data ,paste0(path_census_rds, state_abbreviations ,"/select_acs5_2018_", state_abbreviations, ".rds"))
  

}



 