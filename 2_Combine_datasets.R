#-------------------------------------------------------------------------------
# Combine all data
#
# 2- Eligible Communities
#    /NFIP/Preprocessing/eligible_communities/load_and_update_nfip_communities.R
# 3- Redacted Policies
#   /NFIP/Preprocessing/eligible_communities/
#     3.1 Spread points regularily within anonymization distance
# 4- Flood zones
#   /NFIP/Preprocessing/flood_zones/gdb_to_raster.R
# 5- Census tracts
#   /NFIP/Preprocessing/census_data/download_data_per_state.R
# 6- Calculate Intersect
# 7- Calculate ratio of policies per capita for each tract
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Packages
#-------------------------------------------------------------------------------

library(lubridate) #easy date formatitng
library(tidyverse) #pipes and vectorized functions
library(data.table) #Read large datasets fast

library(tigris) # US Shapefile download
library(USAboundaries) # US Shapefile download
library(tidycensus)

library(sf)# Process shapefiles
library(st)# Process shapefiles

library(raster)# Process rasters

# Plot
library(ggspatial)
library(ggplot2)
library(ggthemes)
library(jsonlite)

# path_feature_selection = "C:/Users/veigel/Documents/GitHub/NFIP/Preprocessing/data/feature_selection"
# shapefile_eligible_communities = "C:/Users/veigel/Documents/GitHub/NFIP/Preprocessing/data/eligible_communities/eligible_communities/community_merge.shp"
# shapefile_policies = "C:/Users/veigel/Documents/GitHub/NFIP/Preprocessing/data/policies/policies.shp"
# path_floodzone_rasters = "C:/Users/veigel/Documents/GitHub/NFIP/Preprocessing/data/flood_zones/floodmaps/preprocessed"
# path_census_rds = "C:/Users/veigel/Documents/GitHub/NFIP/Preprocessing/data/census_data/na_included/acs_tract_rds"
# output_feature_selection = "C:/Users/veigel/Documents/GitHub/NFIP/Preprocessing/data/feature_selection/"
# path_census_rds_with_policies = "C:/Users/veigel/Documents/GitHub/NFIP/Preprocessing/data/census_data/acs_tract_rds_with_policy_count"


#-------------------------------------------------------------------------------
# Initialize
#-------------------------------------------------------------------------------

policies_per_tract <- function(state_names,
                               state_abbr,
                               state_fp,
                               path_feature_selection = "C:/Users/veigel/Documents/GitHub/NFIP/Preprocessing/data/feature_selection" ,
                               shapefile_eligible_communities = "C:/Users/veigel/Documents/GitHub/NFIP/Preprocessing/data/eligible_communities/eligible_communities/community_merge.shp",
                               shapefile_policies = "C:/Users/veigel/Documents/GitHub/NFIP/Preprocessing/data/policies/policies.shp",
                               path_floodzone_rasters = "C:/Users/veigel/Documents/GitHub/NFIP/Preprocessing/data/flood_zones/floodmaps/preprocessed",
                               path_census_rds = "C:/Users/veigel/Documents/GitHub/NFIP/Preprocessing/data/census_data/acs_tract_rds",
                               output_feature_selection = "C:/Users/veigel/Documents/GitHub/NFIP/Preprocessing/data/feature_selection",
                               path_census_rds_with_policies = "C:/Users/veigel/Documents/GitHub/NFIP/Preprocessing/data/census_data/acs_tract_rds_with_policy_count") {
  
  rasterOptions(maxmemory = 1e+09)
  rasterOptions(memfrac = 0.8)
  
  
  usstates <- us_states()
  state_outline <- usstates %>% filter(statefp == state_fp)
  
  #-------------------------------------------------------------------------------
  # 2 communities and eligibility
  # Previously edited according to the 2020 FEMA Community status book in
  #-------------------------------------------------------------------------------
  
  community_sf <- sf::st_read(
    dsn = shapefile_eligible_communities,
    query = paste0(
      "SELECT * FROM community_merge WHERE J_STATE=",
      "\'",
      state_fp ,
      "\'"
    )
  ) %>%
    st_transform("+proj=longlat +datum=WGS84 +no_defs") %>% 
    st_make_valid()
  
  community_sf_yes <-
    community_sf %>%
    filter(El_2020 == "Y") %>% 
    mutate(elig_year = year(elig_dt)) 
  
  #-------------------------------------------------------------------------------
  # 3 Policies Data
  #-------------------------------------------------------------------------------
  
  pol_sf <- sf::st_read(
    dsn = shapefile_policies,
    query = paste0("SELECT * FROM policies WHERE prprtyS=", "\'", state_abbr , "\'")
  ) %>%
    st_set_crs("+proj=longlat +datum=WGS84 +no_defs")  %>%
    st_as_sf() %>%
    st_crop(state_outline) 
  
  pol_sf <- pol_sf %>% 
    group_by(orgnNBD,rprtdZC) %>% 
    slice(which.max(plcyTrD)) %>% 
    ungroup()

  # Overlapping points for visualization -----------------------------------------
  
  pol_sf$X <- st_coordinates(pol_sf)[, 1]
  pol_sf$Y <- st_coordinates(pol_sf)[, 2]
  
  pol_sf_point_size <-
    pol_sf %>%
    group_by(X, Y) %>%
    count() #%>%
  #st_transform(crs = "+proj=longlat +datum=NAD83 +no_defs")
 
  # 3.1 Spread points inside anonymization zone of one decimal degree -----------------

  # Calculate generate uniform random numbers of X and Y within 0.1 degrees
  
  set.seed(123)
  random_offset_y <- runif(n = nrow(pol_sf),
                           min = -0.05,
                           max = 0.05)
  random_offset_x <- runif(n = nrow(pol_sf),
                           min = -0.05,
                           max = 0.05)
  
  # create new geometry column
  pol_sf_dist <-
    data.frame(
      "Y" = st_coordinates(pol_sf)[, 2] + random_offset_y,
      "X" = st_coordinates(pol_sf)[, 1] + random_offset_x
    ) %>%
    st_as_sf(coords = c("X", "Y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  # Overwrite anonymized coordinates and reformat to sf
  pol_sf <-
    st_as_sf(cbind(st_drop_geometry(pol_sf), pol_sf_dist)) %>% 
    mutate(X = pol_sf$X + random_offset_x,
           Y =  pol_sf$Y + random_offset_y) %>%
    st_crop(community_sf_yes)
  
  # duplicate contracts that are for several housing units
  pol_sf <- pol_sf[rep(row.names(pol_sf), pol_sf$plcyCnt), 1:ncol(pol_sf)]
  
  # Uncomment for plot of deanonymization
  # cellsize <- 0.1
  # grd_lrg <- st_make_grid(st_as_sfc(st_bbox(pol_sf_point_size) + c(-cellsize/2, -cellsize/2,cellsize/2, cellsize/2)), what="polygons", cellsize=cellsize)
  # X11()
  # plot(grd_lrg)
  # plot(st_geometry(pol_sf_dist),pch = 20, add = TRUE)
  # plot(st_geometry(pol_sf_point_size), col = "red", pch = 20, add = TRUE)
  
  #-------------------------------------------------------------------------------
  # 4 Flood zone
  # Aggregate surrounding Pixels (?)
  #-------------------------------------------------------------------------------
  
  flood_zone_rasters <-
    list.files(path_floodzone_rasters, full.names = TRUE)
  flood_zone_rasters  <-
    flood_zone_rasters[c(which(grepl(
      paste0("*", state_names , "*"), flood_zone_rasters
    )))] # select names with state
  
  zones <-
    c(
      "A" = 1,
      "A99" = 2,
      "AE" = 3,
      "AH" = 4,
      "AO" = 5,
      "D" = 6,
      "X" = 7,
      "V" = 8 ,
      "VE" = 9 ,
      "AREA NOT INCLUDED" = 10,
      "OPEN WATER" = 11
    )
  
  flood_zone <- raster(flood_zone_rasters[1])
  flood_zone <-
    merge(flood_zone, raster(flood_zone_rasters[2]), tolerance = 0.5)
  flood_zone <-
    merge(flood_zone, raster(flood_zone_rasters[3]), tolerance = 0.5)
  
  # Project from NAD83 to WGS84
  flood_zone <-
    projectRaster(flood_zone, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  policies_zones <- extract(flood_zone, pol_sf)
  policies_zones <-
    factor(policies_zones,
           levels = zones,
           labels = names(zones))
  
  policies_zones <- as.character(policies_zones)
  # Append to points
  pol_sf$FldZn_raster <- policies_zones
  
  
  pol_sf$floodZn_ifelse <-
    ifelse(!is.na(pol_sf$floodZn),
           pol_sf$floodZn,
           pol_sf$FldZn_raster)
  
  cat(
    paste0(
      "NA's in",
      state_names,
      " flood Zone: \n ",
      length(which(is.na(
        pol_sf$floodZn_ifelse
      ))),
      "\n",
      "NA's in policies data: \n ",
      length(which(is.na(pol_sf$floodZn))),
      "\n",
      "NA's in raster data: \n ",
      length(which(is.na(
        pol_sf$FldZn_raster
      )))
    ),
    file = paste0(path_floodzone_rasters, "NA_log_floodzones.txt"),
    append = TRUE,
    sep = "\n"
  )
  
  
  
  #-------------------------------------------------------------------------------
  # 5 Census Tracts
  #-------------------------------------------------------------------------------
  

  census_data <-
    readRDS(
      paste0(
        path_census_rds,
        "/",
        state_abbr  ,
        "/select_acs5_2018_",
        state_abbr,
        ".rds"
      )
    ) 
  
    #set geometry for rds
  st_geometry(census_data) <- census_data$geom %>%
    st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs") %>% 
    st_make_valid()
  
  # Only eligible communities ---------------------------------
  
  census_data <-
    census_data[lengths(st_intersects(census_data, community_sf_yes)) >= 1, -which(startsWith(names(census_data), "na") |
                                                                                     names(census_data) == "geom")]
 
    ##################### policyCount
  
  # Intersection between polygon and points ---------------------------------
  
  points_in_polygon <- st_intersects(census_data, pol_sf)
  census_data$pt_count <- lengths(points_in_polygon)
  
  # Intersection community rating system ---------------------------------
  
  polygon_in_community <- st_intersects(census_data, community_sf)
  
  census_data$crs <- NA
  
  for (i in 1:nrow(polygon_in_community)){
  census_data$crs[i] <- median(community_sf$CRS_CLA[ polygon_in_community[[i]]], na.rm = TRUE)
  }
  
  #length(which(is.na(pol_sf$ddcAICC)))
  #length(which(is.na(pol_sf$ddcAIBC)))

  #-------------------------------------------------------------------------------
  # 6 add percentage of floodzone area
  #-------------------------------------------------------------------------------
  
  area_fluvial_zone_in_census_tract <-
    function(census_data, flood_zone, plot_raster = FALSE) {
      # all values > 5 become 1, etc.
      m <- c(0,5.09, 1,  
             5.1, 7.09, 0,
             7.1,9.09,0,
             9.1,12,0 )
      rclmat <- matrix(m, ncol=3, byrow=TRUE)
      flood_zone_rc <-
        raster::reclassify(flood_zone, rclmat, include.lowest = TRUE)
      
      flood_zone_A <- flood_zone_rc
      flood_zone_A[which(values(flood_zone_rc) == 0)] <- NA
    
      st <- Sys.time()
      flood_zone_rc_sum <-
        raster::extract(
          flood_zone_A,
          census_data,
          fun = sum,
          na.rm = TRUE,
          df = TRUE
        )
      
      et <- Sys.time()
      
      et - st #Time difference of 9.40396 mins    #Time difference of 25.37067 mins
      
      raster_cell_area <- prod(res(flood_zone_rc))
      raster_cell_area <- raster_cell_area * 1000000 #km2 in m2
      
      flood_zone_rc_sum$area_A <- raster_cell_area * flood_zone_rc_sum$layer
      
      census_data$area <-
        st_area(census_data) #/1000000 #Take care of units (m2)
      
      census_data$area_A_percent <-
        ((100 / census_data$area) * flood_zone_rc_sum$area_A) * 100
      
      if (plot_raster == TRUE) {
        plot(st_geometry(census_data)[length(census_data) / 2])
        plot(flood_zone_A, add = TRUE)
        plot(st_geometry(census_data)[length(census_data) / 2], add = TRUE)
      }
      
      return(census_data)
      
    }
  
  area_coastal_zone_in_census_tract <-
    function(census_data, flood_zone, plot_raster = FALSE) {
      # all values > 5 become 1, etc.
      m <- c(0,5.09, 0,  
             5.1, 7.09, 0,
             7.1,9.09,1,
             9.1,12,0 )
      rclmat <- matrix(m, ncol=3, byrow=TRUE)
      flood_zone_rc <-
        raster::reclassify(flood_zone, rclmat, include.lowest = TRUE)
      
      flood_zone_A <- flood_zone_rc
      flood_zone_A[which(values(flood_zone_rc) == 0)] <- NA
      
      st <- Sys.time()
      flood_zone_rc_sum <-
        raster::extract(
          flood_zone_A,
          census_data,
          fun = sum,
          na.rm = TRUE,
          df = TRUE
        )
      
      et <- Sys.time()
      
      et - st #Time difference of 9.40396 mins    #Time difference of 25.37067 mins
      
      raster_cell_area <- prod(res(flood_zone_rc))
      raster_cell_area <- raster_cell_area * 1000000 #km2 in m2
      
      flood_zone_rc_sum$area_V <- raster_cell_area * flood_zone_rc_sum$layer
      
      census_data$area <-
        st_area(census_data) #/1000000 #Take care of units (m2)
      
      census_data$area_V_percent <-
        ((100 / census_data$area) * flood_zone_rc_sum$area_V) * 100
      
      if (plot_raster == TRUE) {
        plot(st_geometry(census_data)[length(census_data) / 2])
        plot(flood_zone_A, add = TRUE)
        plot(st_geometry(census_data)[length(census_data) / 2], add = TRUE)
      }
      
      return(census_data)
      
    }
  
  census_data <-
    area_fluvial_zone_in_census_tract(census_data = census_data,
                                   flood_zone = flood_zone,
                                   plot_raster = FALSE)
  
  census_data <-
    area_coastal_zone_in_census_tract(census_data = census_data,
                                   flood_zone = flood_zone,
                                   plot_raster = FALSE)
  census_ratio <-
    census_data  %>%
    mutate(ratio = pt_count / (estimate_total_concept_total_population)) 
  
  
  #-------------------------------------------------------------------------------
  # 5 Census Tracts
  #-------------------------------------------------------------------------------
  
  census_feature_selection <- st_drop_geometry(census_data)
  
  census_ratio_feature_selection <- st_drop_geometry(census_ratio)
  
  #-------------------------------------------------------------------------------
  # 6 Export
  #-------------------------------------------------------------------------------

  
  exportJSON <- toJSON(census_ratio_feature_selection, force = TRUE)
  write(
    exportJSON,
    paste0(
      output_feature_selection,
      "/",
      state_abbr ,
      "/",
      state_abbr,
      "_census_ratio_feature_selection.json"
    )
  )
  
  saveRDS(
    census_ratio ,
    paste0(
      path_census_rds_with_policies,
      "/",
      state_abbr ,
      "/policy_count_acs5_2018_",
      state_abbr,
      ".rds"
    )
  )
  
  # Open a pdf file
  pdf(paste0(
    path_census_rds_with_policies,
    "/",
    state_abbr ,
    "/tracts_and_communities_acs5_2018_",
    state_abbr,
    ".pdf"
  )) 
  
  plot(st_geometry(community_sf_yes), border = "grey", main = state_names)
  plot(st_geometry(census_data), border = "black", add = TRUE)
  
  dev.off()
  
}

