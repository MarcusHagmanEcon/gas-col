#--------------------------------------------------------------------------
# Script Name: 01_data_cleaning_stations.R
# 
# Author: Marcus Hagman
# Date: 2023-10-18
# 
# Purpose: This script takes in the raw list of gas stations
          # and the cleaned population file, then calculates 
          # values of variables that will be needed in the analysis.
          # For example, it calculates variables that will determine the
          # competitiveness of each station
#
# Input: - 01_data/01_raw/gas_stations.csv
#        - 01_data/02_processed/cleaned_population.rds
# 
# Output: - 01_data/02_processed/cleaned_gas_stations.rds
#
# Instructions: 
#
# Revision History:
#--------------------------------------------------------------------------

rm(list=ls())

setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), "/Dropbox/gas-col"))

library(tidyverse)
library(osrm)
library(sf)

# Load data
gas_stations <- read.csv("01_data/01_raw/gas_stations.csv")
cleaned_population <- readRDS("01_data/02_processed/cleaned_population.rds")

### Remove and edit observations

# Remove not real observation
gas_stations <- gas_stations %>% filter(name != "01_test")

# 16 observations where latitude and longitude are swapped
gas_stations <- gas_stations %>% mutate(lat_lng_swapped = lat > 0 & lat < 20 & lng > 40 & lng < 60) %>%
  mutate(lat_temp = lat) %>%
  mutate(lat = ifelse(lat_lng_swapped, lng,lat)) %>%
  mutate(lng = ifelse(lat_lng_swapped, lat_temp, lng)) %>%
  select( - c(lat_lng_swapped, lat_temp))

# 3 observations with longitudes far outside of Germany
gas_stations <- gas_stations %>% filter(lng < 16)

# There is reason to believe that the coordinates are wrong for the 4 where
#at least one coordinate was entered as an integer
gas_stations <- gas_stations %>% filter(lat != floor(lat) & lng != floor(lng))


### relabel

# Brand "" should be NA
gas_stations <- gas_stations %>% mutate(brand = ifelse(brand == "", NA, brand))

# Remove whitespace from brand and make lowercase
gas_stations$brand <- tolower(gsub("\\s+", "", gas_stations$brand))

### Calculate competition metrics
source("02_R/02_functions/haversine_distance.R")
source("02_R/02_functions/competition_metrics.R")
source("02_R/02_functions/population_within_radius.R")
source("02_R/02_functions/closest_station.R")

population_within_5km <- mapply(population_within_radius,
                                 lat = gas_stations$lat,
                                 lng = gas_stations$lng,
                                 MoreArgs=list(pop_data = cleaned_population,
                                               radius = 5*10^3))
gas_stations$population_within_5km <- population_within_5km
gas_stations$population_within_5km_sq <- population_within_5km^2

population_within_10km <- mapply(population_within_radius,
                                 lat = gas_stations$lat,
                                 lng = gas_stations$lng,
                                 MoreArgs=list(pop_data = cleaned_population,
                                               radius = 10*10^3))
gas_stations$population_within_10km <- population_within_10km
gas_stations$population_within_10km_sq <- population_within_10km^2

population_within_15km <- mapply(population_within_radius,
                                 lat = gas_stations$lat,
                                 lng = gas_stations$lng,
                                 MoreArgs=list(pop_data = cleaned_population,
                                               radius = 15*10^3))
gas_stations$population_within_15km <- population_within_15km
gas_stations$population_within_15km_sq <- population_within_15km^2

competition_metrics_df <- sapply(1:nrow(gas_stations), competition_metrics, df = gas_stations)
gas_stations <- gas_stations %>% cbind(t(competition_metrics_df)) 

# Change type where relevant
gas_stations <- gas_stations %>% 
  mutate(drdur_to_nearest_station = as.numeric(drdur_to_nearest_station),
         drdur_to_2nd_nearest_station = as.numeric(drdur_to_2nd_nearest_station),
         drdur_to_3rd_nearest_station = as.numeric(drdur_to_3rd_nearest_station),
         drdis_to_nearest_station = as.numeric(drdis_to_nearest_station),
         drdis_to_2nd_nearest_station = as.numeric(drdis_to_2nd_nearest_station),
         drdis_to_3rd_nearest_station = as.numeric(drdis_to_3rd_nearest_station),
         phdis_to_nearest_station = as.numeric(phdis_to_nearest_station),
         phdis_to_2nd_nearest_station = as.numeric(phdis_to_2nd_nearest_station),
         phdis_to_3rd_nearest_station = as.numeric(phdis_to_3rd_nearest_station),
         distance_rank_of_nearest_station_drdur = as.numeric(distance_rank_of_nearest_station_drdur),
         distance_rank_of_nearest_station_drdis = as.numeric(distance_rank_of_nearest_station_drdis),
         stations_within_5km = as.numeric(stations_within_5km),
         stations_within_10km = as.numeric(stations_within_10km),
         stations_within_15km = as.numeric(stations_within_15km))

gas_stations <- gas_stations %>% mutate(stations_per_million_pop_5km = 10^6 * as.integer(stations_within_5km) / population_within_5km,
                                        stations_per_million_pop_10km = 10^6 * as.integer(stations_within_10km) / population_within_10km)


### Most common brands

gas_stations <- gas_stations %>% mutate(brand_and_neighbor = paste(brand, brand_of_nearest_station_drdur, sep = "_"))

most_common_brands <- gas_stations %>%
  count(brand, sort = TRUE) %>%
  head(5) %>%
  pull(brand)

gas_stations <- gas_stations %>%
  mutate(brand_and_neighbor_most_common = 
           if_else(brand %in% most_common_brands & brand_of_nearest_station_drdur  %in% 
                     most_common_brands, brand_and_neighbor, ""),
         independent  = ifelse(is.na(brand), TRUE, grepl("bft|frei", brand)),
         same_brand_as_nearest_station_drdur = ifelse(!is.na(brand) & !is.na(brand_of_nearest_station_drdur), brand == brand_of_nearest_station_drdur, FALSE),
         same_brand_as_2nd_nearest_station_drdur = ifelse(!is.na(brand) & !is.na(brand_of_2nd_nearest_station_drdur), brand == brand_of_2nd_nearest_station_drdur, FALSE),
         same_brand_as_3rd_nearest_station_drdur = ifelse(!is.na(brand) & !is.na(brand_of_3rd_nearest_station_drdur), brand == brand_of_3rd_nearest_station_drdur, FALSE),
         same_brand_as_nearest_station_drdis = ifelse(!is.na(brand) & !is.na(brand_of_nearest_station_drdis), brand == brand_of_nearest_station_drdis, FALSE),
         same_brand_as_2nd_nearest_station_drdis = ifelse(!is.na(brand) & !is.na(brand_of_2nd_nearest_station_drdis), brand == brand_of_2nd_nearest_station_drdis, FALSE),
         same_brand_as_3rd_nearest_station_drdis = ifelse(!is.na(brand) & !is.na(brand_of_3rd_nearest_station_drdis), brand == brand_of_3rd_nearest_station_drdis, FALSE),
         same_brand_as_nearest_station_phdis = ifelse(!is.na(brand) & !is.na(brand_of_nearest_station_phdis), brand == brand_of_nearest_station_phdis, FALSE),
         same_brand_as_2nd_nearest_station_phdis = ifelse(!is.na(brand) & !is.na(brand_of_2nd_nearest_station_phdis), brand == brand_of_2nd_nearest_station_phdis, FALSE),
         same_brand_as_3rd_nearest_station_phdis = ifelse(!is.na(brand) & !is.na(brand_of_3rd_nearest_station_phdis), brand == brand_of_3rd_nearest_station_phdis, FALSE),
         diff_drdur_2nd_1st = drdur_to_2nd_nearest_station - drdur_to_nearest_station,
         diff_drdis_2nd_1st = drdis_to_2nd_nearest_station - drdis_to_nearest_station,
         diff_phdis_2nd_1st = phdis_to_2nd_nearest_station - phdis_to_nearest_station,
         less_than_50m_to_neighbor_phdis = phdis_to_nearest_station < 0.05)

saveRDS(gas_stations, file = "01_data/02_processed/cleaned_gas_stations.rds")
