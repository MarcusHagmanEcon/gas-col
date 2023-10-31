#--------------------------------------------------------------------------
# Script Name: 01_data_cleaning_stations.R
# 
# Author: Marcus Hagman
# Date: 2023-10-18
# 
# Purpose: This script takes in the raw list of gas stations and calculates 
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

setwd("C:/Users/marcu/Documents/gas-col")

library(tidyverse)
library(osrm)
library(sf)

# Load data
gas_stations <- read.csv("01_data/01_raw/gas_stations.csv", nrows = 10)
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
#at least one station was entered as an integer
gas_stations <- gas_stations %>% filter(lat != floor(lat) & lng != floor(lng))


### relabel

# Brand "" should be NA
gas_stations <- gas_stations %>% mutate(brand = ifelse(brand == "", NA, brand))


### Calculate competition metrics
source("02_R/02_functions/haversine_distance.R")
source("02_R/02_functions/competition_metrics.R")
source("02_R/02_functions/population_within_radius.R")

population_within_10km <- mapply(population_within_radius,
                                 lat = gas_stations$lat,
                                 lng = gas_stations$lng,
                                 MoreArgs=list(pop_data = cleaned_population,
                                               radius = 10^4))
gas_stations$population_within_10km <- population_within_10km
population_within_5km <- mapply(population_within_radius,
                                 lat = gas_stations$lat,
                                 lng = gas_stations$lng,
                                 MoreArgs=list(pop_data = cleaned_population,
                                               radius = 5*10^3))
gas_stations$population_within_5km <- population_within_5km

#competition_metrics_sample <- sapply(1:1000, competition_metrics, df = gas_stations)
competition_metrics <- sapply(1:nrow(gas_stations), competition_metrics, df = gas_stations)
gas_stations <- gas_stations %>% cbind(t(competition_metrics))
#rm(brand_of_nearest_station_duration_list)


### Most common brands

gas_stations <- gas_stations %>% mutate(brand_and_neighbor = paste(brand, brand_of_nearest_station_duration, sep = "_"))

most_common_brands <- gas_stations %>%
  count(brand, sort = TRUE) %>%
  head(5) %>%
  pull(brand)

gas_stations <- gas_stations %>%
  mutate(brand_and_neighbor_most_common = 
           if_else(brand %in% most_common_brands & brand_of_nearest_station_duration  %in% 
                     most_common_brands, brand_and_neighbor, ""),
         nearest_station_same_brand = ifelse(!is.na(brand) & !is.na(brand_of_nearest_station_duration), brand == brand_of_nearest_station_duration, FALSE))

#saveRDS(gas_stations, file = "01_data/02_processed/cleaned_gas_stations.rds")
