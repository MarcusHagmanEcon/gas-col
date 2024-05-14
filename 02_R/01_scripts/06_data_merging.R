#--------------------------------------------------------------------------
# Script Name: 03_data_merging.R
# 
# Author: Marcus Hagman
# Date: 2023-10-18
# 
# Purpose: This script produces dataframes that are ready for analysis without
#          further modifications. It uses 4 dataframes to do so, listed below.
#          For reasons of computational efficiency, the output dataframe for each 
#          level of analysis is separate.
#
# Input: - 01_data/02_processed/cleaned_gas_stations.rds
#        - 01_data/02_processed/cleaned_gas_prices.rds
#        - 01_data/02_processed/cleaned_oil_prices.rds
#        - 01_data/02_processed/brand_df.rds
# 
# Output: - 01_data/02_processed
#
# Instructions: 
#
# Revision History:
#--------------------------------------------------------------------------

rm(list=ls())

setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), "/Dropbox/gas-col"))

library(tidyverse)
library(lubridate)

# Load data
gas_prices <- readRDS("01_data/02_processed/cleaned_gas_prices.rds")
gas_stations <- readRDS("01_data/02_processed/cleaned_gas_stations.rds")
oil <- readRDS("01_data/02_processed/cleaned_oil_prices.rds")

combined_df <- left_join(gas_prices, gas_stations, by = c("stid" = "id")) %>%
  left_join(oil, by = c("date" = "date_day"))

rm(gas_prices)

combined_df <- combined_df %>% rename(oil = brent)

combined_df <- combined_df %>% mutate(weekday = weekdays(date))

mkt_pwr_est_df <- combined_df %>%
  select(log_e5,
         e5,
         oil,
         starts_with("same_brand_as_nearest_station_"), 
         starts_with("brand_of_nearest_station_"), 
         starts_with("stations_within_"), 
         starts_with("population_within_"),
         stations_per_million_pop_10km, 
         less_than_50m_to_neighbor_phdis,
         stid,
         brand,
         date)
saveRDS(mkt_pwr_est_df, file = "01_data/02_processed/mkt_pwr_est_df.rds")
