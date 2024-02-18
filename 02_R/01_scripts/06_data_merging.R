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
brand_df <- readRDS("01_data/02_processed/brand_df.rds")

combined_df <- left_join(gas_prices, gas_stations, by = c("stid" = "id")) %>%
  left_join(oil, by = c("date" = "date_day"))


rm(gas_prices)

# Big brands
big_brands <- brand_df %>% mutate(brand = as.character(brand)) %>% select(brand) %>%
  pull()
# SPS
sps_brands <- brand_df %>% filter(t_stat > 2) %>% mutate(brand = as.character(brand)) %>% 
  select(brand) %>%
  pull()

combined_df <- combined_df %>% rename(oil = brent)

combined_df <- combined_df %>% 
  mutate(unilateral_mkt_pwr = ifelse((brand %in% sps_brands) & 
                                       (brand_of_nearest_station_phdis == brand), 
                                       brand_df[which(brand_df[,1] == brand),
                                       paste0(brand, "_t_stat")][[1]] > 1.96,0),
         coordinated_mkt_pwr = ifelse((brand %in% sps_brands) & 
                                        (brand_of_nearest_station_phdis %in% sps_brands) & 
                                        (brand != brand_of_nearest_station_phdis),
                                        brand_df[which(brand_df[,1] == brand),
                                        paste0(brand_of_nearest_station_phdis, "_t_stat")][[1]] > 1.96,0))

combined_df <- combined_df %>% mutate(weekday = weekdays(date))

combined_df_big <- combined_df %>% filter(brand %in% sps_brands)

combined_df_big <- combined_df_big %>%
  group_by(stid) %>%
  do({
    model <- lm(e5 ~ oil, data = .)
    residuals <- broom::augment(model)$`.resid`
    data.frame(., ect = residuals)
  })

combined_df_big <- combined_df_big %>%
  mutate(lag_ect = ifelse(stid == lag(stid, n = 1), lag(ect), NA),
         lag_e5 = ifelse(stid == lag(stid, n = 1), lag(e5), NA),
         lag_oil = ifelse(stid == lag(stid, n = 1), lag(oil), NA))

#combined_df_mondays <- combined_df_big %>% filter(weekday == "Monday")

for (i in 1:30) {
  combined_df_big <- combined_df_big %>%
    mutate(!!paste0("diff_e5_", i) := ifelse(stid == lag(stid, n = i), 
                                             lag(e5, n = i - 1) - lag(e5, n = i), 
                                             NA),
           !!paste0("diff_e5_pos_", i) := max(!!paste0("diff_e5_", i),0),
           !!paste0("diff_e5_neg_", i) := ifelse(stid == lag(stid, n = i), 
                                             min(lag(e5, n = i - 1) - lag(e5, n = i), 0), 
                                             NA),
           !!paste0("m1_diff_oil_", i) := ifelse(stid == lag(stid, n = i), 
                                              lag(oil, n = i - 1) - lag(oil, n = i), 
                                              NA))
  combined_df_big[[paste0("diff_e5_pos_", i)]] <- pmax(combined_df_big[[paste0("diff_e5_", i)]],0)
  combined_df_big[[paste0("diff_e5_neg_", i)]] <- pmin(combined_df_big[[paste0("diff_e5_", i)]],0)  
  combined_df_big[[paste0("m2_diff_oil_pos_", i)]] <- pmax(combined_df_big[[paste0("m1_diff_oil_", i)]],0)
  combined_df_big[[paste0("m2_diff_oil_neg_", i)]] <- pmin(combined_df_big[[paste0("m1_diff_oil_", i)]],0)
  # combined_df_big[[paste0("m3_diff_oil_uni_", i)]] <- 
  #   ifelse(combined_df_big$unilateral_mkt_pwr > 0, combined_df_big[[paste0("m1_diff_oil_", i)]], 0)
  # combined_df_big[[paste0("m3_diff_oil_no_uni_", i)]] <- 
  #   ifelse(combined_df_big$unilateral_mkt_pwr == 0, combined_df_big[[paste0("m1_diff_oil_", i)]], 0)
  # combined_df_big[[paste0("m4_diff_oil_coord_", i)]] <- 
  #   ifelse(combined_df_big$coordinated_mkt_pwr > 0, combined_df_big[[paste0("m1_diff_oil_", i)]], 0)
  # combined_df_big[[paste0("m4_diff_oil_no_coord_", i)]] <- 
  #   ifelse(combined_df_big$coordinated_mkt_pwr == 0, combined_df_big[[paste0("m1_diff_oil_", i)]], 0)
  # 
  # combined_df_big[[paste0("m5_diff_oil_uni_pos_", i)]] <- 
  #   ifelse(combined_df_big$unilateral_mkt_pwr > 0, combined_df_big[[paste0("m2_diff_oil_pos_", i)]], 0)
  # combined_df_big[[paste0("m5_diff_oil_no_uni_pos_", i)]] <- 
  #   ifelse(combined_df_big$unilateral_mkt_pwr == 0, combined_df_big[[paste0("m2_diff_oil_pos_", i)]], 0)
  # combined_df_big[[paste0("m6_diff_oil_coord_pos_", i)]] <- 
  #   ifelse(combined_df_big$coordinated_mkt_pwr > 0, combined_df_big[[paste0("m2_diff_oil_pos_", i)]], 0)
  # combined_df_big[[paste0("m6_diff_oil_no_coord_pos_", i)]] <- 
  #   ifelse(combined_df_big$coordinated_mkt_pwr == 0, combined_df_big[[paste0("m2_diff_oil_pos_", i)]], 0)
  # 
  # combined_df_big[[paste0("m5_diff_oil_uni_neg_", i)]] <- 
  #   ifelse(combined_df_big$unilateral_mkt_pwr > 0, combined_df_big[[paste0("m2_diff_oil_neg_", i)]], 0)
  # combined_df_big[[paste0("m5_diff_oil_no_uni_neg_", i)]] <- 
  #   ifelse(combined_df_big$unilateral_mkt_pwr == 0, combined_df_big[[paste0("m2_diff_oil_neg_", i)]], 0)
  # combined_df_big[[paste0("m6_diff_oil_coord_neg_", i)]] <- 
  #   ifelse(combined_df_big$coordinated_mkt_pwr > 0, combined_df_big[[paste0("m2_diff_oil_neg_", i)]], 0)
  # combined_df_big[[paste0("m6_diff_oil_no_coord_neg_", i)]] <- 
  #   ifelse(combined_df_big$coordinated_mkt_pwr == 0, combined_df_big[[paste0("m2_diff_oil_neg_", i)]], 0)
}

mkt_pwr_est_df <- combined_df %>%
  select(log_e5,
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

spt_df <- combined_df_big %>%
  select(starts_with("diff_e5_"),
         starts_with("m1"),
         lag_ect,
         stid,
         brand,
         date,
         lag_e5,
         lag_oil)
saveRDS(spt_df, file = "01_data/02_processed/spt_df.rds")
rm(spt_df)

apt_df <- combined_df_big %>%
  select(starts_with("diff_e5_"),
         starts_with("m2"),
         lag_ect,
         stid,
         brand,
         date,
         lag_e5,
         lag_oil,
         unilateral_mkt_pwr,
         coordinated_mkt_pwr)
saveRDS(apt_df, file = "01_data/02_processed/apt_df.rds")
rm(apt_df)

