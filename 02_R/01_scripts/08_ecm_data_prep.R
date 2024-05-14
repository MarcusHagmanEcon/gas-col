#--------------------------------------------------------------------------
# Script Name: 08_ecm_data_prep.R
# 
# Author: Marcus Hagman
# Date: 2024-02-06
# 
# Purpose: This script runs regression on a per-brand level
#
# Input: - 01_data/02_processed/mkt_pwr_est_df.rds
#        - 01_data/02_processed/cleaned_gas_stations.rds
# 
# Output: - Graphs
#         - Tables
#         - 01_data/02_processed/brand_df.rds
#
# Instructions: 
#
# Revision History:
#--------------------------------------------------------------------------
rm(list=ls())

setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), "/Dropbox/gas-col"))

library(tidyverse)

data <- readRDS("01_data/02_processed/mkt_pwr_est_df.rds")
brand_df <- readRDS("01_data/02_processed/brand_df.rds")

# Big brands
big_brands <- brand_df %>% mutate(brand = as.character(brand)) %>% select(brand) %>%
  pull()
# SPS
sps_brands <- brand_df %>% filter(t_stat > 2) %>% mutate(brand = as.character(brand)) %>% 
  select(brand) %>%
  pull()

data <- data %>% 
  mutate(unilateral_mkt_pwr = ifelse((brand %in% sps_brands) & 
                                       (brand_of_nearest_station_phdis == brand), 
                                     brand_df[which(brand_df[,1] == brand),
                                              paste0(brand, "_t_stat")][[1]] > 1.96,0),
         coordinated_mkt_pwr = ifelse((brand %in% sps_brands) & 
                                        (brand_of_nearest_station_phdis %in% sps_brands) & 
                                        (brand != brand_of_nearest_station_phdis),
                                      brand_df[which(brand_df[,1] == brand),
                                               paste0(brand_of_nearest_station_phdis, "_t_stat")][[1]] > 1.96,0))


data <- data %>% filter(brand %in% sps_brands)

data <- data %>%
  group_by(stid) %>%
  do({
    model <- lm(e5 ~ oil, data = .)
    residuals <- broom::augment(model)$`.resid`
    data.frame(., ect = residuals)
  })

data <- data %>%
  mutate(lag_ect = ifelse(stid == lag(stid, n = 1), lag(ect), NA),
         lag_e5 = ifelse(stid == lag(stid, n = 1), lag(e5), NA),
         lag_oil = ifelse(stid == lag(stid, n = 1), lag(oil), NA))

for (i in 1:30) {
  data <- data %>%
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
  data[[paste0("diff_e5_pos_", i)]] <- pmax(data[[paste0("diff_e5_", i)]],0)
  data[[paste0("diff_e5_neg_", i)]] <- pmin(data[[paste0("diff_e5_", i)]],0)
  data[[paste0("m2_diff_oil_pos_", i)]] <- pmax(data[[paste0("m1_diff_oil_", i)]],0)
  data[[paste0("m2_diff_oil_neg_", i)]] <- pmin(data[[paste0("m1_diff_oil_", i)]],0)
}

spt_df <- data %>%
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

apt_df <- data %>%
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
