#--------------------------------------------------------------------------
# Script Name: 03_data_merging.R
# 
# Author: Marcus Hagman
# Date: 2023-10-18
# 
# Purpose: This script merges the two cleaned dataframes so analysis can be conducted.
#
# Input: - 01_data/02_processed/cleaned_gas_stations.rds
#        - 01_data/02_processed/cleaned_gas_prices.rds
# 
# Output: - 01_data/02_processed/analysis_data.rds
#
# Instructions: 
#
# Revision History:
#--------------------------------------------------------------------------

rm(list=ls())

setwd("C:/Users/marcu/Documents/gas-col")

library(tidyverse)
library(lubridate)

# Load data
gas_prices <- readRDS("01_data/02_processed/cleaned_gas_prices.rds")
gas_stations <- readRDS("01_data/02_processed/cleaned_gas_stations.rds")

analysis_data <- left_join(gas_prices, gas_stations, by = c("stid" = "id"))

# # Log prices
# analysis_data <- analysis_data %>% mutate(log_e5 = log(e5),
#                                           log_e10 = log(e10),
#                                           log_diesel = log(diesel))
# 
# # Remove outliers
# bounds <- analysis_data %>% group_by(date) %>% summarize(median = median(log_e5, na.rm = TRUE),
#                                                       iqr = IQR(log_e5, na.rm = TRUE),
#                                                       upper_bound_e5 = median + 5 * iqr,
#                                                       lower_bound_e5 = median - 5 * iqr ) %>%
#   select(date, upper_bound_e5, lower_bound_e5)
# 
# analysis_data <- analysis_data %>% left_join(bounds, by = c("date"))
# 
# analysis_data <- analysis_data %>% filter(log_e5 > lower_bound_e5 & log_e5 < upper_bound_e5)

saveRDS(analysis_data, file = "01_data/02_processed/analysis_data.rds")
