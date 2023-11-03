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
oil <- readRDS("01_data/02_processed/cleaned_oil.rds")

analysis_data <- left_join(gas_prices, gas_stations, by = c("stid" = "id")) %>%
  left_join(oil, by = c("date"))

analysis_data$diff_e5 <- analysis_data$e5 - lag(analysis_data$e5)


saveRDS(analysis_data, file = "01_data/02_processed/analysis_data.rds")
