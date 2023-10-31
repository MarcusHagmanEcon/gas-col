#--------------------------------------------------------------------------
# Script Name: 04_oil_cleaning.R
# 
# Author: Marcus Hagman
# Date: 2023-10-18
# 
# Purpose: Cleans raw europe brent spot prices
#
# Input: - 01_data/01_raw/europe_brent_spot.csv
# 
# Output: - 01_data/02_processed/cleaned_oil_prices.rds
#
# Instructions: 
#
# Revision History:
#--------------------------------------------------------------------------

rm(list=ls())

setwd("C:/Users/marcu/Documents/gas-col")

library(tidyverse)

oil <- read.csv("01_data/01_raw/europe_brent_spot.csv", skip = 4) %>% 
  mutate(Day = as.Date(Day, format="%m/%d/%Y"))
colnames(oil) = c("date_day", "brent")

saveRDS(oil, file = "01_data/02_processed/cleaned_oil_prices.rds")