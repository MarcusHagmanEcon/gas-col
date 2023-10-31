#--------------------------------------------------------------------------
# Script Name: 02_data_cleaning_prices.R
# 
# Author: Marcus Hagman
# Date: 2023-10-18
# 
# Purpose: This script takes in the long document of all price changes and 
#           calculates the number of price changes per day and average daily price.
#           Outliers are removed.
#
# Input: - 01_data/01_raw/gas_prices.csv
# 
# Output: - 01_data/02_processed/cleaned_gas_prices.rds
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
gas_prices <- read.csv("01_data/01_raw/gas_prices.csv")

# Set up variables 
gas_prices <- gas_prices %>% mutate(datetime = ymd_hms(gas_prices$date),
                                    date = as.Date(substr(date, 1, 10)),
                                    price_changed = TRUE) %>%
  select(-c(changed, id))

gas_prices <- gas_prices %>% arrange(stid, datetime)

# Remove observations with unrealistic prices
gas_prices <- gas_prices %>% mutate(e5 = ifelse(e5 > 10 & e5 < 9999, e5, NA),
                                    e10 = ifelse(e10 > 10 & e10 < 9999, e10, NA),
                                    diesel = ifelse(diesel > 10 & diesel < 9999, diesel, NA))

# Add observations for start and end of day
source("02_R/02_functions/add_obs_start_end.R")
rows_to_add <- add_obs_start_end(gas_prices)
gas_prices <- gas_prices %>% rbind(rows_to_add)
gas_prices <- gas_prices %>% arrange(stid, datetime)
rm(rows_to_add)

# Fill in prices to the start and end of day observations
source("02_R/02_functions/fill_in_prices.R")
gas_prices <- fill_in_prices(gas_prices)

# Add duration variable
gas_prices <- gas_prices %>% arrange(stid, datetime)
gas_prices <- gas_prices %>% mutate(duration = ifelse(stid == lead(stid),
                                                      lead(datetime) - datetime, NA))

# Remove observations before 7 and after 21
source("02_R/02_functions/remove_obs_outside_hours.R")
gas_prices <- remove_obs_outside_hours(gas_prices)

gas_prices <- gas_prices %>%
  mutate(
    weighted_e5 = e5 * duration,
    weighted_e10 = e10 * duration,
    weighted_diesel = diesel * duration
  ) 

gas_prices_day <- gas_prices %>%
  mutate(
    weighted_e5 = e5 * duration,
    weighted_e10 = e10 * duration,
    weighted_diesel = diesel * duration
  ) %>%
  group_by(stid, date) %>%
  summarise(
    e5 = sum(weighted_e5) / sum(duration),
    e10 = sum(weighted_e10) / sum(duration),
    diesel = sum(weighted_diesel) / sum(duration)
  )

# Remove obs where all prices are NA
gas_prices_day <- gas_prices_day %>% filter(!is.na(e5) | !is.na(e10) | !is.na(diesel))

saveRDS(gas_prices_day, file = "01_data/02_processed/cleaned_gas_prices.rds")
