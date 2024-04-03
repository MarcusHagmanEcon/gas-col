#--------------------------------------------------------------------------
# Script Name: 02_data_cleaning_prices.R
# 
# Author: Marcus Hagman
# Date: 2023-10-18
# 
# Purpose: This script takes in the long document of all price changes and 
#           calculates the number of price changes per day and average daily price.
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

setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), "/Dropbox/gas-col"))

library(tidyverse)
library(lubridate)

timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

# Load data
gas_prices <- read.csv("01_data/01_raw/gas_prices.csv")

# Set up variables, remove ones that are not used
gas_prices <- gas_prices %>% mutate(datetime = ymd_hms(gas_prices$date),
                                    date = as.Date(substr(date, 1, 10)),
                                    price_changed = TRUE) %>%
  select(-c(changed, id))

gas_prices <- gas_prices %>% arrange(stid, datetime)

# Remove observations with unrealistic prices
gas_prices <- gas_prices %>% mutate(e5 = ifelse(e5 > 10 & e5 < 9999, e5, NA),
                                    e10 = ifelse(e10 > 10 & e10 < 9999, e10, NA),
                                    diesel = ifelse(diesel > 10 & diesel < 9999, diesel, NA))

# Change unit of prices to EUR per liter
gas_prices <- gas_prices %>% mutate(e5 = e5 / 1000,
                                   e10 = e10 / 1000,
                                   diesel = diesel / 1000)

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

# gas_prices <- gas_prices %>%
#   mutate(
#     weighted_e5 = e5 * duration,
#     weighted_e10 = e10 * duration,
#     weighted_diesel = diesel * duration
#   ) 

# Convert unit of observation into day-station   
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
    diesel = sum(weighted_diesel) / sum(duration),
    price_changes = n()
  )

# Remove obs where all prices are NA
gas_prices_day <- gas_prices_day %>% filter(!is.na(e5) | !is.na(e10) | !is.na(diesel))

# Log prices
gas_prices_day <- gas_prices_day %>% mutate(log_e5 = log(e5),
                                          log_e10 = log(e10),
                                          log_diesel = log(diesel))

# Remove outliers
bounds <- gas_prices_day %>% group_by(date) %>% 
  summarize(median = median(log_e5, na.rm = TRUE),
             iqr = IQR(log_e5, na.rm = TRUE),
             upper_bound_e5 = median + 5 * iqr,
             lower_bound_e5 = median - 5 * iqr ) %>%
  select(date, upper_bound_e5, lower_bound_e5)

gas_prices_day <- gas_prices_day %>% left_join(bounds, by = c("date"))

gas_prices_day <- gas_prices_day %>% filter(log_e5 > lower_bound_e5 & log_e5 < upper_bound_e5)

gas_prices_day <- gas_prices_day %>% select( -c(lower_bound_e5, upper_bound_e5))

price_info <- gas_prices_day %>%
  group_by(stid) %>%
  summarize(
    First_Price_Date = min(date[!is.na(e5)], na.rm = TRUE),
    Last_Price_Date = max(date[!is.na(e5)], na.rm = TRUE)
  )

saveRDS(gas_prices_day, file = "01_data/02_processed/cleaned_gas_prices.rds")
  