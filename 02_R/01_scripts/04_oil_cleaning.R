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
library(xml2)
library(zoo)

# Oil in USD
oil <- read.csv("01_data/01_raw/oil.csv", skip = 4) %>% 
  mutate(Day = as.Date(Day, format="%m/%d/%Y"))
colnames(oil) = c("date_day", "brent")


# USD EUR Exchange rate

xml_file <- read_xml("01_data/01_raw/usd_eur.xml")
# Get and strip namespaces (if you want to ignore them)
ns <- xml_ns(xml_file)
xml_ns_strip(xml_file)

# Find all observation nodes
obs_nodes <- xml_find_all(xml_file, "//Obs")

# Check if nodes are found
print(length(obs_nodes))

# Extract data from each observation (if nodes are found)
if (length(obs_nodes) > 0) {
  data <- lapply(obs_nodes, function(node) {
    data.frame(
      TimePeriod = xml_attr(node, "TIME_PERIOD"),
      ObsValue = as.numeric(xml_attr(node, "OBS_VALUE"))
    )
  })
  
  # Convert the list to a data frame
  fx <- do.call(rbind, data)
} else {
  fx <- NULL
}

fx <- fx %>% mutate(date_day = as.Date(TimePeriod)) %>%
  select(-c(TimePeriod)) %>%
  rename(usd_eur = ObsValue)


# Merge
oil <- oil %>% left_join(fx, by = c("date_day"))

# Calculate price of Brent in Euros per liter
oil <- oil %>% mutate(brent = (brent/usd_eur)/159 ) %>%
  select(-c(usd_eur))

oil <- oil %>% arrange(date_day)


# Find the row index of the first non-NA value in 'brent'
first_non_na <- which(!is.na(oil$brent))[1]

# Keep only the rows from the first non-NA 'brent' value onwards
oil <- oil[first_non_na:nrow(oil), ]

# Create a complete date sequence
full_dates <- data.frame(date_day = seq(from = min(oil$date_day), to = max(oil$date_day), by = "day"))

# Merge the full date sequence with your dataset
oil <- merge(full_dates, oil, by = "date_day", all.x = TRUE)

# Perform linear interpolation on 'brent' column
oil$brent <- na.approx(oil$brent)


saveRDS(oil, file = "01_data/02_processed/cleaned_oil_prices.rds")

