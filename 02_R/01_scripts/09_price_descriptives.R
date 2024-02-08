#--------------------------------------------------------------------------
# Script Name: 05_analysis.R
# 
# Author: Marcus Hagman
# Date: 2024-02-05
# 
# Purpose: This script produces all the outputs, including tables and graphs.
#
# Input: - 01_data/02_processed/mkt_pwr_est_df.rds
# 
# Output: Tables and graphs
#
# Instructions: 
#
# Revision History:
#--------------------------------------------------------------------------

rm(list=ls())

setwd("C:/Users/marcu/Documents/gas-col")

library(ggplot2)
library(tidyverse)

timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

# To change: Make lag_oil into oil
gas_prices  <- readRDS("01_data/02_processed/cleaned_gas_prices.rds") 
oil <- readRDS("01_data/02_processed/cleaned_oil_prices.rds")

station_prices <- gas_prices %>% left_join(oil, by = c("date" = "date_day")) %>%
  select(stid, date, brent, e5) %>% mutate( e5 = e5 /1000)

rm(gas_prices)
rm(oil)


station_list <- station_prices$stid %>% unique


avg_prices <- station_prices %>% group_by(date) %>%
  summarize(brent = ifelse(max(brent) == min(brent), max(brent),0), 
            median_e5 = median(e5),
            p5_e5 = quantile(e5, 0.05),
            p95_e5 = quantile(e5, 0.95))

# Reshape the data from wide to long
avg_prices_long <- pivot_longer(avg_prices, 
                                cols = c(brent, median_e5, p5_e5, p95_e5), 
                                names_to = "Series", values_to = "price")

# Plot using ggplot2
plot <- ggplot(avg_prices_long, aes(x = date, y = price, color = Series)) +
  geom_line() + # Use geom_line() for line plots
  theme_minimal() + # Optional: Use a minimal theme
  labs(x = "Date", y = "Price (EUR / liter)", title = "Brent and Gasoline Prices over Time") +
  scale_color_manual(values = c("brent" = "blue", "median_e5" = "darkred",
                                "p5_e5" = "pink", "p95_e5" = "red"),
                     labels = c("Crude Oil Price", "Median Gasoline Price", 
                                "5th Percentile Gasoline Price", "95th Percentile Gasoline Price"))
ggsave(paste("03_outputs/figures/", timestamp,"_prices_over_time.png", sep=""), plot, width = 10, height = 6, units = "in")


# Below is some code if I want to save show the price series of a few specific stations too

# 
# particular_prices <-  avg_prices %>% select(date) %>%
#   left_join(station_prices %>% filter(stid == station_list[3]) %>%
#               select(date, e5) %>% rename("e5_example_1" = "e5"), by = c("date")) %>%
#   left_join(station_prices %>% filter(stid == station_list[6]) %>%
#               select(date, e5) %>% rename("e5_example_2" = "e5"), by = c("date")) %>% 
#   select(-starts_with("stid"))
# 
# avg_prices <- avg_prices %>% left_join(particular_prices, by = "date")
# 
# # Reshape the data from wide to long
# avg_prices_long <- pivot_longer(avg_prices, cols = c(brent, avg_e5, e5_example_1, e5_example_2), names_to = "Series", values_to = "price")
# 
# # Plot using ggplot2
# plot <- ggplot(avg_prices_long, aes(x = date, y = price, color = type)) +
#   geom_line() + # Use geom_line() for line plots
#   theme_minimal() + # Optional: Use a minimal theme
#   labs(x = "Date", y = "Price", title = "Brent and Avg E5 Prices Over Time") +
#   scale_color_manual(values = c("brent" = "blue", "avg_e5" = "red", 
#                                 "e5_example_1" = "green", "e5_example_2" = "black")) # Optional: Specify colors
# plot
# 
