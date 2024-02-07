#--------------------------------------------------------------------------
# Script Name: 01_data_cleaning_stations.R
# 
# Author: Marcus Hagman
# Date: 2023-10-18
# 
# Purpose: This script takes in the cleaned gas station data, and produces 
#          tables and graphs. It contains no price data.
#
# Input: - 01_data/02_processed/cleaned_gas_stations.rds
# 
# Output: Tables, graphs
#
# Instructions: 
#
# Revision History:
#--------------------------------------------------------------------------

rm(list=ls())

setwd("C:/Users/marcu/Documents/gas-col")

library(tidyverse)
library(xtable)
library(ggplot2)

# Load data
gas_stations <- readRDS("01_data/02_processed/cleaned_gas_stations.rds")


### List of most common brands, frequency and percentage
tbl1 <- gas_stations %>% filter(!is.na(brand)) %>% group_by(brand) %>% 
  summarise(n = n()) %>% arrange(-n) %>% mutate(percentage = paste(as.character(round(100*n/nrow(gas_stations)), digits = 5), "%", sep = ""))
names(tbl1) <- c("Brand", "Number", "Percentage")
latex_tbl1 <- xtable(tbl1[1:10,])
print(latex_tbl1, type = "latex", file = "03_outputs/tables/20231029_desctab1.tex", 
      include.rownames=FALSE)

# Contingency table of stations and their nearest neighbors
most_common_brands <- gas_stations %>% filter(!is.na(brand)) %>% group_by(brand) %>% 
  summarise(n = n()) %>% arrange(-n) %>% select(brand) %>% head(10) %>% pull()
gas_stations$brand <- factor(gas_stations$brand, levels = most_common_brands)
gas_stations$brand_of_nearest_station_phdis <- factor(gas_stations$brand_of_nearest_station_phdis, levels = most_common_brands)
tbl2 <- table(gas_stations$brand[gas_stations$brand %in% most_common_brands & 
                                   gas_stations$brand_of_nearest_station_phdis %in% most_common_brands],
      gas_stations$brand_of_nearest_station_phdis[gas_stations$brand %in% most_common_brands  & 
                                                       gas_stations$brand_of_nearest_station_phdis %in% most_common_brands])
latex_tbl2 <- xtable(tbl2, caption = "Contingency Table of Gas Station Brands and Nearest Stations")
print(latex_tbl2, type = "latex", file = "03_outputs/tables/20231029_desctab2.tex")



# Frequency independent
tbl3 <- gas_stations %>% filter(!is.na(brand)) %>% group_by(independent) %>% 
  summarise(n = n()) %>% arrange(-n) %>% mutate(independent = ifelse(independent, "Independent", "Not Independent"))
latex_tbl3 <- xtable(tbl3)
print(latex_tbl3, type = "latex", file = "03_outputs/tables/output_table3.tex", 
      include.rownames=FALSE)


# Correlation structure under different distance metrics
tbl4 <- cor(gas_stations %>% 
                    select(same_brand_as_nearest_station_phdis, same_brand_as_nearest_station_drdis, same_brand_as_nearest_station_drdur) %>%
                    rename("Straight Distance" = same_brand_as_nearest_station_phdis,
                           "Driving Distance" = same_brand_as_nearest_station_drdis,
                           "Driving Duration" = same_brand_as_nearest_station_drdur)) %>%
  xtable(caption = "Correlation of a dummy variable indicating whether the closest station belongs to the same brand, under different distance metrics")
print(tbl4, file = "03_outputs/tables/20231030_desctab4.tex")


# Histogram of duration to nearest station
p1_phdis <- ggplot(gas_stations, aes(x=phdis_to_nearest_station)) +
  geom_histogram(binwidth=0.5, fill="blue", color="black", alpha=0.7) + # adjust binwidth as needed
  labs(title="Histogram of Distance (As the Crow flies) to Nearest Station",
       x="Distance (As the Crow flies) to Nearest Station, km",
       y="Count") +
  theme_minimal()+
  xlim(0, 35)
print(p1_phdis)
ggsave(filename="03_outputs/figures/20231029_descfig_disttonear_phdis.png", plot=p1_phdis, width=6, height=4, dpi=300)
p1_drdis <- ggplot(gas_stations, aes(x=drdis_to_nearest_station)) +
  geom_histogram(binwidth=0.5, fill="blue", color="black", alpha=0.7) + # adjust binwidth as needed
  labs(title="Histogram of Driving Distance to Nearest Station",
       x="Driving Distance to Nearest Station, km",
       y="Count") +
  theme_minimal()+
  xlim(0, 35)
print(p1_drdis)
ggsave(filename="03_outputs/figures/20231029_descfig_disttonear_drdis.png", plot=p1_drdis, width=6, height=4, dpi=300)
p1_drdur <- ggplot(gas_stations, aes(x=drdur_to_nearest_station)) +
  geom_histogram(binwidth=0.5, fill="blue", color="black", alpha=0.7) + # adjust binwidth as needed
  labs(title="Histogram of Driving Duration to Nearest Station",
       x="Driving Duration to Nearest Station, Minutes",
       y="Count") +
  theme_minimal()+
  xlim(0, 35)
print(p1_drdur)
ggsave(filename="03_outputs/figures/20231029_descfig_disttonear_drdur.png", plot=p1_drdur, width=6, height=4, dpi=300)

# Histogram of difference in duration to second nearest and nearest
p2 <- ggplot(gas_stations, aes(x=diff_duration_2nd_1st)) +
  geom_histogram(binwidth=1, fill="blue", color="black", alpha=0.7) + # adjust binwidth as needed
  labs(title="diff_duration_2nd_1st",
       x="diff_duration_2nd_1st",
       y="Count") +
  theme_minimal()
print(p2)
ggsave(file="03_outputs/figures/descriptive_histogram_duration_diff_2nd_1st.png", plot=p2, width=6, height=4, dpi=300)

# Histogram of difference in duration to second nearest and nearest
p3 <- ggplot(gas_stations, aes(x=population_within_10km)) +
  geom_histogram(binwidth=10^4, fill="blue", color="black", alpha=0.7) + # adjust binwidth as needed
  labs(title="population_within_10km",
       x="population_within_10km",
       y="Count") +
  theme_minimal()
print(p3)
ggsave(file="03_outputs/figures/descriptive_histogram_population_within_10km.png", plot=p3, width=6, height=4, dpi=300)

# Histogram of difference in duration to second nearest and nearest
p4 <- ggplot(gas_stations, aes(x=stations_per_million_pop_10km)) +
  geom_histogram(binwidth=10, fill="blue", color="black", alpha=0.7) + # adjust binwidth as needed
  labs(title="stations_per_million_pop_10km",
       x="stations_per_million_pop_10km",
       y="Count") +
  theme_minimal()
print(p4)
ggsave(file="03_outputs/figures/descriptive_histogram_stations_per_million_pop_10km.png", plot=p4, width=6, height=4, dpi=300)








