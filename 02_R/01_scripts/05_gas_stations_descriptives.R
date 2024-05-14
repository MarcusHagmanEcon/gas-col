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

setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), "/Dropbox/gas-col"))

library(tidyverse)
library(xtable)
library(ggplot2)
library(viridis)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(gridExtra)
library(knitr)

timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

# Load data
gas_stations <- readRDS("01_data/02_processed/cleaned_gas_stations.rds")

source("02_R/02_functions/transform_strings.R")


### List of most common brands, frequency and percentage
tbl1 <- gas_stations %>% filter(!is.na(brand)) %>% group_by(brand) %>% 
  summarise(n = n()) %>% arrange(-n) %>% mutate(percentage = paste(as.character(round(100*n/nrow(gas_stations)), digits = 5), "%", sep = ""))
names(tbl1) <- c("Brand", "Number", "Percentage")
tbl1$Brand <- transform_strings(tbl1$Brand)
latex_tbl1 <- xtable(tbl1[1:5,])
print(latex_tbl1, type = "latex", file = paste0("03_output/tables/", timestamp,"_most_common_brands.tex"), 
      include.rownames=FALSE)



# Get Germany boundary data
germany_map <- ne_countries(scale = "medium", country = "germany", returnclass = "sf")
n_bins <- 15

density_map_list <- list()
# Plotting with approximate density using geom_density_2d_filled
density_map_unfiltered <- ggplot() +
  geom_sf(data = germany_map, fill = "lightgray", color = "black", size = 1.5) + # Plot the map
  geom_point(data = gas_stations, aes(x = lng, y = lat), alpha = 0.4, size = 0.1) + # Plot points
  stat_density_2d(data = gas_stations, aes(x = lng, y = lat, fill = ..level..), geom = "polygon", alpha = 0.3, bins = n_bins) + # Add density estimates
  scale_fill_viridis_c(option = "inferno") + # Use viridis color scale
  coord_sf(xlim = c(5.5, 15), ylim = c(47, 55), expand = FALSE) + # Set map limits
  labs(title = "Density of All Stations in Germany") +
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 20)
  )

density_map_list[["unfiltered"]] <- density_map_unfiltered

for (b in tolower(tbl1$Brand[1:5])){
  
   map <- ggplot() +
    geom_sf(data = germany_map, fill = "lightgray", color = "black", size = 1.5) + # Plot the map
    geom_point(data = gas_stations %>% filter(brand == b), aes(x = lng, y = lat), alpha = 0.4, size = 0.1) + # Plot points
    stat_density_2d(data = gas_stations %>% filter(brand == b), aes(x = lng, y = lat, fill = ..level..), geom = "polygon", alpha = 0.3, bins = n_bins) + # Add density estimates
    scale_fill_viridis_c(option = "inferno") + # Use viridis color scale
    coord_sf(xlim = c(5.5, 15), ylim = c(47, 55), expand = FALSE) + # Set map limits
    labs(title = paste0("Density of ", transform_strings(b), " Stations")) +
    theme_minimal()+
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(), 
      axis.text.y = element_blank(),
      axis.title.x = element_blank(), 
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(size = 20)
    )
   map
  density_map_list[[as.character(b)]] <- map
}

# Arrange the plots
png(paste0("03_output/graphs/", timestamp,"_density_map.png"), width = 1200, height = 800) # Adjust the size as needed
grid.arrange(grobs = density_map_list, ncol = 3, nrow = 2)
dev.off()

# Contingency table of stations and their nearest neighbors
most_common_brands <- gas_stations %>% filter(!is.na(brand)) %>% group_by(brand) %>%
  summarise(n = n()) %>% arrange(-n) %>% select(brand) %>% head(5) %>% pull()
gas_stations$brand <- factor(gas_stations$brand, levels = most_common_brands)
gas_stations$brand_of_nearest_station_phdis <- factor(gas_stations$brand_of_nearest_station_phdis, levels = most_common_brands)
tbl2 <- table(gas_stations$brand[gas_stations$brand %in% most_common_brands &
                                   gas_stations$brand_of_nearest_station_phdis %in% most_common_brands],
      gas_stations$brand_of_nearest_station_phdis[gas_stations$brand %in% most_common_brands  &
                                                       gas_stations$brand_of_nearest_station_phdis %in% most_common_brands])
colnames(tbl2) <- transform_strings(most_common_brands)
rownames(tbl2) <- transform_strings(most_common_brands)
latex_tbl2 <- xtable(tbl2, caption = "Contingency Table of Gas Station Brands (Rows) and Brands of Nearest Stations (Columns)")
print(latex_tbl2, type = "latex", file = paste0("03_output/tables/", timestamp, "_contingency_table.tex"))




