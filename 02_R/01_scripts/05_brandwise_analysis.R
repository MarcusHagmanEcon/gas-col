#--------------------------------------------------------------------------
# Script Name: 05_analysis.R
# 
# Author: Marcus Hagman
# Date: 2023-12-05
# 
# Purpose: This script runs regression on a per-brand level
#
# Input: - 01_data/02_processed/analysis_data.rds
#        - 01_data/02_processed/cleaned_gas_stations.rds
# 
# Output: - Graphs
#         - 01_data/02_processed/brand_df.rds
#
# Instructions: 
#
# Revision History:
#--------------------------------------------------------------------------

rm(list=ls())

setwd("C:/Users/marcu/Documents/gas-col")

library(tidyverse)
library(lfe)

# Load data
analysis_data <- readRDS("01_data/02_processed/analysis_data.rds")
gas_stations <- readRDS("01_data/02_processed/cleaned_gas_stations.rds")

brand_df <- gas_stations %>% filter(!is.na(brand)) %>% group_by(brand) %>% 
  summarise(n = n()) %>% arrange(-n) %>%  slice(1:5) %>% 
  mutate(coef = NA, se = NA, t_stat = NA, p_val = NA)
for (b in brand_df$brand){
  print(which(brand_df$brand == b))
  print(b)
  reg_data <- analysis_data %>% filter(brand == b)
  
  model_brand <- felm(log_e5 ~ same_brand_as_nearest_station_phdis +
                        stations_within_5km + stations_within_10km + stations_within_15km +
                        population_within_5km + population_within_10km + 
                        population_within_5km_sq + population_within_10km_sq + 
                        stations_per_million_pop_10km + 
                        less_than_50m_to_neighbor_phdis| date | 0 | date + stid ,
                      data = reg_data,
                      na.action = na.omit)
  
  model_brand_coef <- summary(model_brand)$coef
  
  brand_df$coef[which(brand_df$brand == b)] <- model_brand_coef[1,1]
  brand_df$se[which(brand_df$brand == b)] <- model_brand_coef[1,2]
  brand_df$t_stat[which(brand_df$brand == b)] <- model_brand_coef[1,3]
  brand_df$p_val[which(brand_df$brand == b)] <- model_brand_coef[1,4]
  
  print(model_brand_coef)
}

# Calculate the lower and upper bounds of the 95% CI
brand_df$lower_bound = brand_df$coef - 1.96 * brand_df$se
brand_df$upper_bound = brand_df$coef + 1.96 * brand_df$se

# Sort the brands in the order they appear in the dataframe
brand_df$brand <- factor(brand_df$brand, levels = unique(brand_df$brand))
brand_df <- brand_df %>% arrange(n)

# Create the plot with swapped axes, a thicker dotted red line at zero, 
# and brands sorted in their original order
plot1 <- ggplot(brand_df, aes(x = coef, y = brand)) +
      geom_point() +  # This adds the point estimates
      geom_errorbarh(aes(xmin = lower_bound, xmax = upper_bound), height = 0.2) +
      geom_vline(xintercept = 0, linetype = "dotted", color = "red", size = 1) + # Thicker red line
      theme_minimal() +
      labs(title = "95% CI of Coefficient Estimates for Each Brand",
       x = "Coefficient Estimate",
       y = "Brand")

ggsave("03_outputs/figures/20231206_brand_coef.png", plot1, width = 10, height = 6)

sps_list <- brand_df %>% arrange(-n) %>% filter(t_stat>2) %>% select(brand) %>%
  mutate(brand = as.character(brand)) %>% pull

for(element in sps_list) {
  # Create a new column in brand_df for each element
  # The column name is the element value followed by '_coef'
  brand_df[[paste0(element, "_coef")]] <- NA
  brand_df[[paste0(element, "_se")]] <- NA
  brand_df[[paste0(element, "_t_stat")]] <- NA
  brand_df[[paste0(element, "_p_val")]] <- NA
  
  analysis_data[[paste0(element, "_neighbor_brand")]] <- analysis_data$brand_of_nearest_station_phdis == element
}

for (b in sps_list){
  print(b)
  reg_data <- analysis_data %>% filter(brand == b)
  
  # Create the formula string
  formula_str <- paste("log_e5 ~", paste(paste0(sps_list, "_neighbor_brand"), collapse = " + "),
                       "+ stations_within_5km + stations_within_10km + stations_within_15km +
                        population_within_5km + population_within_10km + 
                        population_within_5km_sq + population_within_10km_sq + 
                        stations_per_million_pop_10km + 
                        less_than_50m_to_neighbor_phdis | date | 0 | date + stid")
  
  # Convert to a formula
  model_formula <- formula(formula_str)
  
  # Use in felm
  model_brand <- felm((formula = model_formula) ,
                      data = reg_data,
                      na.action = na.omit)
  
  model_brand_coef <- summary(model_brand)$coef
  
  for (b2 in sps_list){
    brand_df[which(brand_df$brand == b), which( grepl(b2,names(brand_df)))] <- 
      t(model_brand_coef[which(sps_list == b2),1:4])
  }
}


filtered_brand_df <- brand_df %>% 
  filter(brand %in% sps_list)

# Reshape the data to a long format
long_brand_df <- filtered_brand_df %>%
  gather(key = "category", value = "value", aral_coef:total_p_val) %>%
  separate(category, into = c("brand_type", "measure"), sep = "_") %>%
  filter(brand_type %in% sps_list) %>%
  spread(key = measure, value = value)  %>%
  mutate(group_id = as.numeric(factor(paste(brand, brand_type)))) %>%
  mutate(y_pos = group_id * 10 - (group_id - 1) %% 3)

# Calculate the lower and upper bounds of the 95% CI
long_brand_df$lower_bound = long_brand_df$coef - 1.96 * long_brand_df$se
long_brand_df$upper_bound = long_brand_df$coef + 1.96 * long_brand_df$se

long_brand_df <- long_brand_df %>% arrange(n)

# Plotting
plot2 <- ggplot(long_brand_df, aes(x = coef, y = brand_type)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lower_bound, xmax = upper_bound), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "red", size = 1) +
  facet_wrap(~ brand, scales = "free_y", nrow = 3) +  # Adding facet_wrap
  theme_minimal() +
  labs(title = "95% CI of Coefficient Estimates for Each Brand",
       x = "Coefficient Estimate",
       y = "Brand")
plot2

ggsave("03_outputs/figures/20231206_brand_pair_coef.png", plot2, width = 10, height = 6)



saveRDS(brand_df, file = "01_data/02_processed/brand_df.rds")
