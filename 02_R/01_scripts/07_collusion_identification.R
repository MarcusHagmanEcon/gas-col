#--------------------------------------------------------------------------
# Script Name: 07_collusion_identification.R
# 
# Author: Marcus Hagman
# Date: 2024-02-06
# 
# Purpose: This script runs regression with price as an outcome variable
#
# Input: - 01_data/02_processed/mkt_pwr_est_df.rds
#        - 01_data/02_processed/cleaned_gas_stations.rds
# 
# Output: - Graphs
#         - Tables
#         - 01_data/02_processed/brand_df.rds
#
# Instructions: 
#
# Revision History:
#--------------------------------------------------------------------------

rm(list=ls())

setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), "/Dropbox/gas-col"))

library(tidyverse)
library(lfe)
library(stargazer)

source("02_R/02_functions/transform_strings.R")

timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

# Load data
analysis_data <- readRDS("01_data/02_processed/mkt_pwr_est_df.rds")
gas_stations <- readRDS("01_data/02_processed/cleaned_gas_stations.rds")


# Distance measure comparison
model_phdis <- felm(log_e5 ~ same_brand_as_nearest_station_phdis +
                      stations_within_5km + stations_within_10km + stations_within_15km +
                      population_within_5km + population_within_10km + population_within_15km + 
                      population_within_5km_sq + population_within_10km_sq + population_within_15km_sq  +
                      stations_per_million_pop_10km + 
                      less_than_50m_to_neighbor_phdis|  brand + date  | 0 | brand + date,
                    data = analysis_data,
                    na.action = na.omit)
model_drdis <- felm(log_e5 ~ same_brand_as_nearest_station_drdis +
                      stations_within_5km + stations_within_10km + stations_within_15km +
                      population_within_5km + population_within_10km + population_within_15km + 
                      population_within_5km_sq + population_within_10km_sq + population_within_15km_sq  +
                      stations_per_million_pop_10km + 
                      less_than_50m_to_neighbor_phdis|  brand + date  | 0 | brand + date,
                    data = analysis_data,
                    na.action = na.omit)
model_drdur <- felm(log_e5 ~ same_brand_as_nearest_station_drdur +
                      stations_within_5km + stations_within_10km + stations_within_15km +
                      population_within_5km + population_within_10km + population_within_15km + 
                      population_within_5km_sq + population_within_10km_sq + population_within_15km_sq  +
                      stations_per_million_pop_10km + 
                      less_than_50m_to_neighbor_phdis|  brand + date  | 0 | brand + date,
                    data = analysis_data,
                    na.action = na.omit)
model_full <- felm(log_e5 ~ same_brand_as_nearest_station_phdis +
                     same_brand_as_nearest_station_drdis +
                     same_brand_as_nearest_station_drdur +
                     stations_within_5km + stations_within_10km + stations_within_15km +
                     population_within_5km + population_within_10km + population_within_15km + 
                     population_within_5km_sq + population_within_10km_sq + population_within_15km_sq  +
                     stations_per_million_pop_10km + 
                     less_than_50m_to_neighbor_phdis|  brand + date  | 0 | brand + date,
                   data = analysis_data,
                   na.action = na.omit)
model_phdis_coef <- summary(model_phdis)$coef
model_drdis_coef <- summary(model_drdis)$coef
model_drdur_coef <- summary(model_drdur)$coef
model_full_coef <- summary(model_full)$coef
# Use stargazer with type set to "latex"
sink(paste0("03_output/tables/", timestamp, "_distance_measure_comp.tex"))
stargazer(model_phdis, model_drdis, model_drdur, model_full, type = "latex",
          dep.var.labels = "$\\ln p^R_{ibt}$",
          covariate.labels = c("Same Brand as Nearest, Linear Distance", "Same Brand as Nearest, Driving Distance",
                               "Same Brand as Nearest, Driving Duration"),
          omit = c("stations_within_5km", "stations_within_10km", "stations_within_15km", 
                   "population_within_5km", "population_within_10km", "population_within_15km",
                   "population_within_5km_sq", "population_within_10km_sq", "population_within_15km_sq", 
                   "stations_per_million_pop_10km", "less_than_50m_to_neighbor_phdis"),
          se = list(model_phdis_coef[,2],model_drdis_coef[,2],model_drdur_coef[,2], model_full_coef[,2]), # assuming second column contains SEs
          omit.stat = "all", # to omit additional statistics like R-squared, F-statistic, etc.
          single.row = FALSE,
          title = "Effect on Log Price of Having the Nearest Station Belong to Same Brand", 
          label = "",
          notes = "Standard errors are clustered at the date and brand levels",
          add.lines = list( c("Controls, Date FE \\& Brand FE", rep("\\checkmark", 4)),
                            c("R^2", format(summary(model_phdis)$r.squared, digits = 5),
                              format(summary(model_drdis)$r.squared, digits = 5),
                              format(summary(model_drdur)$r.squared, digits = 5),
                              format(summary(model_full)$r.squared, digits = 5)),
                            c("Observations", format(summary(model_phdis)$N),
                              format(summary(model_drdis)$N),
                              format(summary(model_drdur)$N),
                              format(summary(model_full)$N))))
sink()

# By Brand

brand_df <- gas_stations %>% filter(!is.na(brand)) %>% group_by(brand) %>% 
  summarise(n = n()) %>% arrange(-n) %>%  slice(1:5) %>% 
  mutate(coef = NA, se = NA, t_stat = NA, p_val = NA)
models_list <- list()
for (b in brand_df$brand){
  print(which(brand_df$brand == b))
  print(b)
  reg_data <- analysis_data %>% filter(brand == b)
  
  model_name <- paste0("model_brand_", b)
  model <- felm(log_e5 ~ same_brand_as_nearest_station_phdis +
                stations_within_5km + stations_within_10km + stations_within_15km +
                population_within_5km + population_within_10km + population_within_15km + 
                population_within_5km_sq + population_within_10km_sq + population_within_15km_sq  +
                stations_per_million_pop_10km + 
                less_than_50m_to_neighbor_phdis | date | 0 | date + stid,
              data = reg_data,
              na.action = na.omit)
  
  models_list[[model_name]] <- model
  
  model_brand_coef <- summary(model)$coef

  brand_df$coef[which(brand_df$brand == b)] <- model_brand_coef[1,1]
  brand_df$se[which(brand_df$brand == b)] <- model_brand_coef[1,2]
  brand_df$t_stat[which(brand_df$brand == b)] <- model_brand_coef[1,3]
  brand_df$p_val[which(brand_df$brand == b)] <- model_brand_coef[1,4]

  print(model_brand_coef)
}

transformed_brands <- transform_strings(brand_df$brand)

sink(paste0("03_output/tables/", timestamp, "_brand_coef.tex"))
stargazer(models_list, type = "latex",
          dep.var.labels = "\\ln p^R_{ibt}",
          column.labels = transform_strings(brand_df$brand),
          covariate.labels = c("Same Brand as Nearest, Linear Distance"),
          omit = c("stations_within_5km", "stations_within_10km", "stations_within_15km", 
                   "population_within_5km", "population_within_10km", "population_within_15km",
                   "population_within_5km_sq", "population_within_10km_sq", "population_within_15km_sq", 
                   "stations_per_million_pop_10km", "less_than_50m_to_neighbor_phdis"),
          omit.stat = "all", # to omit additional statistics like R-squared, F-statistic, etc.
          single.row = FALSE,
          title = "Effect on Log Price of Having the Nearest Station Belong to Same Brand, by Brand", 
          label = "",
          notes = "Standard errors are clustered at the date and station levels",
          add.lines = list( c("Controls \\& Date Fixed Effects", rep("\\checkmark", 5)), 
                            c("Observations", 
                              sapply(models_list, 
                                     function(model) format(summary(model)$N)))))
sink()


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

ggsave("03_output/graphs/20240206_brand_coef.png", plot1, width = 10, height = 6)

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

models_list_2 <- list()
for (b in sps_list){
  print(b)
  reg_data <- analysis_data %>% filter(brand == b)
  
  # Create the formula string
  formula_str <- paste("log_e5 ~", paste(paste0(sps_list, "_neighbor_brand"), collapse = " + "),
                       "+ stations_within_5km + stations_within_10km + stations_within_15km +
                        population_within_5km + population_within_10km + population_within_15km + 
                        population_within_5km_sq + population_within_10km_sq + population_within_15km_sq  +
                        stations_per_million_pop_10km + 
                        less_than_50m_to_neighbor_phdis | date | 0 | date + stid")
  
  # Convert to a formula
  model_formula <- formula(formula_str)
  
  # Use in felm
  model_brand <- felm((formula = model_formula) ,
                      data = reg_data,
                      na.action = na.omit)
  
  model_name <- paste0("model_2_brand_", b)
  
  models_list_2[[model_name]] <- model_brand
  
  model_brand_coef <- summary(model_brand)$coef
  
  for (b2 in sps_list){
    brand_df[which(brand_df$brand == b), which( grepl(b2,names(brand_df)))] <- 
      t(model_brand_coef[which(sps_list == b2),1:4])
  }
}
  
sink(paste0("03_output/tables/", timestamp, "_brand_pair_coef.tex"))
stargazer(models_list_2, type = "latex",
          dep.var.labels = "$\\ln p^R_{ibt}$",
          column.labels = transform_strings(sps_list),
          covariate.labels = paste0("Nearest Station Brand ", transform_strings(sps_list)),
          omit = c("stations_within_5km", "stations_within_10km", "stations_within_15km", 
                   "population_within_5km", "population_within_10km", "population_within_15km",
                   "population_within_5km_sq", "population_within_10km_sq", "population_within_15km_sq", 
                   "stations_per_million_pop_10km", "less_than_50m_to_neighbor_phdis"),
          omit.stat = "all", # to omit additional statistics like R-squared, F-statistic, etc.
          single.row = FALSE,
          title = "Effect on Log Price of Having the Nearest Station Belong to Different Brands", 
          label = "",
          notes = "Standard errors are clustered at the date and station levels",
          add.lines = list( c("Controls \\& Date Fixed Effects", rep("\\checkmark", length(sps_list))), 
                            c("Observations", 
                              sapply(models_list_2, 
                                     function(model) format(summary(model)$N)))))
sink()


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

long_brand_df$brand_for_plot <- paste("Brand b:", long_brand_df$brand)
plot2 <- ggplot(long_brand_df, aes(x = coef, y = brand_type)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lower_bound, xmax = upper_bound), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "red", size = 1) +
  facet_wrap(~ brand_for_plot, scales = "free_y", nrow = 3) +  # Facet wrap with modified brand names
  theme_minimal() +
  labs(title = "95% CI of Coefficient Estimates for Each Brand Pair",
       x = "Coefficient Estimate",
       y = "Brand n:")  # Modified y-axis label
plot2

ggsave("03_output/graphs/20240206_brand_pair_coef.png", plot2, width = 10, height = 6)





saveRDS(brand_df, file = "01_data/02_processed/brand_df.rds")
