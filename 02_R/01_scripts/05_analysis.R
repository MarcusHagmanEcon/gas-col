#--------------------------------------------------------------------------
# Script Name: 05_analysis.R
# 
# Author: Marcus Hagman
# Date: 2023-10-18
# 
# Purpose: This script produces all the outputs, including tables and graphs.
#
# Input: - 01_data/02_processed/analysis_data.rds
# 
# Output: Tables and graphs
#
# Instructions: 
#
# Revision History:
#--------------------------------------------------------------------------

rm(list=ls())

setwd("C:/Users/marcu/Documents/gas-col")

library(tidyverse)
library(lubridate)
library(lfe)
library(zoo)
library(stargazer)
library(nlme)
library(lmtest)
#library(plyr)
source("02_R/02_functions/cumulative_response.R")

# Load data
analysis_data <- readRDS("01_data/02_processed/analysis_data.rds")
gas_stations <- readRDS("01_data/02_processed/cleaned_gas_stations.rds")
brand_df <- readRDS("01_data/02_processed/brand_df.rds")

# Big brands
big_brands <- brand_df %>% mutate(brand = as.character(brand)) %>% select(brand) %>%
  pull()
# SPS
sps_brands <- brand_df %>% filter(t_stat > 2) %>% mutate(brand = as.character(brand)) %>% select(brand) %>%
  pull()

analysis_data <- analysis_data %>% rename(oil = brent) %>%
  mutate(e5 = e5/100)

analysis_data <- analysis_data %>% 
  mutate(unilateral_mkt_pwr = ifelse((brand %in% sps_brands) & 
    (brand_of_nearest_station_phdis == brand), brand_df[which(brand_df[,1] == brand),
                                                            paste0(brand, "_t_stat")][[1]] > 1.96,0),
    coordinated_mkt_pwr = ifelse((brand %in% sps_brands) & 
      (brand_of_nearest_station_phdis %in% sps_brands) & 
      (brand != brand_of_nearest_station_phdis), brand_df[which(brand_df[,1] == brand),
       paste0(brand_of_nearest_station_phdis, "_t_stat")][[1]] > 1.96,0))

analysis_data <- analysis_data %>% mutate(weekday = weekdays(date))

analysis_data_big <- analysis_data %>% filter(brand %in% big_brands)

analysis_data_big <- analysis_data_big %>%
  group_by(stid) %>%
  do({
    model <- lm(e5 ~ oil, data = .)
    residuals <- broom::augment(model)$`.resid`
    data.frame(., ect = residuals)
  })

analysis_data_big <- analysis_data_big %>%
  mutate(lag_ect = ifelse(stid == lag(stid, n = 1), lag(ect), NA))

#analysis_data_mondays <- analysis_data_big %>% filter(weekday == "Monday")

for (i in 1:22) {
  analysis_data_big <- analysis_data_big %>%
    mutate(!!paste0("diff_e5_", i) := ifelse(stid == lag(stid, n = i), 
                                             lag(e5, n = i - 1) - lag(e5, n = i), 
                                             NA),
           !!paste0("diff_oil_", i) := ifelse(stid == lag(stid, n = i), 
                                             lag(oil, n = i - 1) - lag(oil, n = i), 
                                             NA))
  analysis_data_big[[paste0("diff_oil_pos_", i)]] <- pmax(analysis_data_big[[paste0("diff_oil_", i)]],0)
  analysis_data_big[[paste0("diff_oil_neg_", i)]] <- pmin(analysis_data_big[[paste0("diff_oil_", i)]],0)
  analysis_data_big[[paste0("diff_oil_uni_", i)]] <- 
    ifelse(analysis_data_big$unilateral_mkt_pwr > 0, analysis_data_big[[paste0("diff_oil_", i)]], 0)
  analysis_data_big[[paste0("diff_oil_no_uni_", i)]] <- 
    ifelse(analysis_data_big$unilateral_mkt_pwr == 0, analysis_data_big[[paste0("diff_oil_", i)]], 0)
  analysis_data_big[[paste0("diff_oil_coord_", i)]] <- 
    ifelse(analysis_data_big$coordinated_mkt_pwr > 0, analysis_data_big[[paste0("diff_oil_", i)]], 0)
  analysis_data_big[[paste0("diff_oil_no_coord_", i)]] <- 
    ifelse(analysis_data_big$coordinated_mkt_pwr == 0, analysis_data_big[[paste0("diff_oil_", i)]], 0)
  
  analysis_data_big[[paste0("diff_oil_uni_pos_", i)]] <- 
    ifelse(analysis_data_big$unilateral_mkt_pwr > 0, analysis_data_big[[paste0("diff_oil_pos_", i)]], 0)
  analysis_data_big[[paste0("diff_oil_no_uni_pos_", i)]] <- 
    ifelse(analysis_data_big$unilateral_mkt_pwr == 0, analysis_data_big[[paste0("diff_oil_pos_", i)]], 0)
  analysis_data_big[[paste0("diff_oil_coord_pos_", i)]] <- 
    ifelse(analysis_data_big$coordinated_mkt_pwr > 0, analysis_data_big[[paste0("diff_oil_pos_", i)]], 0)
  analysis_data_big[[paste0("diff_oil_no_coord_pos_", i)]] <- 
    ifelse(analysis_data_big$coordinated_mkt_pwr == 0, analysis_data_big[[paste0("diff_oil_pos_", i)]], 0)
  
  analysis_data_big[[paste0("diff_oil_uni_neg_", i)]] <- 
    ifelse(analysis_data_big$unilateral_mkt_pwr > 0, analysis_data_big[[paste0("diff_oil_neg_", i)]], 0)
  analysis_data_big[[paste0("diff_oil_no_uni_neg_", i)]] <- 
    ifelse(analysis_data_big$unilateral_mkt_pwr == 0, analysis_data_big[[paste0("diff_oil_neg_", i)]], 0)
  analysis_data_big[[paste0("diff_oil_coord_neg_", i)]] <- 
    ifelse(analysis_data_big$coordinated_mkt_pwr > 0, analysis_data_big[[paste0("diff_oil_neg_", i)]], 0)
  analysis_data_big[[paste0("diff_oil_no_coord_neg_", i)]] <- 
    ifelse(analysis_data_big$coordinated_mkt_pwr == 0, analysis_data_big[[paste0("diff_oil_neg_", i)]], 0)
}








# for (w in 1:3) {
#   print(w)
#   analysis_data_mondays <- analysis_data_mondays %>%
#     mutate(!!paste0("diff_wk_e5_", w) := ifelse(stid == lag(stid, n = w), 
#                                                 lag(e5, n = w - 1) - lag(e5, n = w), 
#                                                 NA),
#            !!paste0("diff_wk_oil_", w) := ifelse(stid == lag(stid, n = w), 
#                                                  lag(oil, n = w - 1) - lag(oil, n = w), 
#                                                  NA))
#   analysis_data_mondays[[paste0("diff_wk_oil_pos_", w)]] <- pmax(analysis_data_mondays[[paste0("diff_wk_oil_", w)]],0)
#   analysis_data_mondays[[paste0("diff_wk_oil_neg_", w)]] <- pmin(analysis_data_mondays[[paste0("diff_wk_oil_", w)]],0)
# }
# analysis_data_mondays <- analysis_data_mondays %>%
#   mutate(lag_ect = ifelse(stid == lag(stid, n = i), lag(ect), NA))

# Unique dates
#unique_dates <- sort(unique(analysis_data$date))



# Test hypothesis of strategic pricing
# TODO: Richer set of controls
model_phdis <- felm(log_e5 ~ same_brand_as_nearest_station_phdis +
                      stations_within_5km + stations_within_10km + stations_within_15km +
                      population_within_5km + population_within_10km + 
                      population_within_5km_sq + population_within_10km_sq + 
                      stations_per_million_pop_10km + 
                      less_than_50m_to_neighbor_phdis|  brand + date  | 0 | brand + date,
                              data = analysis_data,
                              na.action = na.omit)
model_drdis <- felm(log_e5 ~ same_brand_as_nearest_station_drdis +
                      stations_within_5km + stations_within_10km + stations_within_15km +
                      population_within_5km + population_within_10km + 
                      population_within_5km_sq + population_within_10km_sq + 
                      stations_per_million_pop_10km + 
                      less_than_50m_to_neighbor_phdis|  brand + date  | 0 | brand + date,
                    data = analysis_data,
                    na.action = na.omit)
model_drdur <- felm(log_e5 ~ same_brand_as_nearest_station_drdur +
                      stations_within_5km + stations_within_10km + stations_within_15km +
                      population_within_5km + population_within_10km + 
                      population_within_5km_sq + population_within_10km_sq + 
                      stations_per_million_pop_10km + 
                      less_than_50m_to_neighbor_phdis|  brand + date  | 0 | brand + date,
                    data = analysis_data,
                    na.action = na.omit)
model_full <- felm(log_e5 ~ same_brand_as_nearest_station_phdis +
                      same_brand_as_nearest_station_drdis +
                      same_brand_as_nearest_station_drdur +
                     stations_within_5km + stations_within_10km + stations_within_15km +
                     population_within_5km + population_within_10km + 
                     population_within_5km_sq + population_within_10km_sq + 
                     stations_per_million_pop_10km + 
                     less_than_50m_to_neighbor_phdis|  brand + date  | 0 | brand + date,
                    data = analysis_data,
                    na.action = na.omit)
model_phdis_coef <- summary(model_phdis)$coef
model_drdis_coef <- summary(model_drdis)$coef
model_drdur_coef <- summary(model_drdur)$coef
model_full_coef <- summary(model_full)$coef
# Use stargazer with type set to "latex"
sink("03_outputs/tables/20231207_distancecomp.tex")
stargazer(model_phdis, model_drdis, model_drdur, model_full, type = "latex",
          covariate.labels = c("Same Brand as Nearest, Straight Distance", "Same Brand as Nearest, Driving Distance", "Same Brand as Nearest, Driving Duration"),
          omit = c("stations_within_5km", "stations_within_10km", "stations_within_15km", 
                   "population_within_5km", "population_within_10km",
                     "population_within_5km_sq", "population_within_10km_sq", 
                   "stations_per_million_pop_10km", "less_than_50m_to_neighbor_phdis"),
          se = list(model_phdis_coef[,2],model_drdis_coef[,2],model_drdur_coef[,2], model_full_coef[,2]), # assuming second column contains SEs
          omit.stat = "all", # to omit additional statistics like R-squared, F-statistic, etc.
          single.row = FALSE,
          title = "", 
          label = "") 
sink()


## Model 1: Simple price transmission
pt_model_1_formula <- "diff_e5_1 ~ " %>%
  paste(paste(paste0("diff_oil_", 1:22), collapse=" + ")) %>%
  paste(paste(paste0("diff_e5_", 2:22), collapse=" + "), sep = " + ") %>%
  paste0(" + lag_ect | stid | 0 | date + stid") %>% formula()
pt_model_1 <- felm(pt_model_1_formula, data = analysis_data_big %>%
                     select(starts_with("diff_oil_"), starts_with("diff_e5_"), lag_ect, stid, date) )
summary(pt_model_1)
pt_model_1_crf <- cumulative_response(pt_model_1, 1:22, "all")
pt_model_1_crf
pt_model_1_crf$lag <- seq_len(nrow(pt_model_1_crf)) - 1
pt_model_1_crf_plot <- ggplot(pt_model_1_crf, aes(x = lag, y = all_coef)) +
  geom_point() +  # Add points
  geom_line() +  # Add lines connecting the points
  geom_errorbar(aes(ymin = all_coef - 1.96 * all_se, ymax = all_coef + 1.96 * all_se), width = 0.1) +
  labs(x = "Days After Change in Oil Price", y = "Cumulative Effect on Gasoline price (95% Confidence Interval)") +
  theme_minimal()
pt_model_1_crf_plot
ggsave("03_outputs/figures/20231211_pt_figure_1.png", pt_model_1_crf_plot, width = 10, height = 6)


# Model 2: Allow for asymmetry
pt_model_2_formula <- "diff_e5_1 ~ " %>%
  paste(paste(paste0("diff_oil_pos_", 1:22), collapse=" + ")) %>%
  paste(paste(paste0("diff_oil_neg_", 1:22), collapse=" + "), sep = " + ") %>%
  paste(paste(paste0("diff_e5_", 2:22), collapse=" + "), sep = " + ") %>%
  paste0(" + lag_ect | stid | 0 | date + stid") %>% formula()
pt_model_2 <- felm(pt_model_2_formula, data = analysis_data_big %>%
                     select(starts_with("diff_oil_pos_"), starts_with("diff_oil_neg_"), starts_with("diff_e5_"), lag_ect, stid, date))
pt_model_2_crf <- cumulative_response(pt_model_2, 1:22, "up") %>% 
  cbind(cumulative_response(pt_model_2, 23:44, "down"))
pt_model_2_crf$lag <- seq_len(nrow(pt_model_2_crf)) - 1
# Define a small horizontal offset
offset <- 0.1
pt_model_2_crf_plot <- ggplot() +
  # Plotting the first series (up_coef with up_se) with an offset, using triangles for "up"
  geom_point(data = pt_model_2_crf, aes(x = lag - offset, y = up_coef, color = "Oil Price Increase")) +
  geom_line(data = pt_model_2_crf, aes(x = lag - offset, y = up_coef, color = "Oil Price Increase")) +
  geom_errorbar(data = pt_model_2_crf, aes(x = lag - offset, ymin = up_coef - 1.96 * up_se, ymax = up_coef + 1.96 * up_se, color = "Oil Price Increase"), width = 0.1) +
  
  # Plotting the second series (down_coef with down_se), using upside-down triangles for "down"
  geom_point(data = pt_model_2_crf, aes(x = lag, y = down_coef, color = "Oil Price Decrease")) +
  geom_line(data = pt_model_2_crf, aes(x = lag, y = down_coef, color = "Oil Price Decrease")) +
  geom_errorbar(data = pt_model_2_crf, aes(x = lag, ymin = down_coef - 1.96 * down_se, ymax = down_coef + 1.96 * down_se, color = "Oil Price Decrease"), width = 0.1) +
  
  # Labels and theme
  labs(x = "Days After Change in Oil Price", y = "Cumulative Effect on Gasoline price (95% Confidence Interval)", color = "Series") +
  theme_minimal() +
  # Ensure that the legend displays the shapes correctly
  scale_shape_manual(values = c(17, 25))
pt_model_2_crf_plot
ggsave("03_outputs/figures/20231211_pt_figure_2.png", pt_model_2_crf_plot, width = 10, height = 6)


# Model 3: Unilateral Market Power
pt_model_3_formula <- "diff_e5_1 ~ " %>%
  paste(paste(paste0("diff_oil_uni_", 1:22), collapse=" + ")) %>%
  paste(paste(paste0("diff_oil_no_uni_", 1:22), collapse=" + "), sep = " + ") %>%
  paste(paste(paste0("diff_e5_", 2:22), collapse=" + "), sep = " + ") %>%
  paste0(" + lag_ect | stid | 0 | date + stid") %>% formula()
pt_model_3 <- felm(pt_model_3_formula, data = analysis_data_big)
pt_model_3_crf <- cumulative_response(pt_model_3, 1:22, "up") %>% 
  cbind(cumulative_response(pt_model_3, 23:44, "down"))
pt_model_3_crf$lag <- seq_len(nrow(pt_model_3_crf)) - 1
# Define a small horizontal offset
offset <- 0.1
pt_model_3_crf_plot <- ggplot() +
  # Plotting the first series (up_coef with up_se) with an offset, using triangles for "up"
  geom_point(data = pt_model_3_crf, aes(x = lag - offset, y = up_coef, color = "Unilateral Market Power")) +
  geom_line(data = pt_model_3_crf, aes(x = lag - offset, y = up_coef, color = "Unilateral Market Power")) +
  geom_errorbar(data = pt_model_3_crf, aes(x = lag - offset, ymin = up_coef - 1.96 * up_se, ymax = up_coef + 1.96 * up_se, color = "Unilateral Market Power"), width = 0.1) +
  
  # Plotting the second series (down_coef with down_se), using upside-down triangles for "down"
  geom_point(data = pt_model_3_crf, aes(x = lag, y = down_coef, color = "No Unilateral Market Power")) +
  geom_line(data = pt_model_3_crf, aes(x = lag, y = down_coef, color = "No Unilateral Market Power")) +
  geom_errorbar(data = pt_model_3_crf, aes(x = lag, ymin = down_coef - 1.96 * down_se, ymax = down_coef + 1.96 * down_se, color = "No Unilateral Market Power"), width = 0.1) +
  
  # Labels and theme
  labs(x = "Days After Change in Oil Price", y = "Cumulative Effect on Gasoline price (95% Confidence Interval)", color = "Series") +
  theme_minimal() +
  # Ensure that the legend displays the shapes correctly
  scale_shape_manual(values = c(17, 25))
pt_model_3_crf_plot
ggsave("03_outputs/figures/20231211_pt_figure_3.png", pt_model_3_crf_plot, width = 10, height = 6)


# Model 4: Coordinated Market Power
pt_model_4_formula <- "diff_e5_1 ~ " %>%
  paste(paste(paste0("diff_oil_coord_", 1:22), collapse=" + ")) %>%
  paste(paste(paste0("diff_oil_no_coord_", 1:22), collapse=" + "), sep = " + ") %>%
  paste(paste(paste0("diff_e5_", 2:22), collapse=" + "), sep = " + ") %>%
  paste0(" + lag_ect | stid | 0 | date + stid") %>% formula()
pt_model_4 <- felm(pt_model_4_formula, data = analysis_data_big)
pt_model_4_crf <- cumulative_response(pt_model_4, 1:22, "up") %>% 
  cbind(cumulative_response(pt_model_4, 23:44, "down"))
pt_model_4_crf$lag <- seq_len(nrow(pt_model_3_crf)) - 1
# Define a small horizontal offset
offset <- 0.1
pt_model_4_crf_plot <- ggplot() +
  # Plotting the first series (up_coef with up_se) with an offset, using triangles for "up"
  geom_point(data = pt_model_4_crf, aes(x = lag - offset, y = up_coef, color = "Coordinated Market Power")) +
  geom_line(data = pt_model_4_crf, aes(x = lag - offset, y = up_coef, color = "Coordinated Market Power")) +
  geom_errorbar(data = pt_model_4_crf, aes(x = lag - offset, ymin = up_coef - 1.96 * up_se, ymax = up_coef + 1.96 * up_se, color = "Coordinated Market Power"), width = 0.1) +
  
  # Plotting the second series (down_coef with down_se), using upside-down triangles for "down"
  geom_point(data = pt_model_4_crf, aes(x = lag, y = down_coef, color = "No Coordinated Market Power")) +
  geom_line(data = pt_model_4_crf, aes(x = lag, y = down_coef, color = "No Coordinated Market Power")) +
  geom_errorbar(data = pt_model_4_crf, aes(x = lag, ymin = down_coef - 1.96 * down_se, ymax = down_coef + 1.96 * down_se, color = "No Coordinated Market Power"), width = 0.1) +
  
  # Labels and theme
  labs(x = "Days After Change in Oil Price", y = "Cumulative Effect on Gasoline price (95% Confidence Interval)", color = "Series") +
  theme_minimal() +
  # Ensure that the legend displays the shapes correctly
  scale_shape_manual(values = c(17, 25))
pt_model_4_crf_plot
ggsave("03_outputs/figures/20231211_pt_figure_4.png", pt_model_4_crf_plot, width = 10, height = 6)


# Model 5: Unilateral Market Power asymmetry
pt_model_5_formula <- "diff_e5_1 ~ " %>%
  paste(paste(paste0("diff_oil_uni_pos_", 1:22), collapse=" + ")) %>%
  paste(paste(paste0("diff_oil_no_uni_pos_", 1:22), collapse=" + "), sep = " + ") %>%
  paste(paste(paste0("diff_oil_uni_neg_", 1:22), collapse=" + "), sep = " + ") %>%
  paste(paste(paste0("diff_oil_no_uni_neg_", 1:22), collapse=" + "), sep = " + ") %>%
  paste(paste(paste0("diff_e5_", 2:22), collapse=" + "), sep = " + ") %>%
  paste0(" + lag_ect | stid | 0 | date + stid") %>% formula()
pt_model_5 <- felm(pt_model_5_formula, data = analysis_data_big %>%
                     select(starts_with("diff_oil_uni_pos_"), 
                            starts_with("diff_oil_no_uni_pos_"),
                            starts_with("diff_oil_uni_neg_"), 
                            starts_with("diff_oil_no_uni_neg_"),
                            starts_with("diff_e5_"), lag_ect, stid, date))
pt_model_5_crf <- cumulative_response(pt_model_5, 1:22, "uni_up") %>% 
  cbind(cumulative_response(pt_model_5, 23:44, "no_uni_up"))%>% 
  cbind(cumulative_response(pt_model_5, 45:66, "uni_down"))%>% 
  cbind(cumulative_response(pt_model_5, 67:88, "no_uni_down"))
pt_model_5_crf$lag <- seq_len(nrow(pt_model_5_crf)) - 1
# Define a small horizontal offset
offset <- 0.1
pt_model_5_crf_plot <- ggplot() +
  geom_point(data = pt_model_5_crf, aes(x = lag - offset, y = uni_up_coef, color = "Coordinated Market Power, Up")) +
  geom_line(data = pt_model_5_crf, aes(x = lag - offset, y = uni_up_coef, color = "Coordinated Market Power, Up")) +
  geom_errorbar(data = pt_model_5_crf, aes(x = lag - offset, 
                                           ymin = uni_up_coef - 1.96 * uni_up_se, 
                                           ymax = uni_up_coef + 1.96 * uni_up_se, 
                                           color = "Coordinated Market Power, Up"), width = 0.1) +
  
  geom_point(data = pt_model_5_crf, aes(x = lag - offset, y = no_uni_up_coef, color = "Not Coordinated Market Power, Up")) +
  geom_line(data = pt_model_5_crf, aes(x = lag - offset, y = no_uni_up_coef, color = "Not Coordinated Market Power, Up")) +
  geom_errorbar(data = pt_model_5_crf, aes(x = lag - offset, 
                                           ymin = no_uni_up_coef - 1.96 * no_uni_up_se, 
                                           ymax = no_uni_up_coef + 1.96 * no_uni_up_se, 
                                           color = "Not Coordinated Market Power, Up"), width = 0.1) +
  
  geom_point(data = pt_model_5_crf, aes(x = lag - offset, y = uni_down_coef, color = "Coordinated Market Power, Down")) +
  geom_line(data = pt_model_5_crf, aes(x = lag - offset, y = uni_down_coef, color = "Coordinated Market Power, Down")) +
  geom_errorbar(data = pt_model_5_crf, aes(x = lag - offset, 
                                           ymin = uni_down_coef - 1.96 * uni_down_se, 
                                           ymax = uni_down_coef + 1.96 * uni_down_se, 
                                           color = "Coordinated Market Power, Down"), width = 0.1) +
  
  geom_point(data = pt_model_5_crf, aes(x = lag - offset, y = no_uni_down_coef, color = "No Coordinated Market Power, Down")) +
  geom_line(data = pt_model_5_crf, aes(x = lag - offset, y = no_uni_down_coef, color = "No Coordinated Market Power, Down")) +
  geom_errorbar(data = pt_model_5_crf, aes(x = lag - offset, 
                                           ymin = no_uni_down_coef - 1.96 * no_uni_down_se, 
                                           ymax = no_uni_down_coef + 1.96 * no_uni_down_se, 
                                           color = "No Coordinated Market Power, Down"), width = 0.1) +
  
  
  # Labels and theme
  labs(x = "Days After Change in Oil Price", y = "Cumulative Effect on Gasoline price (95% Confidence Interval)", color = "Series") +
  theme_minimal() +
  # Ensure that the legend displays the shapes correctly
  scale_shape_manual(values = c(17, 25))
pt_model_5_crf_plot
ggsave("03_outputs/figures/20231211_pt_figure_5.png", pt_model_5_crf_plot, width = 10, height = 6)


# Model 6: Coordinated Market Power asymmetry
pt_model_6_formula <- "diff_e5_1 ~ " %>%
  paste(paste(paste0("diff_oil_coord_pos_", 1:22), collapse=" + ")) %>%
  paste(paste(paste0("diff_oil_no_coord_pos_", 1:22), collapse=" + "), sep = " + ") %>%
  paste(paste(paste0("diff_oil_coord_neg_", 1:22), collapse=" + "), sep = " + ") %>%
  paste(paste(paste0("diff_oil_no_coord_neg_", 1:22), collapse=" + "), sep = " + ") %>%
  paste(paste(paste0("diff_e5_", 2:22), collapse=" + "), sep = " + ") %>%
  paste0(" + lag_ect | stid | 0 | date + stid") %>% formula()
pt_model_6 <- felm(pt_model_6_formula, data = analysis_data_big  %>%
                     select(starts_with("diff_oil_coord_pos_"), 
                            starts_with("diff_oil_no_coord_pos_"),
                            starts_with("diff_oil_coord_neg_"), 
                            starts_with("diff_oil_no_coord_neg_"),
                            starts_with("diff_e5_"), lag_ect, stid, date))
pt_model_6_crf <- cumulative_response(pt_model_6, 1:22, "coord_up") %>% 
  cbind(cumulative_response(pt_model_6, 23:44, "no_coord_up"))%>% 
  cbind(cumulative_response(pt_model_6, 45:66, "coord_down"))%>% 
  cbind(cumulative_response(pt_model_6, 67:88, "no_coord_down"))
pt_model_6_crf$lag <- seq_len(nrow(pt_model_6_crf)) - 1
# Define a small horizontal offset
offset <- 0.1
pt_model_6_crf_plot <- ggplot() +
  geom_point(data = pt_model_6_crf, aes(x = lag - offset, y = coord_up_coef, color = "Coordinated Market Power, Up")) +
  geom_line(data = pt_model_6_crf, aes(x = lag - offset, y = coord_up_coef, color = "Coordinated Market Power, Up")) +
  geom_errorbar(data = pt_model_6_crf, aes(x = lag - offset, 
                                           ymin = coord_up_coef - 1.96 * coord_up_se, 
                                           ymax = coord_up_coef + 1.96 * coord_up_se, 
                                           color = "Coordinated Market Power, Up"), width = 0.1) +
  
  geom_point(data = pt_model_6_crf, aes(x = lag - offset, y = no_coord_up_coef, color = "Not Coordinated Market Power, Up")) +
  geom_line(data = pt_model_6_crf, aes(x = lag - offset, y = no_coord_up_coef, color = "Not Coordinated Market Power, Up")) +
  geom_errorbar(data = pt_model_6_crf, aes(x = lag - offset, 
                                           ymin = no_coord_up_coef - 1.96 * no_coord_up_se, 
                                           ymax = no_coord_up_coef + 1.96 * no_coord_up_se, 
                                           color = "Not Coordinated Market Power, Up"), width = 0.1) +
  
  geom_point(data = pt_model_6_crf, aes(x = lag - offset, y = coord_down_coef, color = "Coordinated Market Power, Down")) +
  geom_line(data = pt_model_6_crf, aes(x = lag - offset, y = coord_down_coef, color = "Coordinated Market Power, Down")) +
  geom_errorbar(data = pt_model_6_crf, aes(x = lag - offset, 
                                           ymin = coord_down_coef - 1.96 * coord_down_se, 
                                           ymax = coord_down_coef + 1.96 * coord_down_se, 
                                           color = "Coordinated Market Power, Down"), width = 0.1) +
  
  geom_point(data = pt_model_6_crf, aes(x = lag - offset, y = no_coord_down_coef, color = "No Coordinated Market Power, Down")) +
  geom_line(data = pt_model_6_crf, aes(x = lag - offset, y = no_coord_down_coef, color = "No Coordinated Market Power, Down")) +
  geom_errorbar(data = pt_model_6_crf, aes(x = lag - offset, 
                                           ymin = no_coord_down_coef - 1.96 * no_coord_down_se, 
                                           ymax = no_coord_down_coef + 1.96 * no_coord_down_se, 
                                           color = "No Coordinated Market Power, Down"), width = 0.1) +
  
  
  # Labels and theme
  labs(x = "Days After Change in Oil Price", y = "Cumulative Effect on Gasoline price (95% Confidence Interval)", color = "Series") +
  theme_minimal() +
  # Ensure that the legend displays the shapes correctly
  scale_shape_manual(values = c(17, 25))
pt_model_6_crf_plot
ggsave("03_outputs/figures/20231211_pt_figure_6.png", pt_model_6_crf_plot, width = 10, height = 6)