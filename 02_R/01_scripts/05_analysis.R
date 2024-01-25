#--------------------------------------------------------------------------
# Script Name: 05_analysis.R
# 
# Author: Marcus Hagman
# Date: 2024-01-24
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

library(tidyverse)
library(lubridate)
library(lfe)
library(zoo)
library(stargazer)
library(nlme)
library(lmtest)
#library(plyr)
source("02_R/02_functions/cumulative_response.R")
source("02_R/02_functions/cumulative_response_plot.R")



# Test hypothesis of strategic pricing

mkt_pwr_est_df <- readRDS("01_data/02_processed/mkt_pwr_est_df.rds")

model_phdis <- felm(log_e5 ~ same_brand_as_nearest_station_phdis +
                      stations_within_5km + stations_within_10km + stations_within_15km +
                      population_within_5km + population_within_10km + 
                      population_within_5km_sq + population_within_10km_sq + 
                      stations_per_million_pop_10km + 
                      less_than_50m_to_neighbor_phdis|  brand + date  | 0 | brand + date,
                              data = mkt_pwr_est_df,
                              na.action = na.omit)
model_drdis <- felm(log_e5 ~ same_brand_as_nearest_station_drdis +
                      stations_within_5km + stations_within_10km + stations_within_15km +
                      population_within_5km + population_within_10km + 
                      population_within_5km_sq + population_within_10km_sq + 
                      stations_per_million_pop_10km + 
                      less_than_50m_to_neighbor_phdis|  brand + date  | 0 | brand + date,
                    data = mkt_pwr_est_df,
                    na.action = na.omit)
model_drdur <- felm(log_e5 ~ same_brand_as_nearest_station_drdur +
                      stations_within_5km + stations_within_10km + stations_within_15km +
                      population_within_5km + population_within_10km + 
                      population_within_5km_sq + population_within_10km_sq + 
                      stations_per_million_pop_10km + 
                      less_than_50m_to_neighbor_phdis|  brand + date  | 0 | brand + date,
                    data = mkt_pwr_est_df,
                    na.action = na.omit)
model_full <- felm(log_e5 ~ same_brand_as_nearest_station_phdis +
                      same_brand_as_nearest_station_drdis +
                      same_brand_as_nearest_station_drdur +
                     stations_within_5km + stations_within_10km + stations_within_15km +
                     population_within_5km + population_within_10km + 
                     population_within_5km_sq + population_within_10km_sq + 
                     stations_per_million_pop_10km + 
                     less_than_50m_to_neighbor_phdis|  brand + date  | 0 | brand + date,
                    data = mkt_pwr_est_df,
                    na.action = na.omit)
model_phdis_coef <- summary(model_phdis)$coef
model_drdis_coef <- summary(model_drdis)$coef
model_drdur_coef <- summary(model_drdur)$coef
model_full_coef <- summary(model_full)$coef
# Use stargazer with type set to "latex"
sink("03_outputs/tables/20240125_distancecomp.tex")
stargazer(model_phdis, model_drdis, model_drdur, model_full, type = "latex",
          covariate.labels = c("Same Brand as Nearest, Straight Distance", "Same Brand as Nearest, Driving Distance",
                               "Same Brand as Nearest, Driving Duration"),
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

rm(mkt_pwr_est_df)


## Model 1: Symmetric price transmission
model_1_df  <- readRDS("01_data/02_processed/model_1_df.rds")
pt_model_1_formula <- "diff_e5_1 ~ " %>%
  paste(paste(paste0("m1_diff_oil_", 1:30), collapse=" + ")) %>%
  paste(paste(paste0("diff_e5_", 2:30), collapse=" + "), sep = " + ") %>%
  paste0(" + lag_e5 + lag_oil | stid | 0 | date + stid") %>% formula()
pt_model_1 <- felm(pt_model_1_formula, data = model_1_df )
crf <- cumulative_response_sym(pt_model_1)
spt_plot_1 <- cumulative_response_plot(list(crf), c("Response to Change in Oil Price"))
ggsave("03_outputs/figures/20240124_spt_plot_1.png", spt_plot_1, width = 10, height = 6)
rm(model_1_df)

# Model 2: Allow for asymmetry
model_2_df  <- readRDS("01_data/02_processed/model_2_df.rds")
pt_model_2_formula <- "diff_e5_1 ~ " %>%
  paste(paste(paste0("m2_diff_oil_pos_", 1:30), collapse=" + ")) %>%
  paste(paste(paste0("m2_diff_oil_neg_", 1:30), collapse=" + "), sep = " + ") %>%
  paste(paste(paste0("diff_e5_pos_", 2:30), collapse=" + "), sep = " + ") %>%
  paste(paste(paste0("diff_e5_neg_", 2:30), collapse=" + "), sep = " + ") %>%
  paste0("  + lag_e5 + lag_oil | stid | 0 | date + stid") %>% formula()
pt_model_2 <- felm(pt_model_2_formula, data = model_2_df)
crf_pos <- cumulative_response_asym(pt_model_2, "pos")
crf_neg <- cumulative_response_asym(pt_model_2, "neg")
apt_plot_1 <- cumulative_response_plot(list(crf_pos, crf_neg), c("Response to Increase in Oil Price",
                                                   "Response to Decrease in Oil Price"))
ggsave("03_outputs/figures/20240124_apt_plot_1.png", apt_plot_1, width = 10, height = 6)

pt_model_2_no_uni <- felm(pt_model_2_formula, data = model_2_df %>% filter(!unilateral_mkt_pwr>0))
crf_pos_no_uni <- cumulative_response_asym(pt_model_2_no_uni, "pos")
crf_neg_no_uni <- cumulative_response_asym(pt_model_2_no_uni, "neg")

pt_model_2_uni <- felm(pt_model_2_formula, data = model_2_df %>% filter(unilateral_mkt_pwr>0))
crf_pos_uni <- cumulative_response_asym(pt_model_2_uni, "pos")
crf_neg_uni <- cumulative_response_asym(pt_model_2_uni, "neg")

apt_plot_2 <- cumulative_response_plot(list(crf_pos_no_uni, crf_neg_no_uni, crf_pos_uni, crf_neg_uni), 
                         c("Response to Increase in Oil Price, \nno Unilateral Market Power",
                           "Response to Decrease in Oil Price, \nno Unilateral Market Power",
                           "Response to Increase in Oil Price, \nUnilateral Market Power",
                           "Response to Decrease in Oil Price, \nUnilateral Market Power"))
ggsave("03_outputs/figures/20240124_apt_plot_2.png", apt_plot_2, width = 10, height = 6)


pt_model_2_no_coord <- felm(pt_model_2_formula, data = model_2_df %>% filter(!coordinated_mkt_pwr>0))
crf_pos_no_coord <- cumulative_response_asym(pt_model_2_no_coord, "pos")
crf_neg_no_coord <- cumulative_response_asym(pt_model_2_no_coord, "neg")

pt_model_2_coord <- felm(pt_model_2_formula, data = model_2_df %>% filter(coordinated_mkt_pwr>0))
crf_pos_coord <- cumulative_response_asym(pt_model_2_coord, "pos")
crf_neg_coord <- cumulative_response_asym(pt_model_2_coord, "neg")

apt_plot_3 <- cumulative_response_plot(list(crf_pos_no_coord, crf_neg_no_coord, crf_pos_coord, crf_neg_coord), 
                         c("Response to Increase in Oil Price, \nNo Unilateral Market Power",
                           "Response to Decrease in Oil Price, \nNo Unilateral Market Power",
                           "Response to Increase in Oil Price, \nUnilateral Market Power",
                           "Response to Decrease in Oil Price, \nUnilateral Market Power"))
ggsave("03_outputs/figures/20240124_apt_plot_3.png", apt_plot_3, width = 10, height = 6)


