#--------------------------------------------------------------------------
# Script Name: 09_ecm_analysis.R
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

setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), "/Dropbox/gas-col"))

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

timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

legend_text_size <- 15
legend_space <- 4

# Test hypothesis of strategic pricing

spt_df <- readRDS("01_data/02_processed/spt_df.rds")
apt_df <- readRDS("01_data/02_processed/apt_df.rds")

## Lag selection
spt_df  <- readRDS("01_data/02_processed/spt_df.rds")
lag_selection_list <- as.data.frame(matrix(ncol = 2))
names(lag_selection_list) <- c("n", "AIC")
for (n in 1:29) {
  print(n)
  print(lag_selection_list)
  lag_selection_formula <- "diff_e5_1 ~ " %>%
    paste(paste(paste0("m1_diff_oil_", 1:(1+n)), collapse=" + ")) %>%
    paste(paste(paste0("diff_e5_", 2:(1+n)), collapse=" + "), sep = " + ") %>%
    paste0(" + lag_e5 + lag_oil | stid | 0 | date + stid") %>% formula()
  lag_selection_model <- felm(lag_selection_formula, data = spt_df)
  
  # Capture the AIC value
  model_aic <- AIC(lag_selection_model)
  
  # Add a new row to lag_selection_list with the current n and model AIC
  lag_selection_list <- rbind(lag_selection_list, c(n, model_aic))
}
lag_length <- lag_selection_list$n[which.min(lag_selection_list$AIC)]

## Model 1: Symmetric price transmission
pt_model_1_formula <- "diff_e5_1 ~ " %>%
  paste(paste(paste0("m1_diff_oil_", 1:(1+lag_length)), collapse=" + ")) %>%
  paste(paste(paste0("diff_e5_", 2:(1+lag_length)), collapse=" + "), sep = " + ") %>%
  paste0(" + lag_e5 + lag_oil | stid | 0 | date + stid") %>% formula()
pt_model_1 <- felm(pt_model_1_formula, data = spt_df )
crf <- cumulative_response_sym(pt_model_1)
spt_plot_1 <- cumulative_response_plot(list(crf), c("Response to \nChange in Oil Price")) +
  theme(
    legend.text = element_text(size = legend_text_size)
  )
ggsave(paste0("03_output/graphs/", timestamp, "_spt_plot_1.png"), spt_plot_1, width = 12, height = 6)
rm(spt_df)


# Model 2: Allow for asymmetry
apt_df  <- readRDS("01_data/02_processed/apt_df.rds")
pt_model_2_formula <- "diff_e5_1 ~ " %>%
  paste(paste(paste0("m2_diff_oil_pos_", 1:(1+lag_length)), collapse=" + ")) %>%
  paste(paste(paste0("m2_diff_oil_neg_", 1:(1+lag_length)), collapse=" + "), sep = " + ") %>%
  paste(paste(paste0("diff_e5_pos_", 2:(1+lag_length)), collapse=" + "), sep = " + ") %>%
  paste(paste(paste0("diff_e5_neg_", 2:(1+lag_length)), collapse=" + "), sep = " + ") %>%
  paste0("  + lag_e5 + lag_oil | stid | 0 | date + stid") %>% formula()
pt_model_2 <- felm(pt_model_2_formula, data = apt_df)
crf_pos <- cumulative_response_asym(pt_model_2, "pos")
crf_neg <- cumulative_response_asym(pt_model_2, "neg")
apt_plot_1 <- cumulative_response_plot(list(crf_pos, crf_neg), c("Response to \nIncrease in Oil Price",
                                                   "Response to \nDecrease in Oil Price")) +
  theme(
    legend.text = element_text(size = legend_text_size),
    legend.key.size = unit(legend_space, "lines")
  )
ggsave(paste0("03_output/graphs/", timestamp, "_apt_plot_1.png"), apt_plot_1, width = 12, height = 6)

pt_model_2_no_uni <- felm(pt_model_2_formula, data = apt_df %>% filter(!unilateral_mkt_pwr>0))
crf_pos_no_uni <- cumulative_response_asym(pt_model_2_no_uni, "pos")
crf_neg_no_uni <- cumulative_response_asym(pt_model_2_no_uni, "neg")

pt_model_2_uni <- felm(pt_model_2_formula, data = apt_df %>% filter(unilateral_mkt_pwr>0))
crf_pos_uni <- cumulative_response_asym(pt_model_2_uni, "pos")
crf_neg_uni <- cumulative_response_asym(pt_model_2_uni, "neg")

apt_plot_2 <- cumulative_response_plot(list(crf_pos_no_uni, crf_neg_no_uni, crf_pos_uni, crf_neg_uni), 
                         c("Response to \nIncrease in Oil Price, \nno Unilateral Market Power",
                           "Response to \nDecrease in Oil Price, \nno Unilateral Market Power",
                           "Response to \nIncrease in Oil Price, \nUnilateral Market Power",
                           "Response to \nDecrease in Oil Price, \nUnilateral Market Power")) +
  theme(
    legend.text = element_text(size = legend_text_size),
    legend.key.size = unit(legend_space, "lines")
  )
ggsave(paste0("03_output/graphs/", timestamp, "_apt_plot_2.png"), apt_plot_2, width = 12, height = 6)


pt_model_2_no_coord <- felm(pt_model_2_formula, data = apt_df %>% filter(!coordinated_mkt_pwr>0))
crf_pos_no_coord <- cumulative_response_asym(pt_model_2_no_coord, "pos")
crf_neg_no_coord <- cumulative_response_asym(pt_model_2_no_coord, "neg")

pt_model_2_coord <- felm(pt_model_2_formula, data = apt_df %>% filter(coordinated_mkt_pwr>0))
crf_pos_coord <- cumulative_response_asym(pt_model_2_coord, "pos")
crf_neg_coord <- cumulative_response_asym(pt_model_2_coord, "neg")

apt_plot_3 <- cumulative_response_plot(list(crf_pos_no_coord, crf_neg_no_coord, crf_pos_coord, crf_neg_coord), 
                         c("Response to \nIncrease in Oil Price, \nNon-Collusive Station",
                           "Response to \nDecrease in Oil Price, \nNon-Collusive Station",
                           "Response to \nIncrease in Oil Price, \nCollusive Station",
                           "Response to \nDecrease in Oil Price, \nCollusive Station")) +
  theme(
    legend.text = element_text(size = legend_text_size),
    legend.key.size = unit(legend_space, "lines")
  ) 
ggsave(paste0("03_output/graphs/", timestamp, "_apt_plot_3.png"), apt_plot_3, width = 12, height = 6)


