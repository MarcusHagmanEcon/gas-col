#--------------------------------------------------------------------------
# Script Name: 11_effect_openings.R
# 
# Author: Marcus Hagman
# Date: 2024-04-09
# 
# Purpose: I use Callaway and Sant'Anna to find the effect of nearby gas station
#       openings on prices.
#
# Input: - 01_data/02_processed/cleaned_gas_prices.rds
#        - 01_data/02_processed/cleaned_gas_stations.rds
# 
# Output: - _nearby_opening.png
#         - _openings_att.tex
#         - _openings_n.tex
#
# Instructions: 
#
# Revision History:
#--------------------------------------------------------------------------

rm(list=ls())

setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), "/Dropbox/gas-col"))

library(tidyverse)
library(did)
library(gridExtra)
library(xtable)

timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

prices <- readRDS("01_data/02_processed/cleaned_gas_prices.rds")
stations <- readRDS("01_data/02_processed/cleaned_gas_stations.rds")

stid_list <- prices$stid %>% unique()

day_interval <- 28

#Calculate weekly average prices
prices_weekly <- prices %>% mutate(int_date = as.integer(date)) %>%
  filter(int_date %% day_interval == 4)

# Calculate the open and close date of each station
open_close <- prices %>%
  group_by(stid) %>%
  summarize(
    first_price_date = (ceiling(as.integer(as.Date(min(date[!is.na(log_e5)], na.rm = TRUE))) / 
                                  day_interval) * day_interval) %>% as.Date(origin = "1970-01-01"),
    last_price_date = (ceiling(as.integer(as.Date(max(date[!is.na(log_e5)], na.rm = TRUE))) / 
                                  day_interval) * day_interval) %>% as.Date(origin = "1970-01-01")
  )

prices_weekly_reg <- prices_weekly %>% left_join(stations %>% 
  left_join(open_close, by = 
            c("stid_of_nearest_station_phdis" = "stid")),
            by = c("stid" = "id")) %>%
  rename(closest_first_price_date = first_price_date,
         closest_last_price_date = last_price_date) %>%
  mutate(first_price_date = min(date),
         last_price_date = max(date)) %>% 
  mutate(treated_open = (closest_first_price_date > as.Date("2014-06-08") + 3*day_interval) &
           (closest_first_price_date - first_price_date) > day_interval,
         treated_close = (closest_last_price_date < as.Date("2016-05-02") - 3*day_interval) &
           (last_price_date - closest_last_price_date) > day_interval) %>%
  mutate(nearest_station_open = ifelse(treated_open, date > closest_first_price_date, 0) ) %>%
  mutate(brand_neighbor_concat = paste(brand, brand_of_nearest_station_phdis, sep = "_")) %>%
  mutate(date = as.integer(date),
         stid_num = which(stid_list == stid[1]),
         open_treat_group = ifelse(treated_open, as.integer(closest_first_price_date), 0),
         open_treat_group_nc = 
           ifelse(treated_open & 
                    brand_neighbor_concat %in% 
                    c("aral_aral", "aral_shell", "shell_aral", "shell_shell", "shell_total",
                      "total_aral", "total_shell", "total_total"), as.integer(closest_first_price_date), 0),
         open_treat_group_ncsb = 
           ifelse(treated_open & 
                    brand_neighbor_concat %in% 
                    c("aral_aral", "shell_shell", "total_total"), as.integer(closest_first_price_date), 0),
         open_treat_group_ncc = 
           ifelse(treated_open & 
                    brand_neighbor_concat %in% 
                    c("aral_shell", "shell_aral", "shell_total",
                      "total_aral", "total_shell"), as.integer(closest_first_price_date), 0),
         open_treat_group_c = 
           ifelse(treated_open & 
                    !(brand_neighbor_concat %in% 
                    c("aral_aral", "aral_shell", "shell_aral", "shell_shell", "shell_total",
                      "total_aral", "total_shell", "total_total") ), as.integer(closest_first_price_date), 0),
         close_treat_group = ifelse(treated_close, as.integer(closest_last_price_date), 0))


# Run event studies
source("02_R/02_functions/event_study_graph.R")
set.seed(1234)
c_open_model <- att_gt(yname = "log_e5",
                         tname = "date",
                         idname = "stid_num",
                         gname = "open_treat_group_c",
                         xformla = ~ 1,
                         data = prices_weekly_reg,
                         allow_unbalanced_panel = TRUE)
c_open_agges <- aggte(c_open_model, type = "dynamic")
c_graph <- event_study_graph(c_open_agges, -360, 360, 
                  "Effect on Log Price of a Nearby Station Opening, Competitive Opening")

ncsb_open_model <- att_gt(yname = "log_e5",
                        tname = "date",
                        idname = "stid_num",
                        gname = "open_treat_group_ncsb",
                        xformla = ~ 1,
                        data = prices_weekly_reg,
                        allow_unbalanced_panel = TRUE)
ncsb_open_agges <- aggte(ncsb_open_model, type = "dynamic", na.rm = TRUE)
ncsb_graph <- event_study_graph(ncsb_open_agges, -360, 360,
                  "Effect on Log Price of a Nearby Station Opening, Noncompetitive, Same Brand")

ncc_open_model <- att_gt(yname = "log_e5",
                        tname = "date",
                        idname = "stid_num",
                        gname = "open_treat_group_ncc",
                        xformla = ~ 1,
                        data = prices_weekly_reg,
                        allow_unbalanced_panel = TRUE)
ncc_open_agges <- aggte(ncc_open_model, type = "dynamic", na.rm = TRUE)
ncc_graph <- event_study_graph(ncc_open_agges, -360, 360, 
                  "Effect on Log Price of a Nearby Station Opening, Noncompetitive, Collusive")



png(paste0("03_output/graphs/", timestamp, "_nearby_opening.png"), width = 800, height = 1200)
layout_matrix <- rbind(c(1, 1),
                       c(NA, NA),
                       c(2, 2),
                       c(NA, NA),
                       c(3, 3))
grid.arrange(grobs = list(c_graph, ncsb_graph, ncc_graph), 
             layout_matrix = layout_matrix,
             heights = unit(c(4, 1, 4, 1, 4), c("null", "lines", "null", "lines", "null")))
dev.off()




# Present ATT in table
results_df <- data.frame(estimates = c(c_open_agges$overall.att,
                                     ncsb_open_agges$overall.att, ncc_open_agges$overall.att),
                       standard_errors = c(c_open_agges$overall.se,
                                           ncsb_open_agges$overall.se, ncc_open_agges$overall.se))
table_results <- rbind(
  c("Category of Opening", "Competitive", "Same Brand", "Collusive"),
  c("ATT Estimate", round(results_df$estimates, digits = 5)),
  c("Std. Error", paste0("(", round(results_df$standard_errors, digits = 5), ")")),
  c("95\\% Confidence Interval", paste0("[", round(results_df$estimates - 1.96*results_df$standard_errors, digits = 5), ", ", round(results_df$estimates + 1.96*results_df$standard_errors, digits = 5), "]"))
)
xt_result <- xtable(table_results, include.caption = FALSE, label = "tab:results",
             include.rownames = FALSE, include.colnames = FALSE,)
align(xt_result) <- c("l", "l", "r", "r", "r")
print(xt_result, 
      file = paste0("03_output/tables/", timestamp, "_openings_att.tex"),
      include.rownames = FALSE, include.colnames = FALSE,
      hline.after = c(-1,0, 1, nrow(table_results), nrow(table_results)),
      comment = FALSE,
      booktabs = TRUE, # Enables the use of booktabs style.
      sanitize.text.function = function(x){x})


# Present number of events in a table
n_open <- prices_weekly_reg %>% filter(!is.na(open_treat_group)) %>%
  filter(open_treat_group > 0) %>%
  select(stid) %>% unique() %>% nrow()
n_open_c <- prices_weekly_reg %>% filter(!is.na(open_treat_group_c)) %>%
  filter(open_treat_group_c > 0) %>%
  select(stid) %>% unique() %>% nrow()
n_open_ncsb <- prices_weekly_reg %>% filter(!is.na(open_treat_group_ncsb)) %>%
  filter(open_treat_group_ncsb > 0) %>%
  select(stid) %>% unique() %>% nrow()
n_open_ncc <- prices_weekly_reg %>% filter(!is.na(open_treat_group_ncc)) %>%
  filter(open_treat_group_ncc > 0) %>%
  select(stid) %>% unique() %>% nrow()
n_close <- prices_weekly_reg %>% filter(!is.na(close_treat_group)) %>% filter(close_treat_group > 0) %>%
  select(stid) %>% unique() %>% nrow()

table_n <- cbind( c("", "Competitive", "Noncompetitive, Same Brand",
                    "Noncompetitive, Collusive", "Total Number of Nearby Openings", 
                    "Total Number of Nearby Closings"),
                  c("Number of Events", n_open_c, n_open_ncsb, n_open_ncc, n_open,
                    n_close))
xt_n <- xtable(table_n, include.caption = FALSE, label = "tab:results",
                    include.rownames = FALSE, include.colnames = FALSE,)
align(xt_n) <- c("l", "l", "r")
print(xt_n, 
      file = paste0("03_output/tables/", timestamp, "_openings_n.tex"),
      include.rownames = FALSE, include.colnames = FALSE,
      hline.after = c(-1,0, 1, 5, nrow(table_n), nrow(table_n)),
      comment = FALSE,
      booktabs = TRUE, # Enables the use of booktabs style.
      sanitize.text.function = function(x){x})


