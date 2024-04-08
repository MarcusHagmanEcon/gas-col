rm(list=ls())

setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), "/Dropbox/gas-col"))

library(tidyverse)
library(did)

prices <- readRDS("01_data/02_processed/cleaned_gas_prices.rds")
stations <- readRDS("01_data/02_processed/cleaned_gas_stations.rds")

stid_list <- prices$stid %>% unique()

day_interval <- 30

#Calculate weekly average prices
prices_weekly <- prices %>% mutate(int_date = as.integer(date)) %>%
  filter(int_date %% day_interval == 0)

# Calculate the open and close date of each station
open_close <- prices_weekly %>%
  group_by(stid) %>%
  summarize(
    first_price_date = min(date[!is.na(log_e5)], na.rm = TRUE),
    last_price_date = max(date[!is.na(log_e5)], na.rm = TRUE)
  )

prices_weekly_reg <- prices_weekly %>% left_join(stations %>% 
  left_join(open_close, by = 
            c("stid_of_nearest_station_phdis" = "stid")),
            by = c("stid" = "id")) %>%
  rename(closest_first_price_date = first_price_date,
         closest_last_price_date = last_price_date) %>%
  mutate(first_price_date = min(date),
         last_price_date = max(date)) %>% 
  mutate(treated_open = (closest_first_price_date - first_price_date) > 20,
         treated_close = (last_price_date - closest_last_price_date) > 20) %>%
  mutate(nearest_station_open = ifelse(treated_open, date > closest_first_price_date, 0) ) %>%
  mutate(brand_neighbor_concat = paste(brand, brand_of_nearest_station_phdis, sep = "_")) %>%
  mutate(date = as.integer(date),
         open_treat_group = ifelse(treated_open, as.integer(closest_first_price_date), 0),
         stid_num = which(stid_list == stid[1]),
         open_treat_group_mp = 
           ifelse(treated_open & 
                    brand_neighbor_concat %in% 
                    c("aral_aral", "aral_shell", "shell_aral", "shell_shell", "shell_total",
                      "total_aral", "total_shell", "total_total"), as.integer(closest_first_price_date), 0),
         open_treat_group_ump = 
           ifelse(treated_open & 
                    brand_neighbor_concat %in% 
                    c("aral_aral", "shell_shell", "total_total"), as.integer(closest_first_price_date), 0),
         open_treat_group_cmp = 
           ifelse(treated_open & 
                    brand_neighbor_concat %in% 
                    c("aral_shell", "shell_aral", "shell_total",
                      "total_aral", "total_shell"), as.integer(closest_first_price_date), 0),
         open_treat_group_nmp = 
           ifelse(treated_open & 
                    !(brand_neighbor_concat %in% 
                    c("aral_aral", "aral_shell", "shell_aral", "shell_shell", "shell_total",
                      "total_aral", "total_shell", "total_total") ), as.integer(closest_first_price_date), 0))

source("02_R/02_functions/event_study_graph.R")
# Analysis
all_open_model <- att_gt(yname = "log_e5",
                  tname = "date",
                  idname = "stid_num",
                  gname = "open_treat_group",
                  xformla = ~ 1,
                  data = prices_weekly_reg,
                  allow_unbalanced_panel = TRUE)
all_open_agges <- aggte(all_open_model, type = "dynamic")
event_study_graph(all_open_agges, -15, 15)


nmp_open_model <- att_gt(yname = "log_e5",
                         tname = "date",
                         idname = "stid_num",
                         gname = "open_treat_group_nmp",
                         xformla = ~ 1,
                         data = prices_weekly_reg,
                         allow_unbalanced_panel = TRUE)
nmp_open_agges <- aggte(nmp_open_model, type = "dynamic")
ggdid(all_open_agges, ylim = c(-.04, .04))

mp_open_model <- att_gt(yname = "log_e5",
                         tname = "date",
                         idname = "stid_num",
                         gname = "open_treat_group_mp",
                         xformla = ~ 1,
                         data = prices_weekly_reg,
                         allow_unbalanced_panel = TRUE)
mp_open_agges <- aggte(mp_open_model, type = "dynamic", na.rm = TRUE)
ggdid(mp_open_agges, ylim = c(-.04, .04))

ump_open_model <- att_gt(yname = "log_e5",
                        tname = "date",
                        idname = "stid_num",
                        gname = "open_treat_group_ump",
                        xformla = ~ 1,
                        data = prices_weekly_reg,
                        allow_unbalanced_panel = TRUE)
ump_open_agges <- aggte(ump_open_model, type = "dynamic", na.rm = TRUE)
ggdid(ump_open_agges, ylim = c(-.04, .04))

cmp_open_model <- att_gt(yname = "log_e5",
                        tname = "date",
                        idname = "stid_num",
                        gname = "open_treat_group_cmp",
                        xformla = ~ 1,
                        data = prices_weekly_reg,
                        allow_unbalanced_panel = TRUE)
cmp_open_agges <- aggte(cmp_open_model, type = "dynamic", na.rm = TRUE)
ggdid(cmp_open_agges, ylim = c(-.04, .04))





df <- cbind(event_time  = ump_open_agges$egt / 28,
            est = ump_open_agges$att.egt,
            se  = ump_open_agges$se.egt) %>% 
  as.data.frame() %>% 
  filter(event_time >= -10 & event_time <=10)

ggplot(df, aes(x = event_time, y = est)) + 
  geom_point(stat = "identity", position = position_dodge(), width = 1) +
  geom_errorbar(aes(ymin = est - 1.96*se, ymax = est + 1.96*se), width = 0.4) +
  theme_minimal() +
  labs(x = "Time Relative to Nearby Station Opening", y = "Estimate (95% CI)", title = "Event Study of Nearby Gas Station Openings (Without Market Power)")


ggplot(df, aes(x = event_time, y = est)) + 
  geom_point(stat = "identity", position = position_dodge(), width = 1, 
             aes(color = ifelse(event_time >= 0, "darkblue", "darkred"))) + # Color points conditionally
  geom_errorbar(aes(ymin = est - 1.96*se, ymax = est + 1.96*se, 
                    color = ifelse(event_time >= 0, "darkblue", "darkred")), width = 0.4) + # Color error bars conditionally
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + # Add horizontal line at y = 0
  scale_color_identity() + # Use actual color names provided
  theme_minimal() +
  labs(x = "Time Relative to Nearby Station Opening", y = "Estimate (95% CI)", 
       title = "Event Study of Nearby Gas Station Openings (Without Market Power)")
