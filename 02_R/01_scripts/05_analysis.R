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

# Load data
analysis_data <- readRDS("01_data/02_processed/analysis_data.rds")
gas_stations <- readRDS("01_data/02_processed/cleaned_gas_stations.rds")

# Unique dates
unique_dates <- sort(unique(analysis_data$date))

# Test hypothesis of strategic pricing
model_phdis <- felm(log_e5 ~ same_brand_as_nearest_station_phdis +
                              stations_within_5km + stations_within_10km + stations_within_15km +
                              population_within_10km + stations_per_million_pop_10km|  brand + date  | 0 | brand + date,
                              data = analysis_data,
                              na.action = na.omit)
model_drdis <- felm(log_e5 ~ same_brand_as_nearest_station_drdis +
                      stations_within_5km + stations_within_10km + stations_within_15km +
                      population_within_10km + stations_per_million_pop_10km|  brand + date  | 0 | brand + date,
                    data = analysis_data,
                    na.action = na.omit)
model_drdur <- felm(log_e5 ~ same_brand_as_nearest_station_drdur +
                      stations_within_5km + stations_within_10km + stations_within_15km +
                      population_within_10km + stations_per_million_pop_10km|  brand + date  | 0 | brand + date,
                    data = analysis_data,
                    na.action = na.omit)
model_full <- felm(log_e5 ~ same_brand_as_nearest_station_phdis +
                      same_brand_as_nearest_station_drdis +
                      same_brand_as_nearest_station_drdur +
                      stations_within_5km + stations_within_10km + stations_within_15km +
                      population_within_10km + stations_per_million_pop_10km|  brand + date  | 0 | brand + date,
                    data = analysis_data,
                    na.action = na.omit)
model_phdis_coef <- summary(model_phdis)$coef
model_drdis_coef <- summary(model_drdis)$coef
model_drdur_coef <- summary(model_drdur)$coef
model_full_coef <- summary(model_full)$coef
# Use stargazer with type set to "latex"
sink("03_outputs/tables/20231030_distancecomp.tex")
stargazer(model_phdis, model_drdis, model_drdur, model_full, type = "latex",
          covariate.labels = c("Same Brand as Nearest, Straight Distance", "Same Brand as Nearest, Driving Distance", "Same Brand as Nearest, Driving Duration"),
          omit = c("stations_within_5km", "stations_within_10km", "stations_within_15km", "population_within_10km", "stations_per_million_pop_10km"),
          se = list(model_phdis_coef[,2],model_drdis_coef[,2],model_drdur_coef[,2], model_full_coef[,2]), # assuming second column contains SEs
          omit.stat = "all", # to omit additional statistics like R-squared, F-statistic, etc.
          single.row = FALSE,
          title = "", 
          label = "") 
sink()

# Generate first descriptive graph, showing that gas stations with nearest station
#belonging to the same brand charge higher prices
coefs_same_brand = data.frame()
# Loop over all unique dates in analysis_data
for (d in unique_dates) {
  print(d)
  model_e5 <- felm(log_e5 ~ same_brand_as_nearest_station_phdis +
                     stations_within_5km + stations_within_10km + stations_within_15km +
                     population_within_10km + stations_per_million_pop_10km| brand | 0 | brand,
                   data = analysis_data %>% filter(date == d),
                   na.action = na.omit)
  
  results = c(summary(model_e5)$coef[1,1], summary(model_e5)$coef[1,2])
  
  # Bind the coefficients from the model to the coefs dataframe
  coefs_same_brand <- coefs_same_brand %>% rbind(results)
}
names(coefs_same_brand) <- c("e5", "e5_se")
coefs_same_brand$date <- unique_dates

# Convert data to a longer format for values and SEs separately
value_data <- coefs_same_brand %>% select(-ends_with("_se")) %>%
  gather(key = 'variable', value = 'value', -date)

se_data <- coefs_same_brand %>% select(ends_with("_se"), date) %>%
  gather(key = 'variable', value = 'se', -date) %>%
  mutate(variable = sub("_se", "", variable))

# Join the two long data frames
df_long <- left_join(value_data, se_data, by = c("date", "variable"))

# Plot
p <- ggplot(df_long, aes(x = date, y = value)) +
  geom_hline(yintercept = 0, linetype = "solid", linewidth = 0.75, color = "black") +
  geom_errorbar(
    aes(ymin = value - 1.96 * se, ymax = value + 1.96 * se),
    width = 0.2,
    color = "lightgray",
    alpha = 0.7
  ) +
  geom_line() +
  facet_wrap(~variable, ncol = 1, scales = "free_y") +
  ylim(-0.0025, 0.015) +
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = "darkgray"),  # Change color of major gridlines
    panel.grid.minor = element_line(color = "lightgray")    # Change color of minor gridlines
  )

# Save the plot
ggsave("03_outputs/figures/20231030_fig_samebrand_overtime.png", p, width = 9, height = 4)


brand_list <- gas_stations %>% filter(!is.na(brand)) %>% group_by(brand) %>% 
  summarise(n = n()) %>% arrange(-n) %>% filter(n > 25) %>% mutate(t_stat = NA,
                                                                   p_val = NA)
for (b in brand_list$brand){
  print(which(brand_list$brand == b))
  print(b)
  reg_data <- analysis_data %>% filter(brand == b)
  
  model_brand <- felm(log_e5 ~ same_brand_as_nearest_station_phdis +
                 stations_within_5km + stations_within_10km + stations_within_15km +
                 population_within_10km + stations_per_million_pop_10km| date | 0 | date + stid ,
                 data = reg_data,
                 na.action = na.omit)
  
  model_brand_coef <- summary(model_brand)$coef
  
  brand_list$t_stat[which(brand_list$brand == b)] <- model_brand_coef[1,3]
  brand_list$p_val[which(brand_list$brand == b)] <- model_brand_coef[1,4]
  
  print(model_brand_coef)
}

strategic_ps_list <- brand_list %>% filter( t_stat > 1.96) %>% select(brand) %>% pull() %>% sort()
frac_ps = sum(brand_list %>% filter(brand %in% strategic_ps_list) %>% select(n) %>% pull)/nrow(gas_stations)
analysis_data$strategic_ps <- analysis_data$brand %in% strategic_ps_list
analysis_data$neighbor_strategic_ps <- analysis_data$brand_of_nearest_station_phdis %in% strategic_ps_list

collusion_matrix_firms <- brand_list %>% filter(brand %in% strategic_ps_list & n > 500) %>% select(brand) %>% pull() 
collusion_t_matrix <- data.frame()
collusion_alpha_matrix <- data.frame()
for(b in collusion_matrix_firms){
  reg_data <- analysis_data %>% filter(brand == b)
  reg_data$regressors <- ifelse(reg_data$neighbor_strategic_ps, 
                                paste(reg_data$brand, reg_data$brand_of_nearest_station_phdis, sep = "_"),
                                "aaa_non_strategic_neighbor")
  model <- felm(log_e5 ~ as.factor(regressors) +
                  stations_within_5km + stations_within_10km + stations_within_15km +
                  population_within_10km + stations_per_million_pop_10km|  date  | 0 | date + stid ,
                data = reg_data,
                na.action = na.omit)
  model_coef <- summary(model)$coef
  print(model_coef)
  
  rownames(model_coef) <- sub(".*_", "", rownames(model_coef))
  t_model_coef <- t(model_coef)
  
  for(b2 in collusion_matrix_firms){
    collusion_t_matrix[b,b2] <- tryCatch({t_model_coef[3, b2]}, error = function(e) {return(NA)})
    collusion_alpha_matrix[b,b2] <- tryCatch({t_model_coef[1, b2]}, error = function(e) {return(NA)}) /t_model_coef[1, b]
  }
}
rownames(collusion_t_matrix) <- collusion_matrix_firms
rownames(collusion_alpha_matrix) <- collusion_matrix_firms
print(xtable(collusion_t_matrix), type = "latex", file = "03_outputs/tables/20231030_col_t_mat.tex", 
      include.rownames=TRUE)
print(xtable(collusion_alpha_matrix), type = "latex", file = "03_outputs/tables/20231030_col_alpha_mat.tex", 
      include.rownames=TRUE)

# Combine the matrices into a new character matrix
combined_matrix <- matrix("", nrow = nrow(collusion_alpha_matrix), ncol = ncol(collusion_alpha_matrix),
                          dimnames = dimnames(collusion_alpha_matrix))
for (i in 1:nrow(collusion_alpha_matrix)) {
  for (j in 1:ncol(collusion_alpha_matrix)) {
    combined_matrix[i, j] <- paste(round(collusion_alpha_matrix[i, j],digits = 3), "\\par (", round(collusion_t_matrix[i, j],digits = 3), ")", sep = "")
  }
}
print(xtable(combined_matrix), type = "latex", file = "03_outputs/tables/20231030_comb.tex", 
      include.rownames=TRUE, 
      sanitize.text.function = function(x) {x}, hline.after = NULL, comment = FALSE)


# Focus on strategic ps

model_1 <- felm(log_e5 ~ same_brand_as_nearest_station_phdis +
          stations_within_5km + stations_within_10km + stations_within_15km +
          population_within_10km + stations_per_million_pop_10km|  brand + date  | 0 | brand + date,
          data = analysis_data %>% filter(strategic_ps),
          na.action = na.omit)
model_2 <- felm(log_e5 ~ same_brand_as_nearest_station_phdis +
                  same_brand_as_2nd_nearest_station_phdis + 
                  same_brand_as_3rd_nearest_station_phdis +
                  stations_within_5km + stations_within_10km + stations_within_15km +
                  population_within_10km + stations_per_million_pop_10km|  brand + date  | 0 | brand + date,
                  data = analysis_data %>% filter(strategic_ps),
                  na.action = na.omit)
model_3 <- felm(log_e5 ~ same_brand_as_nearest_station_phdis +
                  I(same_brand_as_nearest_station_phdis*diff_phdis_2nd_1st) +
                  stations_within_5km + stations_within_10km + stations_within_15km +
                  population_within_10km + stations_per_million_pop_10km|  brand + date  | 0 | brand + date,
                  data = analysis_data %>% filter(strategic_ps),
                  na.action = na.omit)
model_4 <- felm(log_e5 ~ same_brand_as_nearest_station_phdis +
                  I(same_brand_as_nearest_station_phdis*diff_phdis_2nd_1st) +
                  same_brand_as_2nd_nearest_station_phdis + 
                  same_brand_as_3rd_nearest_station_phdis +
                  stations_within_5km + stations_within_10km + stations_within_15km +
                  population_within_10km + stations_per_million_pop_10km|  brand + date  | 0 | brand + date,
                  data = analysis_data %>% filter(strategic_ps),
                  na.action = na.omit)
model_1_coef <- summary(model_1)$coef
model_2_coef <- summary(model_2)$coef
model_3_coef <- summary(model_3)$coef
model_4_coef <- summary(model_4)$coef
sink("03_outputs/tables/20231030_reg_strat_ps.tex")
stargazer(model_1, model_2, model_3, model_4, type = "latex",
          covariate.labels = c("Same Brand as Nearest", "Same Brand as Second Nearest", "Same Brand as Third Nearest", "Same Brand as Nearest X Diff 2nd 1st"),
          omit = c("stations_within_5km", "stations_within_10km", "stations_within_15km", "population_within_10km", "stations_per_million_pop_10km"),
          se = list(model_1_coef[,2],model_2_coef[,2],model_3_coef[,2], model_4_coef[,2]), # assuming second column contains SEs
          omit.stat = "all", # to omit additional statistics like R-squared, F-statistic, etc.
          single.row = FALSE,
          title = "", 
          label = "") 
sink()
############################################
############################################
############################################



# Generate first descriptive graph, showing that gas stations with nearest station
#belonging to the same brand charge higher prices
coefs_same_brand = data.frame()
# Loop over all unique dates in analysis_data
for (d in unique_dates) {
  print(d)
  model_e5 <- felm(log_e5 ~ same_brand_as_nearest_station + stations_within_5km + stations_within_10km + stations_within_15km| brand | 0 | brand,
                   data = analysis_data %>% filter(date == d),
                   na.action = na.omit)
  
  model_e10 <- felm(log_e10 ~ same_brand_as_nearest_station + stations_within_5km + stations_within_10km + stations_within_15km| brand | 0 | brand,
                   data = analysis_data %>% filter(date == d),
                   na.action = na.omit)
  
  model_diesel <- felm(log_diesel ~ same_brand_as_nearest_station + stations_within_5km + stations_within_10km + stations_within_15km| brand | 0 | brand,
                   data = analysis_data %>% filter(date == d),
                   na.action = na.omit)
  
  results = c(summary(model_e5)$coef[1,1], summary(model_e5)$coef[1,2],
              summary(model_e10)$coef[1,1], summary(model_e10)$coef[1,2],
              summary(model_diesel)$coef[1,1], summary(model_diesel)$coef[1,2])
  
  # Bind the coefficients from the model to the coefs dataframe
  coefs_same_brand <- coefs_same_brand %>% rbind(results)
}
names(coefs_same_brand) <- c("e5", "e5_se", "e10", "e10_se", "diesel", "diesel_se")
coefs_same_brand$date <- unique_dates


# Convert data to a longer format for values and SEs separately
value_data <- coefs_same_brand %>% select(-ends_with("_se")) %>%
  gather(key = 'variable', value = 'value', -date)

se_data <- coefs_same_brand %>% select(ends_with("_se"), date) %>%
  gather(key = 'variable', value = 'se', -date) %>%
  mutate(variable = sub("_se", "", variable))

# Join the two long data frames
df_long <- left_join(value_data, se_data, by = c("date", "variable"))

# Plot
p <- ggplot(df_long, aes(x = date, y = value)) +
  geom_hline(yintercept = 0, linetype = "solid", linewidth = 0.75, color = "black") +
  geom_errorbar(
    aes(ymin = value - 1.96 * se, ymax = value + 1.96 * se),
    width = 0.2,
    color = "lightgray",
    alpha = 0.7
  ) +
  geom_line() +
  facet_wrap(~variable, ncol = 1, scales = "free_y") +
  ylim(-0.0025, 0.015) +
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = "darkgray"),  # Change color of major gridlines
    panel.grid.minor = element_line(color = "lightgray")    # Change color of minor gridlines
  )

# Save the plot
ggsave("03_outputs/figures/01_same_brand_price1.png", p, width = 9, height = 10)




# Similar exercise but only e5 and by brand
most_common_brands <- c("ARAL", "AVIA", "ESSO", "Shell", "TOTAL")
coefs_same_brand_by_brand <- data.frame()
# Loop over all unique dates in analysis_data
for (d in unique_dates) {
  print(d)
  results <- c()
  for (b in most_common_brands){
    print(b)
    model <- lm(log_e5 ~ same_brand_as_nearest_station + stations_within_5km + stations_within_10km + stations_within_15km,
                     data = analysis_data %>% filter(date == d & brand == b),
                     na.action = na.omit)
    results <- c(results, summary(model)$coef[2,1:2])
  }
  # Bind the coefficients from the model to the coefs dataframe
  coefs_same_brand_by_brand <- coefs_same_brand_by_brand %>% rbind(results)
}
names(coefs_same_brand_by_brand) <- c(rbind(most_common_brands, paste0(most_common_brands, "_se")))
coefs_same_brand_by_brand$date <- unique_dates


# Convert data to a longer format for values and SEs separately
value_data <- coefs_same_brand_by_brand %>% select(-ends_with("_se")) %>%
  gather(key = 'variable', value = 'value', -date)

se_data <- coefs_same_brand_by_brand %>% select(ends_with("_se"), date) %>%
  gather(key = 'variable', value = 'se', -date) %>%
  mutate(variable = sub("_se", "", variable))

# Join the two long data frames
df_long <- left_join(value_data, se_data, by = c("date", "variable"))

# Plot
p2 <- ggplot(df_long, aes(x = date, y = value)) +
  geom_hline(yintercept = 0, linetype = "solid", linewidth = 0.75, color = "black") +
  geom_errorbar(
    aes(ymin = value - 1.96 * se, ymax = value + 1.96 * se),
    width = 0.2,
    color = "lightgray",
    alpha = 0.7
  ) +
  geom_line()  +
  facet_wrap(~variable, ncol = 1, scales = "free_y")+
  ylim(-0.01, 0.02) +  # Set y-axis limits
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = "darkgray"),  # Change color of major gridlines
    panel.grid.minor = element_line(color = "lightgray")    # Change color of minor gridlines
  )


# Save the plot
ggsave("03_outputs/figures/02_same_brand_price_by_brand1.png", p2, width = 9, height = 12)



# Calculate the degree to which firms internalize each others' profits
brands_potential_col <- c("ARAL", "Shell", "TOTAL")
alphas <- data.frame()
# Loop over all unique dates in analysis_data
for (d in unique_dates) {
  print(d)
  results <- c()
  for (b1 in brands_potential_col){
    reg_data <- analysis_data %>% filter(brand == b1, date == d)
    for (b2 in setdiff(brands_potential_col, b1)){
      same_same <- paste(b1,b1, sep = "_")
      same_other <- paste(b1,b2, sep = "_")
      reg_data$same_other <- reg_data$brand_and_neighbor == same_other
      model <- lm(log_e5 ~ same_brand_as_nearest_station + same_other + stations_within_5km + stations_within_10km + stations_within_15km,
                  data = reg_data ,
                  na.action = na.omit)
      # print(summary(model))
      # print(summary(model)$coef[3,1])
      # print(summary(model)$coef[2,1])
      # print(summary(model)$coef[3,1] / summary(model)$coef[2,1])
      results <- c(results, summary(model)$coef[3,1] / summary(model)$coef[2,1])
    }
  }
  # Bind the coefficients from the model to the coefs dataframe
  alphas <- alphas %>% rbind(results)
}
names(alphas) <- c("ARAL_Shell", "ARAL_TOTAL", "Shell_ARAL", "Shell_TOTAL", "TOTAL_ARAL", "TOTAL_Shell")
alphas$date <- unique_dates

alphas_mavg <- alphas
cols_to_average <- setdiff(names(alphas), "date")
alphas[cols_to_average] <- lapply(alphas[cols_to_average], function(x) rollmean(x, k=30, fill=NA, align="right"))

alphas_plot <- gather(alphas_mavg %>% select("Shell_TOTAL", "TOTAL_Shell", "date"), key="variable", value="value", -date)
#coefs_long <- gather(coefs_e5_mavg, key="variable", value="value", -date)

p3 <- ggplot(data=alphas_plot, aes(x=date, y=value, color=variable)) +
  geom_line() +
  labs(title="", x="Date", y="Alpha") +
  theme_minimal()

ggsave("03_outputs/figures/03_shell_total_alphas1.png", p3, width = 9, height = 4)

alphas <- alphas %>% mutate(change_ARAL_Shell = ARAL_Shell - lag(ARAL_Shell),
                            change_Shell_ARAL = Shell_ARAL - lag(Shell_ARAL),
                            change_ARAL_TOTAL = ARAL_TOTAL - lag(ARAL_TOTAL),
                            change_TOTAL_ARAL = TOTAL_ARAL - lag(TOTAL_ARAL),
                            change_Shell_TOTAL = Shell_TOTAL - lag(Shell_TOTAL),
                            change_TOTAL_Shell = TOTAL_Shell - lag(TOTAL_Shell),
                            lag_diff_ARAL_Shell = lag(ARAL_Shell - Shell_ARAL),
                            lag_diff_Shell_ARAL = lag(Shell_ARAL - ARAL_Shell),
                            lag_diff_ARAL_TOTAL = lag(ARAL_TOTAL - TOTAL_ARAL),
                            lag_diff_TOTAL_ARAL = lag(TOTAL_ARAL - ARAL_TOTAL),
                            lag_diff_Shell_TOTAL = lag(Shell_TOTAL - TOTAL_Shell),
                            lag_diff_TOTAL_Shell = lag(TOTAL_Shell - Shell_TOTAL))

alphas_long <- alphas %>% select(date, change_ARAL_Shell, lag_diff_ARAL_Shell) %>% 
    rename(change = change_ARAL_Shell, lag_diff = lag_diff_ARAL_Shell) %>%
  rbind(alphas %>% select(date, change_Shell_ARAL, lag_diff_Shell_ARAL) %>% 
          rename(change = change_Shell_ARAL, lag_diff = lag_diff_Shell_ARAL))%>%
  rbind(alphas %>% select(date, change_ARAL_TOTAL, lag_diff_ARAL_TOTAL) %>% 
          rename(change = change_ARAL_TOTAL, lag_diff = lag_diff_ARAL_TOTAL))%>%
  rbind(alphas %>% select(date, change_TOTAL_ARAL, lag_diff_TOTAL_ARAL) %>% 
          rename(change = change_TOTAL_ARAL, lag_diff = lag_diff_TOTAL_ARAL))%>%
  rbind(alphas %>% select(date, change_Shell_TOTAL, lag_diff_Shell_TOTAL) %>% 
          rename(change = change_Shell_TOTAL, lag_diff = lag_diff_Shell_TOTAL))%>%
  rbind(alphas %>% select(date, change_TOTAL_Shell, lag_diff_TOTAL_Shell) %>% 
          rename(change = change_TOTAL_Shell, lag_diff = lag_diff_TOTAL_Shell))

alphas_long <- alphas_long %>% filter(!is.na(change))

model_change_on_lag_diff <- lm(change ~ lag_diff, data = alphas_long)
model_change_on_lag_diff %>% summary()
stargazer(model_change_on_lag_diff, type = "latex", out = "03_outputs/tables/model_change_on_lag_diff.tex")

source("02_R/02_functions/corr_matrix.R")
corr_matrix <- corr_matrix(692)

fit <- gls(change ~ lag_diff, data = alphas_long, correlation = corr_matrix)






alphas_long$resids <- NA
alphas_long$resids[!is.na(alphas_long$change)] <- residuals(model)

lm(I(ARAL_Shell - lag(ARAL_Shell)) ~ I(lag(ARAL_Shell - Shell_ARAL)), data = alphas) %>% summary()
lm(I(Shell_ARAL - lag(Shell_ARAL)) ~ I(lag(Shell_ARAL - ARAL_Shell)), data = alphas) %>% summary()
lm(I(ARAL_TOTAL - lag(ARAL_TOTAL)) ~ I(lag(ARAL_TOTAL - TOTAL_ARAL)), data = alphas) %>% summary()
lm(I(TOTAL_ARAL - lag(TOTAL_ARAL)) ~ I(lag(TOTAL_ARAL - ARAL_TOTAL)), data = alphas) %>% summary()
lm(I(Shell_TOTAL - lag(Shell_TOTAL)) ~ I(lag(Shell_TOTAL - TOTAL_Shell)), data = alphas) %>% summary()
lm(I(TOTAL_Shell - lag(TOTAL_Shell)) ~ I(lag(TOTAL_Shell - Shell_TOTAL)), data = alphas) %>% summary()

lm(I(ARAL_Shell - lag(ARAL_Shell)) ~ I(lag(ARAL_Shell)) + lag((Shell_ARAL)) + I(lag(ARAL_Shell * Shell_ARAL)), data = alphas) %>% summary()
lm(I(Shell_ARAL - lag(Shell_ARAL)) ~ I(lag(Shell_ARAL)) + lag((ARAL_Shell)) +  I(lag(Shell_ARAL * ARAL_Shell)), data = alphas) %>% summary()
lm(I(ARAL_TOTAL - lag(ARAL_TOTAL)) ~ I(lag(ARAL_TOTAL)) + lag((TOTAL_ARAL)), data = alphas) %>% summary()
lm(I(TOTAL_ARAL - lag(TOTAL_ARAL)) ~ I(lag(TOTAL_ARAL)) + lag((ARAL_TOTAL)), data = alphas) %>% summary()
lm(I(Shell_TOTAL - lag(Shell_TOTAL)) ~ I(lag(Shell_TOTAL)) + lag((TOTAL_Shell)), data = alphas) %>% summary()
lm(I(TOTAL_Shell - lag(TOTAL_Shell)) ~ I(lag(TOTAL_Shell)) + lag((Shell_TOTAL)), data = alphas) %>% summary()

lm(I(ARAL_Shell - lag(ARAL_Shell)) ~ I(lag(ARAL_Shell)) + lag((Shell_ARAL)), data = alphas) %>% summary()
lm(I(Shell_ARAL - lag(Shell_ARAL)) ~ I(lag(Shell_ARAL)) + lag((ARAL_Shell)), data = alphas) %>% summary()
lm(I(ARAL_TOTAL - lag(ARAL_TOTAL)) ~ I(lag(ARAL_TOTAL)) + lag((TOTAL_ARAL)), data = alphas) %>% summary()
lm(I(TOTAL_ARAL - lag(TOTAL_ARAL)) ~ I(lag(TOTAL_ARAL)) + lag((ARAL_TOTAL)), data = alphas) %>% summary()
lm(I(Shell_TOTAL - lag(Shell_TOTAL)) ~ I(lag(Shell_TOTAL)) + lag((TOTAL_Shell)), data = alphas) %>% summary()
lm(I(TOTAL_Shell - lag(TOTAL_Shell)) ~ I(lag(TOTAL_Shell)) + lag((Shell_TOTAL)), data = alphas) %>% summary()

lm(I(ARAL_Shell - lag(ARAL_Shell)) ~ I(lag(ARAL_Shell - Shell_ARAL)) + I(lag(ARAL_Shell - lag(ARAL_Shell))), data = alphas) %>% summary()
lm(I(Shell_ARAL - lag(Shell_ARAL)) ~ I(lag(Shell_ARAL - ARAL_Shell)) + I(lag(Shell_ARAL - lag(Shell_ARAL))), data = alphas) %>% summary()
lm(I(ARAL_TOTAL - lag(ARAL_TOTAL)) ~ I(lag(ARAL_TOTAL - TOTAL_ARAL)) + I(lag(ARAL_TOTAL - lag(ARAL_TOTAL))), data = alphas) %>% summary()
lm(I(TOTAL_ARAL - lag(TOTAL_ARAL)) ~ I(lag(TOTAL_ARAL - ARAL_TOTAL)) + I(lag(TOTAL_ARAL - lag(TOTAL_ARAL))), data = alphas) %>% summary()
lm(I(Shell_TOTAL - lag(Shell_TOTAL)) ~ I(lag(Shell_TOTAL - TOTAL_Shell)) + I(lag(Shell_TOTAL - lag(Shell_TOTAL))), data = alphas) %>% summary()
lm(I(TOTAL_Shell - lag(TOTAL_Shell)) ~ I(lag(TOTAL_Shell - Shell_TOTAL)) + I(lag(TOTAL_Shell - lag(TOTAL_Shell))), data = alphas) %>% summary()

lm(I(ARAL_Shell - lag(ARAL_Shell)) ~ 0 + I(lag(ARAL_Shell - Shell_ARAL)), data = alphas) %>% summary()
lm(I(Shell_ARAL - lag(Shell_ARAL)) ~ 0 + I(lag(Shell_ARAL - ARAL_Shell)), data = alphas) %>% summary()
lm(I(ARAL_TOTAL - lag(ARAL_TOTAL)) ~ 0 + I(lag(ARAL_TOTAL - TOTAL_ARAL)), data = alphas) %>% summary()
lm(I(TOTAL_ARAL - lag(TOTAL_ARAL)) ~ 0 + I(lag(TOTAL_ARAL - ARAL_TOTAL)), data = alphas) %>% summary()
lm(I(Shell_TOTAL - lag(Shell_TOTAL)) ~ 0 + I(lag(Shell_TOTAL - TOTAL_Shell)), data = alphas) %>% summary()
lm(I(TOTAL_Shell - lag(TOTAL_Shell)) ~ 0 + I(lag(TOTAL_Shell - Shell_TOTAL)), data = alphas) %>% summary()

lm(I(ARAL_Shell - lag(ARAL_Shell)) ~ 0 + I(lag(ARAL_Shell - Shell_ARAL)) + I(lag(ARAL_Shell - lag(ARAL_Shell))), data = alphas) %>% summary()
lm(I(Shell_ARAL - lag(Shell_ARAL)) ~ 0 + I(lag(Shell_ARAL - ARAL_Shell)) + I(lag(Shell_ARAL - lag(Shell_ARAL))), data = alphas) %>% summary()
lm(I(ARAL_TOTAL - lag(ARAL_TOTAL)) ~ 0 + I(lag(ARAL_TOTAL - TOTAL_ARAL)) + I(lag(ARAL_TOTAL - lag(ARAL_TOTAL))), data = alphas) %>% summary()
lm(I(TOTAL_ARAL - lag(TOTAL_ARAL)) ~ 0 + I(lag(TOTAL_ARAL - ARAL_TOTAL)) + I(lag(TOTAL_ARAL - lag(TOTAL_ARAL))), data = alphas) %>% summary()
lm(I(Shell_TOTAL - lag(Shell_TOTAL)) ~ 0 + I(lag(Shell_TOTAL - TOTAL_Shell)) + I(lag(Shell_TOTAL - lag(Shell_TOTAL))), data = alphas) %>% summary()
lm(I(TOTAL_Shell - lag(TOTAL_Shell)) ~ 0 + I(lag(TOTAL_Shell - Shell_TOTAL)) + I(lag(TOTAL_Shell - lag(TOTAL_Shell))), data = alphas) %>% summary()

lm(I((ARAL_Shell - lag(ARAL_Shell))/lag(ARAL_Shell)) ~ I(lag(ARAL_Shell - Shell_ARAL)), data = alphas) %>% summary()
lm(I((Shell_ARAL - lag(Shell_ARAL))/lag(Shell_ARAL)) ~ I(lag(Shell_ARAL - ARAL_Shell)), data = alphas) %>% summary()
lm(I((ARAL_TOTAL - lag(ARAL_TOTAL))/lag(ARAL_TOTAL)) ~ I(lag(ARAL_TOTAL - TOTAL_ARAL)), data = alphas) %>% summary()
lm(I((TOTAL_ARAL - lag(TOTAL_ARAL))/lag(TOTAL_ARAL)) ~ I(lag(TOTAL_ARAL - ARAL_TOTAL)), data = alphas) %>% summary()
lm(I((Shell_TOTAL - lag(Shell_TOTAL))/lag(Shell_TOTAL)) ~ I(lag(Shell_TOTAL - TOTAL_Shell)), data = alphas) %>% summary()
lm(I((TOTAL_Shell - lag(TOTAL_Shell))/lag(TOTAL_Shell)) ~ I(lag(TOTAL_Shell - Shell_TOTAL)), data = alphas) %>% summary()


ggplot(alphas, aes(x = lag(ARAL_TOTAL), y = TOTAL_ARAL)) +
  geom_point()+
  geom_smooth(method = "lm" , formula = y ~ x + I(x^2), se = FALSE)

ggplot(alphas, aes(x=date, y=Shell_TOTAL)) +
  geom_line() +
  ggtitle("Line Graph with Date on X-axis and Y Variable on Y-axis") +
  xlab("Date") +
  ylab("Y Variable")


ggplot(alphas, aes(x=date, y=TOTAL_Shell)) +
  geom_line() +
  ggtitle("Line Graph with Date on X-axis and Y Variable on Y-axis") +
  xlab("Date") +
  ylab("Y Variable")















# Create an empty dataframe to store coefficients
coefs_e5 <- data.frame()
coefs_e10 <- data.frame()
coefs_diesel <- data.frame()
unique_dates <- sort(unique(analysis_data$date))

# Loop over all unique dates in analysis_data
for (d in unique_dates) {
  print(d)
  
  model_e5 <- felm(e5 ~as.factor(brand_and_neighbor_most_common) + stations_within_5km + stations_within_10km + stations_within_15km| brand | 0 | brand,
                data = analysis_data %>% filter(date == d),
                na.action = na.omit)
  
  model_e10 <- felm(e10 ~ as.factor(brand_and_neighbor_most_common) + stations_within_5km + stations_within_10km + stations_within_15km| brand | 0 | brand,
                   data = analysis_data %>% filter(date == d),
                   na.action = na.omit)

  model_diesel <- felm(diesel ~ as.factor(brand_and_neighbor_most_common) + stations_within_5km + stations_within_10km + stations_within_15km| brand | 0 | brand,
                   data = analysis_data %>% filter(date == d),
                   na.action = na.omit)
  
  # Bind the coefficients from the model to the coefs dataframe
  coefs_e5 <- rbind(coefs_e5, t(model_e5$coefficients))
  coefs_e10 <- rbind(coefs_e10, t(model_e10$coefficients))
  coefs_diesel <- rbind(coefs_diesel, t(model_diesel$coefficients))
}

# Add a date column
coefs_e5$date <- unique_dates
# Edit the names of the coefficient df
names(coefs_e5) <- sub(".*?\\)", "", names(coefs_e5))
names(coefs_e5) <- gsub(" ", "_", names(coefs_e5))

coefs_e5_mavg <- coefs_e5
cols_to_average <- setdiff(names(coefs_e5), "date")
coefs_e5_mavg[cols_to_average] <- lapply(coefs_e5[cols_to_average], function(x) rollmean(x, k=1, fill=NA, align="right"))

coefs_long <- gather(coefs_e5_mavg %>% select("col_shell_total", "col_total_shell", "brent", "date"), key="variable", value="value", -date)
#coefs_long <- gather(coefs_e5_mavg, key="variable", value="value", -date)

ggplot(data=coefs_long, aes(x=date, y=value, color=variable)) +
  geom_line() +
  labs(title="Line Graph Over Time", x="Date", y="Value") +
  theme_minimal()

price_seller_and_neighbor <- 
  matrix(unlist(colMeans(coefs_e5 %>% select(-c(date)))[1:25]), nrow=5, ncol=5, byrow=TRUE)

coefs_e5 <- coefs_e5 %>% mutate(col_shell_total = ifelse(Shell_TOTAL / Shell_Shell > 1, 1, 
                                                         ifelse(Shell_Shell <= 0, 0, Shell_TOTAL / Shell_Shell )),
                                col_total_shell = ifelse( TOTAL_Shell/TOTAL_TOTAL > 1, 1, 
                                                         ifelse(TOTAL_TOTAL <= 0, 0, TOTAL_Shell/TOTAL_TOTAL )))

coefs_e5 <- coefs_e5 %>% mutate(diff_col_shell_total = col_shell_total - lag(col_shell_total),
                                diff_col_total_shell = col_total_shell - lag(col_total_shell))

coefs_e5 <- coefs_e5 %>% left_join(cleaned_oil_prices, by = c("date" = "date_day"))

coefs_e5 <- coefs_e5 %>% mutate(diff_brent = brent - lag(brent))

lm(diff_col_shell_total ~ lag(diff_brent) + lag(lag(diff_brent)) + lag(lag(lag(diff_brent)))+ lag(lag(lag(lag(diff_brent)))), data = coefs_e5) %>% summary()

daily_avg_prices <- analysis_data %>% group_by(date) %>% summarise(e5 = mean(e5, na.rm = TRUE))

daily_avg_prices <- daily_avg_prices %>% left_join(cleaned_oil_prices, by = c("date" = "date_day"))

daily_avg_prices <- daily_avg_prices %>% mutate(diff_e5 = e5 - lag(e5),
                                                diff_brent = brent - lag(brent))

lm(diff_e5 ~ lag(diff_brent) + lag(lag(diff_brent))+ lag(lag(lag(diff_brent))) + lag(lag(lag(lag(diff_brent)))), data = daily_avg_prices) %>% summary()



lm(diff_col_shell_total ~ diff_brent^2, data = coefs_e5) %>% summary()


analysis_data <- analysis_data %>% mutate(neighbor_same_brand = ifelse(!is.na(brand) & !is.na(brand_of_nearest_station), brand == brand_of_nearest_station, FALSE))

analysis_data <- analysis_data %>% mutate(log_e5 = log(e5))


daily_avg_prices <- analysis_data %>% group_by(date, neighbor_same_brand) %>% summarise(e5 = mean(e5, na.rm = TRUE))

daily_avg_prices <- daily_avg_prices %>% mutate(log_e5 = log(e5))

ggplot(daily_avg_prices, aes(x=date, y=log_e5, group=neighbor_same_brand, color=as.factor(neighbor_same_brand))) +
  geom_line(size=1) +
  labs(color="Neighbor Same Brand", x="Date", y="Value") +
  theme_minimal()

daily_diff <- daily_avg_prices %>% group_by(date) %>% summarize(diff = log_e5[2] - log_e5[1])

ggplot(daily_diff, aes(x=date, y=diff) ) +
  geom_line(size=1) +
  labs( x="Date", y="Value")

daily_diff <- daily_diff %>% left_join(cleaned_oil_prices, by = c("date" = "date_day"))

daily_diff <- daily_diff %>% mutate( diff_diff = (diff - lag(diff)),
                                     diff_brent = brent - lag(brent))

lm(diff_diff ~ 1, data = daily_diff) %>% summary()

# analysis_data <- analysis_data %>%
#   group_by(brand) %>%
#   mutate(brand_n = n()) %>%
#   ungroup() %>%
#   group_by(brand_of_nearest_station) %>%
#   mutate(brand_of_nearest_station_n = n()) %>%
#   ungroup() %>%
#   group_by(brand_and_neighbor) %>%
#   mutate(brand_and_neighbor_n = n()) %>%
#   ungroup() %>% mutate(brand_frac = brand_n / nrow(analysis_data),
#                        brand_of_nearest_station_frac = brand_of_nearest_station_n / nrow(analysis_data),
#                        brand_and_neighbor_frac = brand_and_neighbor_n / nrow(analysis_data),
#                        representation_of_pair = brand_and_neighbor_frac / (brand_frac * brand_of_nearest_station_frac))
# 
# (analysis_data %>% select(brand_and_neighbor_most_common, representation_of_pair) %>%
#   filter(brand_and_neighbor_most_common != "") %>% unique() %>% arrange(brand_and_neighbor_most_common) %>% 
#   select(representation_of_pair) %>% pull())[c(1,7,13,19,25)] %>% sum()

felm(log_e5 ~ same_brand_as_nearest_station_phdis + same_brand_as_nearest_station_drdur + same_brand_as_nearest_station_drdis + population_within_10km + stations_per_million_pop_10km| brand | 0 | brand,
                 data = analysis_data %>% filter(date == unique_dates[1]),
                 na.action = na.omit) %>% summary()

felm(log_e5 ~ same_brand_as_nearest_station_phdis + I(diff_phdis_2nd_1st*same_brand_as_nearest_station_phdis) + 
       same_brand_as_2nd_nearest_station_phdis  + I(diff_phdis_2nd_1st*same_brand_as_2nd_nearest_station_phdis) + 
       same_brand_as_3rd_nearest_station_phdis + population_within_10km + stations_per_million_pop_10km| brand + date | 0 | brand + date,
     data = analysis_data,
     na.action = na.omit) %>% summary()


felm(log_e5 ~ same_brand_as_nearest_station_phdis +
       same_brand_as_2nd_nearest_station_phdis + 
       same_brand_as_3rd_nearest_station_phdis + 
       population_within_10km + stations_per_million_pop_10km| brand + date | 0 | brand + date,
     data = analysis_data,
     na.action = na.omit) %>% summary()

felm(log_e5 ~ same_brand_as_nearest_station_phdis +
       same_brand_as_2nd_nearest_station_phdis + 
       same_brand_as_3rd_nearest_station_phdis + 
       same_brand_as_nearest_station_drdis +
       same_brand_as_2nd_nearest_station_drdis + 
       same_brand_as_3rd_nearest_station_drdis + 
       same_brand_as_nearest_station_drdur +
       same_brand_as_2nd_nearest_station_drdur + 
       same_brand_as_3rd_nearest_station_drdur + 
       population_within_10km + stations_per_million_pop_10km| brand + date | 0 | brand + date,
     data = analysis_data,
     na.action = na.omit) %>% summary()


felm(log_e5 ~ same_brand_as_nearest_station_phdis +
       population_within_10km + stations_per_million_pop_10km| brand  | 0 | brand ,
     data = analysis_data %>% filter(date == unique_dates[1]),
     na.action = na.omit) %>% summary()
felm(log_e5 ~ same_brand_as_nearest_station_phdis +
       population_within_10km + stations_per_million_pop_10km| brand + date  | 0 | stid + date,
     data = analysis_data,
     na.action = na.omit) %>% summary()
felm(log_e5 ~ same_brand_as_nearest_station_phdis +
       same_brand_as_nearest_station_drdis +
       same_brand_as_nearest_station_drdur +
       population_within_10km + stations_per_million_pop_10km|  brand + date  | 0 | brand + date,
     data = analysis_data,
     na.action = na.omit) %>% summary()
felm(log_e5 ~ same_brand_as_nearest_station_phdis +
       same_brand_as_nearest_station_drdis +
       same_brand_as_nearest_station_drdur +
       population_within_10km + stations_per_million_pop_10km|  brand + date  | 0 | stid + date,
     data = analysis_data,
     na.action = na.omit) %>% summary()
felm(log_e5 ~ same_brand_as_nearest_station_phdis +
       same_brand_as_nearest_station_drdis +
       same_brand_as_nearest_station_drdur +
       stations_within_5km + stations_within_10km + stations_within_15km +
       population_within_10km + stations_per_million_pop_10km|  brand + date  | 0 | brand + date,
     data = analysis_data,
     na.action = na.omit) %>% summary()
felm(log_e5 ~ same_brand_as_nearest_station_phdis +
       stations_within_5km + stations_within_10km + stations_within_15km +
       population_within_10km + stations_per_million_pop_10km|  brand + date  | 0 | brand + date,
     data = analysis_data,
     na.action = na.omit) %>% summary()
felm(log_e5 ~ same_brand_as_nearest_station_drdis +
       stations_within_5km + stations_within_10km + stations_within_15km +
       population_within_10km + stations_per_million_pop_10km|  brand + date  | 0 | brand + date,
     data = analysis_data,
     na.action = na.omit) %>% summary()
felm(log_e5 ~ same_brand_as_nearest_station_drdur +
       stations_within_5km + stations_within_10km + stations_within_15km +
       population_within_10km + stations_per_million_pop_10km|  brand + date  | 0 | brand + date,
     data = analysis_data,
     na.action = na.omit) %>% summary()



d = 0.1
(analysis_data %>% filter(phdis_to_nearest_station < d & same_brand_as_nearest_station_phdis) %>% nrow())/
  nrow(analysis_data %>% filter(phdis_to_nearest_station < d ))







analysis_data <- analysis_data %>% mutate(diff_log_e5 = ifelse(stid == lag(stid), log_e5 - lag(log_e5), NA))


# Skip the first 4 rows
oil <- oil[-(1:4), ]

# Separate the column into two using the comma as a separator
# This step assumes that you have a column named 'V1' that contains the data to split.
oil <- separate(oil, col = names(oil), into = c("date", "oil_price"), sep = ",")

# Convert 'date' to an actual Date type if necessary
oil$date <- as.Date(oil$date, format = "%m/%d/%Y")

# Convert 'oil_price' to numeric if necessary
oil$oil_price <- as.numeric(oil$oil_price)

oil$log_oil_price <- log(oil$oil_price)

oil <- oil %>% mutate(diff_log_oil_price = log_oil_price - lag(log_oil_price))

analysis_data <- analysis_data %>% left_join(oil, by = c("date"))

analysis_data <- analysis_data %>% mutate(mkt_pwr = strategic_ps & same_brand_as_nearest_station_phdis)

daily_avg <- analysis_data %>% group_by(date, mkt_pwr) %>% summarise(e5 = mean(e5))

wide_df <- daily_avg %>%
  pivot_wider(
    names_from = mkt_pwr, 
    values_from = e5,
    names_prefix = "e5_",
    values_fill = list(e5 = NA) # This is optional, to fill missing values with NA
  )

wide_df$diff_e5_TRUE = wide_df$e5_TRUE - lag(wide_df$e5_TRUE)
wide_df$diff_e5_FALSE = wide_df$e5_FALSE - lag(wide_df$e5_FALSE)

wide_df <- wide_df %>%
  left_join(oil, by = c("date"))

wide_df <- wide_df[1:691,]

wide_df$oil_price2 <- na.approx(wide_df$oil_price)
wide_df$diff_oil_price2 <- wide_df$oil_price2 - lag(wide_df$oil_price2)

lm(diff_e5_TRUE ~ 0 + lag(diff_oil_price2, n =0) + lag(diff_oil_price2, n =1) + 
     lag(diff_oil_price2, n =2)  + lag(diff_oil_price2, n =3)  +
     lag(diff_oil_price2, n =4) , data = wide_df) %>% summary()
lm(diff_e5_TRUE ~ 0 +  
     lag(diff_oil_price2, n =2)  + 
     I(lag(diff_oil_price2, n =2)*as.numeric(lag(diff_oil_price2, n =2)>0))
     , data = wide_df) %>% summary()
lm(diff_e5_FALSE ~ 0 +  
     lag(diff_oil_price2, n =2)  + 
     I(lag(diff_oil_price2, n =2)*as.numeric(lag(diff_oil_price2, n =2)>0))
   , data = wide_df) %>% summary()
lm(diff_e5_FALSE ~ 0 + lag(diff_oil_price), data = wide_df) %>% summary()

wide_df$oil_price_adj <- wide_df$oil_price * 14
wide_df$oil_price2_adj <- wide_df$oil_price2 * 14
ggplot(wide_df, aes(x = date)) + 
  geom_line(aes(y = e5_TRUE, colour = "e5_TRUE")) + 
  geom_line(aes(y = e5_FALSE, colour = "e5_FALSE")) + 
  geom_line(aes(y = oil_price2_adj, colour = "14x oil_price")) + 
  labs(colour = "Legend") + # Rename the legend title
  scale_colour_manual(values = c("e5_TRUE" = "blue", "e5_FALSE" = "red", "14x oil_price" = "green")) + 
  theme_minimal() +
  labs(x = "Date", y = "Value", title = "Time Series Plot") # Add labels and title
#%>% filter(date < "2014-09-01" & date > "2014-04-01")

lm(diff_log_oil_price ~ 0 + lag(diff_log_oil_price), data = analysis_data) %>% summary()


lm(diff_e5 ~ 0 + lag(diff_oil_price.x), data = analysis_data) %>% summary()

lm(log(diff_e5) ~ 0 + log(diff_oil_price) + I(diff_oil_price * as.numeric(diff_oil_price > 0)), data = analysis_data) %>% summary()


lm(diff_e5 ~  diff_oil_price + I(diff_oil_price * as.numeric(diff_oil_price > 0)), data = analysis_data) %>% summary()

lm(diff_e5 ~  diff_oil_price + I(diff_oil_price * as.numeric(diff_oil_price > 0)), data = analysis_data) %>% summary()


lm(diff_e5 ~ as.numeric((brand == brand_of_nearest_station_phdis)*strategic_ps )+
     lag(lag(diff_oil_price)) + lag(diff_oil_price) + diff_oil_price + 
     I(diff_oil_price * as.numeric(diff_oil_price > 0)) + 
     I(diff_oil_price * as.numeric((brand == brand_of_nearest_station_phdis)*strategic_ps)) +
     I(diff_oil_price * as.numeric(diff_oil_price > 0) * as.numeric((brand == brand_of_nearest_station_phdis)*strategic_ps )),
   data = analysis_data) %>% summary()

lm(diff_e5 ~ 
     lag(lag(diff_oil_price)) + lag(diff_oil_price) + diff_oil_price + 
     I(diff_oil_price * as.numeric(diff_oil_price > 0)),
   data = analysis_data %>% filter(strategic_ps & (brand == brand_of_nearest_station_phdis))) %>% summary()

lm(diff_e5 ~ -1 +
     lag(lag(diff_oil_price)) + lag(diff_oil_price) + diff_oil_price + 
     I(diff_oil_price * as.numeric(diff_oil_price > 0)),
   data = analysis_data %>% filter(strategic_ps )) %>% summary()

lm(diff_e5 ~ -1 +
     lag(lag(diff_oil_price)) + lag(diff_oil_price) + diff_oil_price + 
     I(diff_oil_price * as.numeric(diff_oil_price > 0)),
   data = analysis_data %>% filter(!strategic_ps )) %>% summary()

lm(diff_e5 ~ -1 + diff_oil_price + 
     I(diff_oil_price * as.numeric(diff_oil_price > 0)),
   data = analysis_data %>% filter(strategic_ps )) %>% summary()

lm(diff_e5 ~ -1  + diff_oil_price + 
     I(diff_oil_price * as.numeric(diff_oil_price > 0)),
   data = analysis_data %>% filter(!strategic_ps )) %>% summary()

lm(diff_e5 ~ as.numeric((brand == brand_of_nearest_station_phdis)*strategic_ps )+
     lag(lag(diff_oil_price)) + lag(diff_oil_price) + diff_oil_price + 
     I(diff_oil_price * as.numeric(diff_oil_price > 0)),
   data = analysis_data) %>% summary()


oil <- read_csv("01_data/01_raw/oil.csv")
# Skip the first 4 rows
oil <- oil[-(1:4), ]

# Separate the column into two using the comma as a separator
# This step assumes that you have a column named 'V1' that contains the data to split.
oil <- separate(oil, col = names(oil), into = c("date", "oil_price"), sep = ",")

# Convert 'date' to an actual Date type if necessary
oil$date <- as.Date(oil$date, format = "%m/%d/%Y")

# Convert 'oil_price' to numeric if necessary
oil$oil_price <- as.numeric(oil$oil_price)

oil$log_oil_price <- log(oil$oil_price)

oil <- data.frame("date" = unique_dates)  %>% left_join(oil, by = c("date"))

oil <- oil[1:691,]

oil$oil_price <- na.approx(oil$oil_price)
oil$log_oil_price <- na.approx(oil$log_oil_price)

oil <- oil %>% mutate(diff_oil_price = oil_price - lag(oil_price),
                      diff_log_oil_price = log_oil_price - lag(log_oil_price))


analysis_data <- analysis_data %>% left_join(oil, by = c("date") )

analysis_data$diff_e5 = ifelse(analysis_data$stid == lag(analysis_data$stid), analysis_data$e5 - lag(analysis_data$e5), NA)
analysis_data$diff_log_e5 = ifelse(analysis_data$stid == lag(analysis_data$stid), analysis_data$log_e5 - lag(analysis_data$log_e5), NA)
analysis_data$mkt_pwr <- analysis_data$same_brand_as_nearest_station_phdis * analysis_data$strategic_ps

# Analysis
lm(diff_log_e5 ~ 0+ lag(diff_log_oil_price, n=2) +
    I(lag(diff_log_oil_price, n=2)*(lag(diff_log_oil_price, n=2) > 0)  )+ 
     I(lag(diff_log_oil_price, n=2)*mkt_pwr) +
     I(lag(diff_log_oil_price, n=2)*(lag(diff_log_oil_price, n=2) > 0) *mkt_pwr ) ,
      data = analysis_data) %>% summary()

lm(diff_log_e5 ~ 0+ lag(diff_log_oil_price, n=2) +
     I(lag(diff_log_oil_price, n=2)*(lag(diff_log_oil_price, n=2) > 0)  ),
   data = analysis_data %>% filter(!mkt_pwr)) %>% summary()

lm(diff_log_e5 ~ 0+ lag(diff_log_oil_price, n=2) +
     I(lag(diff_log_oil_price, n=2)*(lag(diff_log_oil_price, n=2) > 0)  ),
   data = analysis_data %>% filter(mkt_pwr==1)) %>% summary()

lm(diff_log_e5 ~ 0+ lag(diff_log_oil_price, n=2) +
     I(lag(diff_log_oil_price, n=2)*(lag(diff_log_oil_price, n=2) > 0)  ),
   data = analysis_data %>% filter(strategic_ps==1)) %>% summary()

lm(diff_log_e5 ~ 0+ lag(diff_log_oil_price, n=2) +
     I(lag(diff_log_oil_price, n=2)*(lag(diff_log_oil_price, n=2) > 0)  ),
   data = analysis_data %>% filter(same_brand_as_nearest_station_phdis==1 & strategic_ps!=1)) %>% summary()

lm(diff_log_e5 ~ 0 + lag(diff_log_oil_price, n=2) +
     I(lag(diff_log_oil_price, n=2)*(lag(diff_log_oil_price, n=2 > 0)) * diff_phdis_2nd_1st  ),
   data = analysis_data %>% filter(mkt_pwr==1) ) %>% summary()

lm(diff_log_e5 ~ 0+ lag(diff_log_oil_price, n=2) +
     I(lag(diff_log_oil_price, n=2)*(lag(diff_log_oil_price, n=2) > 0))+
     I(lag(diff_log_oil_price, n=2)*(lag(diff_log_oil_price, n=2) > 0) * diff_phdis_2nd_1st ),
   data = analysis_data %>% filter(mkt_pwr==1)) %>% summary()

lm(diff_log_e5 ~ 0+ lag(diff_log_oil_price, n=2) +
     I(lag(diff_log_oil_price, n=2)*(lag(diff_log_oil_price, n=2) > 0)),
   data = analysis_data %>% filter(mkt_pwr==1)) %>% summary()

lm(diff_log_e5 ~ 0+ lag(diff_log_oil_price, n=2) +
     I(lag(diff_log_oil_price, n=2)*(lag(diff_log_oil_price, n=2) > 0)),
   data = analysis_data %>% filter((brand_of_3rd_nearest_station_phdis == brand_of_2nd_nearest_station_phdis))) %>% summary()

lm(diff_log_e5 ~ 0+ lag(diff_log_oil_price, n=2) +
     I(lag(diff_log_oil_price, n=2)*(lag(diff_log_oil_price, n=2) > 0)),
   data = analysis_data %>% filter((brand_of_3rd_nearest_station_phdis == brand_of_2nd_nearest_station_phdis) & 
                                     !(brand_of_2nd_nearest_station_phdis %in% strategic_ps_list))) %>% summary()


lm(diff_log_e5 ~ 0+ lag(diff_log_oil_price, n=2),
   data = analysis_data) %>% summary()

lm(diff_log_e5 ~ 0+ lag(diff_log_oil_price, n=2),
   data = analysis_data %>% filter(!mkt_pwr)) %>% summary()

lm(diff_log_e5 ~ 0+ lag(diff_log_oil_price, n=2),
   data = analysis_data %>% filter(mkt_pwr==1)) %>% summary()

lm(diff_log_e5 ~ 0+ lag(diff_log_oil_price, n=2),
   data = analysis_data %>% filter(strategic_ps==1)) %>% summary()



felm(log_e5 ~ same_brand_as_nearest_station_phdis +
       stations_within_5km + stations_within_10km + stations_within_15km +
       population_within_10km + stations_per_million_pop_10km|  brand + date  | 0 | brand + date,
     data = analysis_data %>% filter(strategic_ps),
     na.action = na.omit) %>% summary()

felm(log_e5 ~ same_brand_as_nearest_station_phdis +
       stations_within_5km + stations_within_10km + stations_within_15km +
       population_within_10km + stations_per_million_pop_10km|  brand + date  | 0 | brand + date,
     data = analysis_data %>% filter(!strategic_ps),
     na.action = na.omit) %>% summary()


lm(diff_e5 ~ 0+ lag(diff_oil_price, n=2) +
     I(lag(diff_log_oil_price, n=2)*(lag(diff_log_oil_price, n=2) > 0)),
   data = analysis_data ) %>% summary()

lm(diff_e5 ~ 0+ lag(diff_oil_price, n=2) +
     I(lag(diff_log_oil_price, n=2)*(lag(diff_log_oil_price, n=2) > 0)),
   data = analysis_data %>% filter(same_brand_as_nearest_station_phdis & strategic_ps) ) %>% summary()

lm(diff_e5 ~ 0+ lag(diff_oil_price, n=2) +
     I(lag(diff_log_oil_price, n=2)*(lag(diff_log_oil_price, n=2) > 0)) +
     lag(diff_oil_price, n=2)*as.numeric(same_brand_as_nearest_station_phdis) +
     I(lag(diff_log_oil_price, n=2)*(lag(diff_log_oil_price, n=2) > 0)*same_brand_as_nearest_station_phdis),
   data = analysis_data %>% filter(!strategic_ps) ) %>% summary()

lm(diff_e5 ~ 0+ lag(diff_oil_price, n=2) +
     I(lag(diff_log_oil_price, n=2)*(lag(diff_log_oil_price, n=2) > 0)) +
     lag(diff_oil_price, n=2)*as.numeric(same_brand_as_nearest_station_phdis) +
     I(lag(diff_log_oil_price, n=2)*(lag(diff_log_oil_price, n=2) > 0)*same_brand_as_nearest_station_phdis)+
     I(lag(diff_log_oil_price, n=2)*(lag(diff_log_oil_price, n=2) > 0)*same_brand_as_nearest_station_phdis * diff_phdis_2nd_1st),
   data = analysis_data %>% filter(strategic_ps) ) %>% summary()


lm(diff_e5 ~ 1 + lag(diff_oil_price, n=2) +
     I(lag(diff_log_oil_price, n=2)*(lag(diff_log_oil_price, n=2) > 0)) +
     lag(diff_oil_price, n=2) * as.numeric(strategic_ps & same_brand_as_nearest_station_phdis) +
     I(lag(diff_log_oil_price, n=2)*(lag(diff_log_oil_price, n=2) > 0)) * as.numeric(strategic_ps & same_brand_as_nearest_station_phdis),
   data = analysis_data ) %>% summary()
  
lm(diff_log_e5 ~ 1 + lag(diff_log_oil_price, n=2) +
     I(lag(diff_log_oil_price, n=2)*(lag(diff_log_oil_price, n=2) > 0)) +
     I(lag(diff_log_oil_price, n=2) * as.numeric(strategic_ps & same_brand_as_nearest_station_phdis)) +
     I(lag(diff_log_oil_price, n=2)*(lag(diff_log_oil_price, n=2) > 0) * as.numeric(strategic_ps & same_brand_as_nearest_station_phdis)),
   data = analysis_data ) %>% summary()

lm(diff_log_e5 ~ 0 + lag(diff_log_oil_price, n=2) +
     I(lag(diff_log_oil_price, n=2)*(lag(diff_log_oil_price, n=2) > 0)) +
     I(lag(diff_log_oil_price, n=2) * as.numeric(strategic_ps & same_brand_as_nearest_station_phdis)) +
     I(lag(diff_log_oil_price, n=2)*(lag(diff_log_oil_price, n=2) > 0) * as.numeric(strategic_ps & same_brand_as_nearest_station_phdis)) +
     I(lag(diff_log_oil_price, n=2) * diff_phdis_2nd_1st) +
     I(lag(diff_log_oil_price, n=2)*(lag(diff_log_oil_price, n=2) > 0) * diff_phdis_2nd_1st) +
     I(lag(diff_log_oil_price, n=2) * as.numeric(strategic_ps & same_brand_as_nearest_station_phdis) * diff_phdis_2nd_1st) +
     I(lag(diff_log_oil_price, n=2)*(lag(diff_log_oil_price, n=2) > 0) * as.numeric(strategic_ps & same_brand_as_nearest_station_phdis) * diff_phdis_2nd_1st),
     data = analysis_data ) %>% summary()

lm(diff_log_e5 ~ 0 + lag(diff_log_oil_price, n=3) +
     I(lag(diff_log_oil_price, n=3)*(lag(diff_log_oil_price, n=3) > 0)) +
     I(lag(diff_log_oil_price, n=3) * as.numeric(strategic_ps & same_brand_as_nearest_station_phdis)) +
     I(lag(diff_log_oil_price, n=3)*(lag(diff_log_oil_price, n=3) > 0) * as.numeric(strategic_ps & same_brand_as_nearest_station_phdis)) +
     I(lag(diff_log_oil_price, n=3) * diff_phdis_2nd_1st) +
     I(lag(diff_log_oil_price, n=3)*(lag(diff_log_oil_price, n=3) > 0) * diff_phdis_2nd_1st) +
     I(lag(diff_log_oil_price, n=3) * as.numeric(strategic_ps & same_brand_as_nearest_station_phdis) * diff_phdis_2nd_1st) +
     I(lag(diff_log_oil_price, n=3)*(lag(diff_log_oil_price, n=3) > 0) * as.numeric(strategic_ps & same_brand_as_nearest_station_phdis) * diff_phdis_2nd_1st),
   data = analysis_data ) %>% summary()

felm(log_e5 ~ diff_phdis_2nd_1st + as.numeric(strategic_ps & same_brand_as_nearest_station_phdis) +
     I(as.numeric(strategic_ps & same_brand_as_nearest_station_phdis)*diff_phdis_2nd_1st) +
     stations_within_5km + stations_within_10km + stations_within_15km +
     population_within_10km + stations_per_million_pop_10km|  brand + date  | 0 | brand + date,
     data = analysis_data,
     na.action = na.omit) %>% summary()



model <- lm(diff_log_e5 ~ 0 + lag(diff_log_oil_price, n=0) +
     lag(diff_log_oil_price, n=1) +
     lag(diff_log_oil_price, n=2) +
     lag(diff_log_oil_price, n=3) +
     lag(diff_log_oil_price, n=4) +
     lag(diff_log_oil_price, n=5) +
       lag(diff_log_oil_price, n=6) +
       lag(diff_log_oil_price, n=7) +
       lag(diff_log_oil_price, n=8) +
       lag(diff_log_oil_price, n=9) +
       lag(diff_log_oil_price, n=10) +
       lag(diff_log_oil_price, n=11) +
     I( lag(diff_log_oil_price, n=0) * as.numeric(diff_log_oil_price > 0)) +
     I( lag(diff_log_oil_price, n=1) * as.numeric(diff_log_oil_price > 0)) +
     I( lag(diff_log_oil_price, n=2) * as.numeric(diff_log_oil_price > 0)) +
     I( lag(diff_log_oil_price, n=3) * as.numeric(diff_log_oil_price > 0)) +
     I( lag(diff_log_oil_price, n=4) * as.numeric(diff_log_oil_price > 0)) +
     I( lag(diff_log_oil_price, n=5) * as.numeric(diff_log_oil_price > 0)) + 
       I( lag(diff_log_oil_price, n=6) * as.numeric(diff_log_oil_price > 0)) +
       I( lag(diff_log_oil_price, n=7) * as.numeric(diff_log_oil_price > 0)) +
       I( lag(diff_log_oil_price, n=8) * as.numeric(diff_log_oil_price > 0)) +
       I( lag(diff_log_oil_price, n=9) * as.numeric(diff_log_oil_price > 0)) +
       I( lag(diff_log_oil_price, n=10) * as.numeric(diff_log_oil_price > 0)) +
       I( lag(diff_log_oil_price, n=11) * as.numeric(diff_log_oil_price > 0)),
   data = analysis_data ) %>% summary()

model$coefficients[2:8,1] %>% sum()
model$coefficients[9:15,1] %>% sum()
model$coefficients[16:22,1] %>% sum()
model$coefficients[23:29,1] %>% sum()
model$coefficients[2:8,1] %>% rbind( model$coefficients[16:22,1]) %>% sum()
model$coefficients[9:15,1] %>% rbind( model$coefficients[23:29,1]) %>% sum()

model <- felm(diff_log_e5 ~ lag(diff_log_oil_price, n=0) +
     lag(diff_log_oil_price, n=1) +
     lag(diff_log_oil_price, n=2) +
     lag(diff_log_oil_price, n=3) +
     lag(diff_log_oil_price, n=4) +
     lag(diff_log_oil_price, n=5) +
     lag(diff_log_oil_price, n=6) +
     lag(diff_log_oil_price, n=7) +
     lag(diff_log_oil_price, n=8) +
     lag(diff_log_oil_price, n=9) +
     lag(diff_log_oil_price, n=10) +
     lag(diff_log_oil_price, n=11) +
     I( lag(diff_log_oil_price, n=0) * as.numeric(lag(diff_log_oil_price, n=0) > 0)) +
     I( lag(diff_log_oil_price, n=1) * as.numeric(lag(diff_log_oil_price, n=1) > 0)) +
     I( lag(diff_log_oil_price, n=2) * as.numeric(lag(diff_log_oil_price, n=2) > 0)) +
     I( lag(diff_log_oil_price, n=3) * as.numeric(lag(diff_log_oil_price, n=3) > 0)) +
     I( lag(diff_log_oil_price, n=4) * as.numeric(lag(diff_log_oil_price, n=4) > 0)) +
     I( lag(diff_log_oil_price, n=5) * as.numeric(lag(diff_log_oil_price, n=5) > 0)) + 
     I( lag(diff_log_oil_price, n=6) * as.numeric(lag(diff_log_oil_price, n=6) > 0)) +
     I( lag(diff_log_oil_price, n=7) * as.numeric(lag(diff_log_oil_price, n=7) > 0)) +
     I( lag(diff_log_oil_price, n=8) * as.numeric(lag(diff_log_oil_price, n=8) > 0)) +
     I( lag(diff_log_oil_price, n=9) * as.numeric(lag(diff_log_oil_price, n=9) > 0)) +
     I( lag(diff_log_oil_price, n=10) * as.numeric(lag(diff_log_oil_price, n=10) > 0)) +
     I( lag(diff_log_oil_price, n=11) * as.numeric(lag(diff_log_oil_price, n=11) > 0))|0|0| stid + date,
   data = analysis_data ) %>% summary()

model1 <- lm(diff_log_e5 ~ lag(diff_log_oil_price, n=0) +
                lag(diff_log_oil_price, n=1) +
                lag(diff_log_oil_price, n=2) +
                lag(diff_log_oil_price, n=3) +
                lag(diff_log_oil_price, n=4) +
                lag(diff_log_oil_price, n=5) +
                lag(diff_log_oil_price, n=6) +
                I( lag(diff_log_oil_price, n=0) * as.numeric(lag(diff_log_oil_price, n=0) > 0)) +
                I( lag(diff_log_oil_price, n=1) * as.numeric(lag(diff_log_oil_price, n=1) > 0)) +
                I( lag(diff_log_oil_price, n=2) * as.numeric(lag(diff_log_oil_price, n=2) > 0)) +
                I( lag(diff_log_oil_price, n=3) * as.numeric(lag(diff_log_oil_price, n=3) > 0)) +
                I( lag(diff_log_oil_price, n=4) * as.numeric(lag(diff_log_oil_price, n=4) > 0)) +
                I( lag(diff_log_oil_price, n=5) * as.numeric(lag(diff_log_oil_price, n=5) > 0)) + 
                I( lag(diff_log_oil_price, n=6) * as.numeric(lag(diff_log_oil_price, n=6) > 0)),
              data = analysis_data )

model2 <- felm(diff_log_e5 ~ lag(diff_log_oil_price, n=0) +
                lag(diff_log_oil_price, n=1) +
                lag(diff_log_oil_price, n=2) +
                lag(diff_log_oil_price, n=3) +
                lag(diff_log_oil_price, n=4) +
                lag(diff_log_oil_price, n=5) +
                lag(diff_log_oil_price, n=6) +
                I( lag(diff_log_oil_price, n=0) * as.numeric(lag(diff_log_oil_price, n=0) > 0)) +
                I( lag(diff_log_oil_price, n=1) * as.numeric(lag(diff_log_oil_price, n=1) > 0)) +
                I( lag(diff_log_oil_price, n=2) * as.numeric(lag(diff_log_oil_price, n=2) > 0)) +
                I( lag(diff_log_oil_price, n=3) * as.numeric(lag(diff_log_oil_price, n=3) > 0)) +
                I( lag(diff_log_oil_price, n=4) * as.numeric(lag(diff_log_oil_price, n=4) > 0)) +
                I( lag(diff_log_oil_price, n=5) * as.numeric(lag(diff_log_oil_price, n=5) > 0)) + 
                I( lag(diff_log_oil_price, n=6) * as.numeric(lag(diff_log_oil_price, n=6) > 0)) +
              I(lag(diff_log_oil_price, n=0)*mkt_pwr) +
              I(lag(diff_log_oil_price, n=1)*mkt_pwr) +
              I(lag(diff_log_oil_price, n=2)*mkt_pwr) +
              I(lag(diff_log_oil_price, n=3)*mkt_pwr) +
              I(lag(diff_log_oil_price, n=4)*mkt_pwr) +
              I(lag(diff_log_oil_price, n=5)*mkt_pwr) +
              I(lag(diff_log_oil_price, n=6)*mkt_pwr) +
              I( lag(diff_log_oil_price, n=0) * as.numeric(lag(diff_log_oil_price, n=0) > 0)*mkt_pwr) +
              I( lag(diff_log_oil_price, n=1) * as.numeric(lag(diff_log_oil_price, n=1) > 0)*mkt_pwr) +
              I( lag(diff_log_oil_price, n=2) * as.numeric(lag(diff_log_oil_price, n=2) > 0)*mkt_pwr) +
              I( lag(diff_log_oil_price, n=3) * as.numeric(lag(diff_log_oil_price, n=3) > 0)*mkt_pwr) +
              I( lag(diff_log_oil_price, n=4) * as.numeric(lag(diff_log_oil_price, n=4) > 0)*mkt_pwr) +
              I( lag(diff_log_oil_price, n=5) * as.numeric(lag(diff_log_oil_price, n=5) > 0)*mkt_pwr) + 
              I( lag(diff_log_oil_price, n=6) * as.numeric(lag(diff_log_oil_price, n=6) > 0)*mkt_pwr)|0|0|date+stid,
              data = analysis_data )

anova_result <- anova(model1, model2)
print(anova_result)

analysis_data$mkt_pwr <- as.numeric(analysis_data$same_brand_as_nearest_station_phdis & analysis_data$strategic_ps)

model_no_mkt_pwr <- felm(diff_log_e5 ~ 0 +
                         I( lag(diff_log_oil_price, n=0) * as.numeric(lag(diff_log_oil_price, n=0) <= 0)) +
                         I( lag(diff_log_oil_price, n=1) * as.numeric(lag(diff_log_oil_price, n=1) <= 0)) +
                         I( lag(diff_log_oil_price, n=2) * as.numeric(lag(diff_log_oil_price, n=2) <= 0)) +
                         I( lag(diff_log_oil_price, n=3) * as.numeric(lag(diff_log_oil_price, n=3) <= 0)) +
                         I( lag(diff_log_oil_price, n=4) * as.numeric(lag(diff_log_oil_price, n=4) <= 0)) +
                         I( lag(diff_log_oil_price, n=5) * as.numeric(lag(diff_log_oil_price, n=5) <= 0)) + 
                         I( lag(diff_log_oil_price, n=6) * as.numeric(lag(diff_log_oil_price, n=6) <= 0)) +
                         I( lag(diff_log_oil_price, n=0) * as.numeric(lag(diff_log_oil_price, n=0) > 0)) +
                         I( lag(diff_log_oil_price, n=1) * as.numeric(lag(diff_log_oil_price, n=1) > 0)) +
                         I( lag(diff_log_oil_price, n=2) * as.numeric(lag(diff_log_oil_price, n=2) > 0)) +
                         I( lag(diff_log_oil_price, n=3) * as.numeric(lag(diff_log_oil_price, n=3) > 0)) +
                         I( lag(diff_log_oil_price, n=4) * as.numeric(lag(diff_log_oil_price, n=4) > 0)) +
                         I( lag(diff_log_oil_price, n=5) * as.numeric(lag(diff_log_oil_price, n=5) > 0)) + 
                         I( lag(diff_log_oil_price, n=6) * as.numeric(lag(diff_log_oil_price, n=6) > 0))|0|0|date+stid,
                          data = analysis_data %>% filter(mkt_pwr==0) )

model_mkt_pwr <- felm(diff_log_e5 ~ 0 +
                           I( lag(diff_log_oil_price, n=0) * as.numeric(lag(diff_log_oil_price, n=0) <= 0)) +
                           I( lag(diff_log_oil_price, n=1) * as.numeric(lag(diff_log_oil_price, n=1) <= 0)) +
                           I( lag(diff_log_oil_price, n=2) * as.numeric(lag(diff_log_oil_price, n=2) <= 0)) +
                           I( lag(diff_log_oil_price, n=3) * as.numeric(lag(diff_log_oil_price, n=3) <= 0)) +
                           I( lag(diff_log_oil_price, n=4) * as.numeric(lag(diff_log_oil_price, n=4) <= 0)) +
                           I( lag(diff_log_oil_price, n=5) * as.numeric(lag(diff_log_oil_price, n=5) <= 0)) + 
                           I( lag(diff_log_oil_price, n=6) * as.numeric(lag(diff_log_oil_price, n=6) <= 0)) +
                           I( lag(diff_log_oil_price, n=0) * as.numeric(lag(diff_log_oil_price, n=0) > 0)) +
                           I( lag(diff_log_oil_price, n=1) * as.numeric(lag(diff_log_oil_price, n=1) > 0)) +
                           I( lag(diff_log_oil_price, n=2) * as.numeric(lag(diff_log_oil_price, n=2) > 0)) +
                           I( lag(diff_log_oil_price, n=3) * as.numeric(lag(diff_log_oil_price, n=3) > 0)) +
                           I( lag(diff_log_oil_price, n=4) * as.numeric(lag(diff_log_oil_price, n=4) > 0)) +
                           I( lag(diff_log_oil_price, n=5) * as.numeric(lag(diff_log_oil_price, n=5) > 0)) + 
                           I( lag(diff_log_oil_price, n=6) * as.numeric(lag(diff_log_oil_price, n=6) > 0))|0|0|date+stid,
                         data = analysis_data %>% filter(mkt_pwr==1) )

plot_data <- data.frame(x = 0:6, coef = model_mkt_pwr$coefficients[1:7],
                        se = model_mkt_pwr$cse[1:7], group = 1)
plot_data <- plot_data %>% rbind(
  data.frame(x = 0:6, coef = model_mkt_pwr$coefficients[8:14],
             se = model_mkt_pwr$cse[8:14], group = 2)
)
plot_data <- plot_data %>% rbind(
  data.frame(x = 0:6, coef = model_no_mkt_pwr$coefficients[1:7],
             se = model_no_mkt_pwr$cse[1:7], group = 3)
)
plot_data <- plot_data %>% rbind(
  data.frame(x = 0:6, coef = model_no_mkt_pwr$coefficients[8:14],
             se = model_no_mkt_pwr$cse[8:14], group = 4)
)
plot_data$group <- factor(plot_data$group)

# Generate the overlaid plot
p <- ggplot(plot_data, aes(x = x, y = coef, group = group, color = group)) +
  geom_line(aes(linetype = group), position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = coef - 2*se, ymax = coef + 2*se), 
                width = .2,                    # Adjust the width of the error bars
                position = position_dodge(0.2)) +
  labs(x = 'Coefficient', y = 'Value') +
  theme_minimal() +
  scale_color_manual(values = c("red", "darkred", "lightblue", "blue")) # Customize colors if needed

# Display the plot
print(p)


analysis_data$weekday <- weekdays(analysis_data$date)

model <- felm(diff_log_e5 ~ 0 +
                           I( lag(diff_log_oil_price, n=0) * as.numeric(lag(diff_log_oil_price, n=0) <= 0)) +
                           I( lag(diff_log_oil_price, n=1) * as.numeric(lag(diff_log_oil_price, n=1) <= 0)) +
                           I( lag(diff_log_oil_price, n=2) * as.numeric(lag(diff_log_oil_price, n=2) <= 0)) +
                           I( lag(diff_log_oil_price, n=3) * as.numeric(lag(diff_log_oil_price, n=3) <= 0)) +
                           I( lag(diff_log_oil_price, n=4) * as.numeric(lag(diff_log_oil_price, n=4) <= 0)) +
                           I( lag(diff_log_oil_price, n=5) * as.numeric(lag(diff_log_oil_price, n=5) <= 0)) + 
                           I( lag(diff_log_oil_price, n=6) * as.numeric(lag(diff_log_oil_price, n=6) <= 0)) +
                           I( lag(diff_log_oil_price, n=0) * as.numeric(lag(diff_log_oil_price, n=0) > 0)) +
                           I( lag(diff_log_oil_price, n=1) * as.numeric(lag(diff_log_oil_price, n=1) > 0)) +
                           I( lag(diff_log_oil_price, n=2) * as.numeric(lag(diff_log_oil_price, n=2) > 0)) +
                           I( lag(diff_log_oil_price, n=3) * as.numeric(lag(diff_log_oil_price, n=3) > 0)) +
                           I( lag(diff_log_oil_price, n=4) * as.numeric(lag(diff_log_oil_price, n=4) > 0)) +
                           I( lag(diff_log_oil_price, n=5) * as.numeric(lag(diff_log_oil_price, n=5) > 0)) + 
                           I( lag(diff_log_oil_price, n=6) * as.numeric(lag(diff_log_oil_price, n=6) > 0)) +
                            as.factor(weekday)|0|0|date+stid,
                         data = analysis_data )

plot_data <- data.frame(x = 0:6, coef = model$coefficients[1:7],
                        se = model$cse[1:7], group = 1)
plot_data <- plot_data %>% rbind(
  data.frame(x = 0:6, coef = model$coefficients[8:14],
             se = model$cse[8:14], group = 2)
)
plot_data$group <- factor(plot_data$group)

# Generate the overlaid plot
p <- ggplot(plot_data, aes(x = x, y = coef, group = group, color = group)) +
  geom_line(aes(linetype = group), position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = coef - 2*se, ymax = coef + 2*se), 
                width = .2,                    # Adjust the width of the error bars
                position = position_dodge(0.2)) +
  labs(x = 'Coefficient', y = 'Value') +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue")) # Customize colors if needed

# Display the plot
print(p)

df <- data.frame(coef_neg <- model$coefficients[1:7],
                 coef_pos <- model$coefficients[8:14])

lm(coef_neg ~ coef_pos, data = df) %>% summary()

ggplot(df, aes(x = coef_neg, y = coef_pos)) +
  geom_point() +
  labs(x = "Coef Neg", y = "Coef Pos", title = "Scatterplot of Coef Neg vs Coef Pos") +
  theme_minimal()





analysis_data$weekday <- weekdays(analysis_data$date)
unique_stid <- gas_stations$id
gas_stations$apt_t <- NA
gas_stations$apt_coef <- NA
gas_stations$apt_corr <- NA

for (i in 1:length(unique_stid)){
  stid_var <- unique_stid[i]
  print(i)
  reg_data <- analysis_data %>% filter(stid == stid_var)
  
  try({
    model <- lm(diff_log_e5 ~ 0 +
                  I( lag(diff_log_oil_price, n=0) * as.numeric(lag(diff_log_oil_price, n=0) <= 0)) +
                  I( lag(diff_log_oil_price, n=1) * as.numeric(lag(diff_log_oil_price, n=1) <= 0)) +
                  I( lag(diff_log_oil_price, n=2) * as.numeric(lag(diff_log_oil_price, n=2) <= 0)) +
                  I( lag(diff_log_oil_price, n=3) * as.numeric(lag(diff_log_oil_price, n=3) <= 0)) +
                  I( lag(diff_log_oil_price, n=4) * as.numeric(lag(diff_log_oil_price, n=4) <= 0)) +
                  I( lag(diff_log_oil_price, n=5) * as.numeric(lag(diff_log_oil_price, n=5) <= 0)) + 
                  I( lag(diff_log_oil_price, n=6) * as.numeric(lag(diff_log_oil_price, n=6) <= 0)) +
                  I( lag(diff_log_oil_price, n=0) * as.numeric(lag(diff_log_oil_price, n=0) > 0)) +
                  I( lag(diff_log_oil_price, n=1) * as.numeric(lag(diff_log_oil_price, n=1) > 0)) +
                  I( lag(diff_log_oil_price, n=2) * as.numeric(lag(diff_log_oil_price, n=2) > 0)) +
                  I( lag(diff_log_oil_price, n=3) * as.numeric(lag(diff_log_oil_price, n=3) > 0)) +
                  I( lag(diff_log_oil_price, n=4) * as.numeric(lag(diff_log_oil_price, n=4) > 0)) +
                  I( lag(diff_log_oil_price, n=5) * as.numeric(lag(diff_log_oil_price, n=5) > 0)) + 
                  I( lag(diff_log_oil_price, n=6) * as.numeric(lag(diff_log_oil_price, n=6) > 0)) +
                  as.factor(weekday),
                data = reg_data )
    df <- data.frame(coef_neg = model$coefficients[1:7],
                     coef_pos = model$coefficients[8:14])
    
    output <- lm(coef_neg ~ coef_pos, data = df) %>% summary() %>% coef()
    
    gas_stations$apt_t[i] <- output["coef_pos","t value"]
    gas_stations$apt_coef[i] <- output["coef_pos","Estimate"]
    gas_stations$apt_corr[i] <- cor(df$coef_pos, df$coef_neg)
  })
}

saveRDS(gas_stations, file = "01_data/02_processed/cleaned_gas_stations_temp.rds")

for_merge <- gas_stations %>% select(id, apt_t, apt_coef, apt_corr)

analysis_data_2 <- analysis_data %>% left_join(for_merge, by = c("stid" = "id"))

model <- felm(log_e5 ~ I(apt_corr<0) +  same_brand_as_nearest_station_phdis +
       stations_within_5km + stations_within_10km + stations_within_15km +
       population_within_10km + stations_per_million_pop_10km|   date  | 0 | date,
     data = analysis_data_2,
     na.action = na.omit)
summary(model)
