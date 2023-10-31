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

# Load data
analysis_data <- readRDS("01_data/02_processed/temp_analysis_data.rds")

# Calculate quintile breakpoints
quintiles <- quantile(analysis_data$population_within_10km, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE)

# Categorize each value into its quintile and add as new column
analysis_data$quintile_category <- cut(analysis_data$population_within_10km, 
                            breaks = quintiles, 
                            labels = c("Q1", "Q2", "Q3", "Q4", "Q5"), 
                            include.lowest = TRUE)

# Unique dates
unique_dates <- sort(unique(analysis_data$date))

felm(nearest_station_same_brand ~ stations_per_mil:quintile_category | brand | 0 | brand,
                 data = analysis_data %>% filter(date == unique_dates[1]),
                 na.action = na.omit) %>% summary()

model_e52 <- felm(log_e5 ~ nearest_station_same_brand + number_within_5km + number_within_10km + number_within_15km| brand | 0 | brand,
                 data = analysis_data %>% filter(date == unique_dates[1]),
                 na.action = na.omit)

model_e53 <- felm(log_e5 ~ nearest_station_same_brand | brand | 0 | brand,
                  data = analysis_data %>% filter(date == unique_dates[1]),
                  na.action = na.omit)


# Generate first descriptive graph, showing that gas stations with nearest station
#belonging to the same brand charge higher prices
coefs_same_brand = data.frame()
# Loop over all unique dates in analysis_data
for (d in unique_dates) {
  print(d)
  model_e5 <- felm(log_e5 ~ nearest_station_same_brand + number_within_5km + number_within_10km + number_within_15km| brand | 0 | brand,
                   data = analysis_data %>% filter(date == d),
                   na.action = na.omit)
  
  model_e10 <- felm(log_e10 ~ nearest_station_same_brand + number_within_5km + number_within_10km + number_within_15km| brand | 0 | brand,
                   data = analysis_data %>% filter(date == d),
                   na.action = na.omit)
  
  model_diesel <- felm(log_diesel ~ nearest_station_same_brand + number_within_5km + number_within_10km + number_within_15km| brand | 0 | brand,
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
ggsave("03_outputs/figures/01_same_brand_price.png", p, width = 9, height = 10)




# Similar exercise but only e5 and by brand
most_common_brands <- c("ARAL", "AVIA", "ESSO", "Shell", "TOTAL")
coefs_same_brand_by_brand <- data.frame()
# Loop over all unique dates in analysis_data
for (d in unique_dates) {
  print(d)
  results <- c()
  for (b in most_common_brands){
    print(b)
    model <- lm(log_e5 ~ nearest_station_same_brand + number_within_5km + number_within_10km + number_within_15km,
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
ggsave("03_outputs/figures/02_same_brand_price_by_brand.png", p2, width = 9, height = 12)



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
      model <- lm(log_e5 ~ nearest_station_same_brand + same_other + number_within_5km + number_within_10km + number_within_15km,
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

ggsave("03_outputs/figures/03_shell_total_alphas.png", p3, width = 9, height = 4)

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
  
  model_e5 <- felm(e5 ~as.factor(brand_and_neighbor_most_common) + number_within_5km + number_within_10km + number_within_15km| brand | 0 | brand,
                data = analysis_data %>% filter(date == d),
                na.action = na.omit)
  
  model_e10 <- felm(e10 ~ as.factor(brand_and_neighbor_most_common) + number_within_5km + number_within_10km + number_within_15km| brand | 0 | brand,
                   data = analysis_data %>% filter(date == d),
                   na.action = na.omit)

  model_diesel <- felm(diesel ~ as.factor(brand_and_neighbor_most_common) + number_within_5km + number_within_10km + number_within_15km| brand | 0 | brand,
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
