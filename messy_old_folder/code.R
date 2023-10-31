rm(list=ls())

setwd("C:/Users/marcu/Documents/gasoline")  # Replace with the actual path to your desired directory

library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyverse)

gas_station <- read.csv("gas_station.csv")
oil <- read.csv("Europe_Brent_Spot_Price_FOB.csv", skip = 4) %>% 
  mutate(Day = as.Date(Day, format="%m/%d/%Y"))
colnames(oil) = c("date_day", "brent")


gas_station_information_history <- read.csv("gas_station_information_history.csv")

#gas_station_information_history_trunc <- gas_station_information_history[1:1000000,]

avg_daily_prices <- gas_station_information_history %>% 
  mutate(date_day = as.Date(substr(date,1,10))) %>%
  group_by(date_day) %>%
  summarize(avg_e5 = mean(e5), avg_e10 = mean(e10), avg_diesel = mean(diesel)) %>% 
  left_join(oil, by = c("date_day")) %>%
  mutate(brent = 10*brent)

# Plot for avg_e5
ggplot(avg_daily_prices, aes(x = date_day, y = avg_e5)) +
  geom_line() +
  ggtitle("Average E5 Prices over Time") +
  xlab("Date") +
  ylab("Average E5 Price")

# Plot for avg_e10
ggplot(avg_daily_prices, aes(x = date_day, y = avg_e10)) +
  geom_line() +
  ggtitle("Average E10 Prices over Time") +
  xlab("Date") +
  ylab("Average E10 Price")

# Plot for avg_diesel
ggplot(avg_daily_prices, aes(x = date_day, y = avg_diesel)) +
  geom_line() +
  ggtitle("Average Diesel Prices over Time") +
  xlab("Date") +
  ylab("Average Diesel Price")

# Melt the data to a longer format
long_data <- melt(avg_daily_prices%>% filter(!is.na(brent)), id.vars = "date_day", measure.vars = c("avg_e5", "avg_e10", "avg_diesel", "brent"))

ggplot(long_data, aes(x = date_day, y = value, color = variable)) +
  geom_line() +
  ggtitle("Fuel Prices over Time") +
  xlab("Date") +
  ylab("Price") +
  scale_color_manual(values = c("red", "blue", "green", "orange"), 
                     name = "Fuel Type", 
                     breaks = c("avg_e5", "avg_e10", "avg_diesel", "brent"),
                     labels = c("E5", "E10", "Diesel", "Brent")) +
  theme_minimal()

reg_data <- avg_daily_prices %>%
  filter(!is.na(brent)) %>%
  mutate(diff_e5 = avg_e5 - lag(avg_e5), diff_brent = brent - lag(brent), 
         markup = (avg_e5 - brent)/brent, markup_x_lag_diff_brent = markup * lag(diff_brent),
         lag_brent_pos = lag(diff_brent) > 0,
         lag_brent_pos_x_lag_brent = lag_brent_pos * lag(diff_brent),
         diff2_e5 = avg_e5 - lag(lag(avg_e5)), diff2_brent = brent - lag(lag(brent)),
         lag_diff2_brent = lag(diff2_brent),
         up_down = as.integer( lag(lag(diff_brent)) > 0 & lag(diff_brent) < 0),
         down_up = lag(lag(diff_brent)) < 0 & lag(diff_brent) > 0,
         up_down_x_lag_diff2_brent = up_down * lag_diff2_brent )

model <- lm(diff2_e5 ~ lag(diff_brent) + lag(lag(diff_brent)) +  lag_diff2_brent + lag(lag_diff2_brent) + up_down + up_down_x_lag_diff2_brent, 
            data = reg_data %>% filter(down_up | up_down) )
summary(model)
model <- lm(diff2_e5 ~ lag_diff2_brent, 
            data = reg_data %>% filter(down_up ) )
summary(model)
model <- lm(diff2_e5 ~ lag_diff2_brent, 
            data = reg_data %>% filter( up_down) )
summary(model)

model_neg1 <- lm(diff_e5 ~ lag(diff_brent), data = reg_data %>% filter(lag(diff_brent) < 0))
summary(model_neg1)
model_neg2 <- lm(diff_e5 ~ lag(diff_brent) + markup_x_lag_diff_brent, data = reg_data %>% filter(lag(diff_brent) < 0))
summary(model_neg2)
model_neg3 <- lm(diff_e5 ~ lag(diff_brent), data = reg_data %>% 
                   filter(lag(diff_brent) < 0 & lag(lag(diff_brent) < 0)))
summary(model_neg3)
model_neg4 <- lm(diff_e5 ~ lag(diff_brent), data = reg_data %>% 
                   filter(lag(diff_brent) < 0 & lag(lag(diff_brent) > 0)))
summary(model_neg4)

model_pos1 <- lm(diff_e5 ~ lag(diff_brent), data = reg_data %>% filter(lag(diff_brent) > 0))
summary(model_pos1)
model_pos2 <- lm(diff_e5 ~ lag(diff_brent) +  markup_x_lag_diff_brent, data = reg_data %>% filter(lag(diff_brent) > 0))
summary(model_pos2)
model_pos3 <- lm(diff_e5 ~ lag(diff_brent), data = reg_data %>% 
                   filter(lag(diff_brent) > 0 & lag(lag(diff_brent) > 0)))
summary(model_pos3)
model_pos4 <- lm(diff_e5 ~ lag(diff_brent), data = reg_data %>% 
                   filter(lag(diff_brent) > 0 & lag(lag(diff_brent) < 0)))
summary(model_pos4)

long_data2 <- melt(reg_data, id.vars = "date_day", measure.vars = c("markup", "ratio_neg", "ratio_pos"))

ggplot(long_data2, aes(x = date_day, y = value, color = variable)) +
  geom_line() +
  ggtitle("") +
  xlab("Date") +
  theme_minimal()

ggplot(reg_data%>% filter( up_down), aes(x=diff2_brent, y=diff2_e5)) +
  geom_point() +                      # Scatterplot
  geom_smooth(method="lm") +      # Nonparametric curve
  coord_cartesian(xlim=c(-5, 5), ylim=c(-3, 3)) +   # Adjust visible area
  theme_minimal()                    # Optional theme

ggplot(reg_data%>% filter(down_up), aes(x=diff2_brent, y=diff2_e5)) +
  geom_point() +                      # Scatterplot
  geom_smooth(method="lm") +      # Nonparametric curve
  coord_cartesian(xlim=c(-5, 5), ylim=c(-3, 3)) +   # Adjust visible area
  theme_minimal()                    # Optional theme
