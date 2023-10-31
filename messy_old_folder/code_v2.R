rm(list=ls())

setwd("C:/Users/marcu/Documents/gasoline")  # Replace with the actual path to your desired directory

library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(osrm)
library(lfe)

gas_station <- read.csv("gas_station.csv")
oil <- read.csv("Europe_Brent_Spot_Price_FOB.csv", skip = 4) %>% 
  mutate(Day = as.Date(Day, format="%m/%d/%Y"))
colnames(oil) = c("date_day", "brent")

gas_station_information_history <- read.csv("gas_station_information_history.csv")

#Driving durations
# for (src in 1:nrow(example)){
#   print(src)
#   close_count <- -1
#   for(dst in 1:nrow(example)){
#     duration <- osrmRoute(src = cbind(example$lng[src], example$lat[src]), 
#                           dst = cbind(example$lng[dst], example$lat[dst]))$duration
#     sum_sq_inv_dist <- 0
#     if (duration < 100){
#        close_count <- close_count + 1
#     }
#     if (duration > 0){
#       sum_sq_inv_dist <- sum_sq_inv_dist + duration^(-2)
#     }
#   }
#   example$neighbors[src] <- close_count
# }

gas_station <- as.data.frame( table(gas_station$brand) ) %>% rename(brand = Var1) %>%
  mutate(same_brand = Freq /nrow(gas_station)) %>% select( c(brand, same_brand) ) %>%
  full_join(gas_station, by = c("brand"))


haversine_distance <- function(lat1, lng1, lat2, lng2) {
  # Convert degrees to radians
  lat1 <- lat1 * pi / 180
  lng1 <- lng1 * pi / 180
  lat2 <- lat2 * pi / 180
  lng2 <- lng2 * pi / 180
  
  R <- 6371  # Earth's radius in km
  
  dlat <- lat2 - lat1
  dlng <- lng2 - lng1
  
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlng / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  
  distance <- R * c
  return(distance)
}

get_fraction_same_brand_within_30km <- function(index, df) {
  
  cat("Processing station:", index, "\n")
  
  target_lat <- df$lat[index]
  target_lng <- df$lng[index]
  target_brand <- df$brand[index]
  
  distances <- mapply(haversine_distance, 
                      lat1 = rep(target_lat, nrow(df)), 
                      lng1 = rep(target_lng, nrow(df)), 
                      lat2 = df$lat, 
                      lng2 = df$lng)
  
  # Check if distance is within 30 km and brand is the same
  within_30km_same_brand <- sum(distances < 30 & df$brand == target_brand)
  within_30km <- sum(distances < 30)
  
  # Subtract 1 to exclude the station itself from the fraction
  fraction <- (within_30km_same_brand - 1) / (within_30km - 1)
  
  return(fraction)
}

gas_station$same_brand_close <- sapply(1:nrow(gas_station), get_fraction_same_brand_within_30km, df=gas_station)

gas_station$overall_minus_close <- gas_station$same_brand - gas_station$same_brand_close

lm(overall_minus_close ~ 1, data = gas_station) %>% summary

gas_station_information_history_1 <- gas_station_information_history %>%
  rename(date_time = date) %>% mutate(date = as.Date(substr(date_time,1, 10)))

df0 <- gas_station_information_history_1 %>% group_by(stid, date) %>%
  summarize(e5 = mean(e5, na.rm = TRUE), .groups = 'drop')

df1 <- df0 %>% left_join(gas_station, by = c("stid" = "id")) %>% 
  select(c(e5, same_brand_close, brand, date, stid))

model <- felm(e5 ~ same_brand_close | date | 0 | date, data = df1)

summary(model)
