rm(list=ls())

setwd("C:/Users/marcu/Documents/gasoline")

library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(osrm)
library(lfe)
library(sandwich)
library(lmtest)

gas_station <- read.csv("gas_station.csv") %>% filter(name != "01_test")
oil <- read.csv("Europe_Brent_Spot_Price_FOB.csv", skip = 4) %>% 
  mutate(Day = as.Date(Day, format="%m/%d/%Y"))
colnames(oil) = c("date_day", "brent")

gas_station_information_history <- read.csv("gas_station_information_history.csv")

# abc <- gas_station_information_history %>% mutate(test <- stid %in% gas_station$id)
# (nrow(abc) - sum(abc$test))/nrow(abc)


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

driving_duration <- function(lat1, lng1, lat2, lng2) {
  
  repeat {
    
    success <- FALSE  # This variable will keep track if the code executed without errors
    
    # 3. Error Handling using tryCatch
    tryCatch({
      duration <- osrmRoute(src = cbind(lng1, lat1),
                            dst = cbind(lng2, lat2))$duration
      
      success <- TRUE  # If the code reached here, it means no errors occurred
    }, error = function(e) {
      cat("Error:", e$message, "\n")
      # Additional error handling, if necessary, goes here
    })
    
    if (success) {
      break  # Exit the loop if the code executed without errors
    }
  }
  
  return(duration)
}

neighboring_stations <- function(short_dist, short_dur, index, df) {
  
  cat("Processing station:", index, "\n")
  
  target_lat <- df$lat[index]
  target_lng <- df$lng[index]
  target_brand <- df$brand[index]
  
  distances <- mapply(haversine_distance, 
                      lat1 = rep(target_lat, nrow(df)), 
                      lng1 = rep(target_lng, nrow(df)), 
                      lat2 = df$lat, 
                      lng2 = df$lng)
  
  close_stations <- df[distances < short_dist,]
  
  durations <- rep(Inf, nrow(df))
  durations[distances < short_dist] <- mapply(driving_duration,
                                       lat1 = rep(target_lat, nrow(close_stations)), 
                                       lng1 = rep(target_lng, nrow(close_stations)), 
                                       lat2 = close_stations$lat, 
                                       lng2 = close_stations$lng)
  
  # Check if distance is within short distance (bird's path) and brand is the same
  same_brand_close_dist <- sum(distances < short_dist & df$brand == target_brand & df$brand != "" & df$id != df$id[index])
  other_brand_close_dist <- sum(distances < short_dist & df$id != df$id[index]) - same_brand_close_dist
  
  # Check if duration is within short duration and brand is the same
  same_brand_close_dur <- sum(durations < short_dur & df$brand == target_brand & df$brand != "" & df$id != df$id[index])
  other_brand_close_dur <- sum(durations < short_dur & df$id != df$id[index]) - same_brand_close_dur
  
  result <- c("same_brand_close_dist" = same_brand_close_dist,
              "other_brand_close_dist" = other_brand_close_dist,
              "same_brand_close_dur" = same_brand_close_dur,
              "other_brand_close_dur" = other_brand_close_dur)
  print(result)
  
  return( result )
}

nearest_station_brand <- function(index, df){
  
  cat("Processing station:", index, "\n")
  
  target_lat <- df$lat[index]
  target_lng <- df$lng[index]
  target_brand <- df$brand[index]
  
  distances <- mapply(haversine_distance, 
                      lat1 = rep(target_lat, nrow(df)), 
                      lng1 = rep(target_lng, nrow(df)), 
                      lat2 = df$lat, 
                      lng2 = df$lng)
  
  distances <- replace(distances, distances == 0, Inf)
  
  result <- df$brand[which.min(distances)]
  
  return(result)
  
}

neighboring_stations_df <- sapply(1:nrow(gas_station),
                                  short_dist = 6,
                                  short_dur = 3,
                                  neighboring_stations, 
                                  df=gas_station)

gas_station$same_brand_neighbor <- neighboring_stations_df[,1]
gas_station$other_brand_neighbor <- neighboring_stations_df[,2]

gas_station_information_history_1 <- gas_station_information_history %>%
  rename(date_time = date) %>% mutate(date = as.Date(substr(date_time,1, 10)))

df0 <- gas_station_information_history_1 %>% group_by(stid, date) %>%
  summarize(e5 = mean(e5, na.rm = TRUE), .groups = 'drop')

df1 <- df0 %>% left_join(gas_station, by = c("stid" = "id")) %>% 
  select(c(e5, same_brand_neighbor, other_brand_neighbor, brand, date, stid))

df2 <- df1 %>% filter(e5 != 0)

model <- felm(e5 ~ same_brand_neighbor + other_brand_neighbor| date + brand | 0 | date + brand , data = df2, 
              na.action = na.omit)
summary(model)

df2 <- df2[order(df2$stid, df2$date), ]
