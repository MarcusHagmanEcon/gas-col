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
library(corrplot)

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

#closest_neighbor <- sapply(1:nrow(gas_station), nearest_station_brand, df = gas_station)

gas_station$nearest_station_brand <- closest_neighbor
gas_station <- gas_station %>% mutate(brand_and_neighbor = paste(brand, nearest_station_brand))

#saveRDS(gas_station, file = "stored_dfs/gas_station_with_neighbor.Rdata")

# Identify the 10 most common occurrences in 'brand_and_neighbor'
most_common_brands <- gas_station %>%
  count(brand, sort = TRUE) %>%
  head(5) %>%
  pull(brand)

# Create the new column based on whether the value in 'brand_and_neighbor' is in the top 10 most common
gas_station <- gas_station %>%
  mutate(brand_and_neighbor_most_common = if_else(brand %in% most_common_brands & nearest_station_brand  %in% most_common_brands, brand_and_neighbor, ""))

gas_station_information_history <- gas_station_information_history %>%
  rename(date_time = date) %>% mutate(date = as.Date(substr(date_time,1, 10)))

df0 <- gas_station_information_history %>% group_by(stid, date) %>%
  summarize(e5 = mean(e5, na.rm = TRUE), .groups = 'drop')

#save(df0, file = "stored_dfs/daily_mean_prices.rds")

df1 <- df0 %>% left_join(gas_station, by = c("stid" = "id")) %>% 
  select(c(e5, nearest_station_brand, brand_and_neighbor, brand_and_neighbor_most_common, brand, date, stid))

df2 <- df1 %>% filter(e5 != 0)

# Create an empty dataframe to store coefficients
coefs <- data.frame()
unique_dates <- sort(unique(df2$date))

# Loop over all unique dates in df2
for (d in unique_dates) {
  print(d)
  
  model <- felm(e5 ~ as.factor(brand_and_neighbor_most_common) | brand | 0 | brand,
                data = df2 %>% filter(date == d),
                na.action = na.omit)
  
  # Bind the coefficients from the model to the coefs dataframe
  coefs <- rbind(coefs, t(model$coefficients))
}

# If you want to add a date column to the coefs dataframe
coefs$date <- unique_dates
names(coefs) <- sub(".*?\\)", "", names(coefs))
names(coefs) <- gsub(" ", "_", names(coefs))

coefs_mavg <- coefs
cols_to_average <- setdiff(names(coefs), "date")
coefs_mavg[cols_to_average] <- lapply(coefs[cols_to_average], function(x) rollmean(x, k=14, fill=NA, align="right"))

coefs_long <- gather(coefs_mavg %>% select("AVIA_ESSO", "ESSO_AVIA", "date"), key="variable", value="value", -date)
#coefs_long <- gather(coefs_mavg, key="variable", value="value", -date)

ggplot(data=coefs_long, aes(x=date, y=value, color=variable)) +
  geom_line() +
  labs(title="Line Graph Over Time", x="Date", y="Value") +
  theme_minimal()

model <- lm(ARAL_ESSO ~ ESSO_ARAL, data = coefs)
summary(model)

cor_matrix <- cor(coefs %>% select(-c(date)))
print(cor_matrix)

coefs_diff <-( coefs[2:nrow(coefs),1:25] - coefs[1:(nrow(coefs)-1),1:25] ) %>%
  cbind(coefs$date[2:nrow(coefs)])

same_first_total <- 0
same_first_n <- 0

reverse_total <- 0
reverse_n <- 0

same_last_total <- 0
same_last_n <- 0

same_last_total <- 0
same_last_n <- 0

other_total <- 0
other_n <- 0

rev_corrs_and_coefs <- data.frame()
p_vals <- data.frame()

for (i in 1:nrow(cor_matrix)){
  for (j in 1:nrow(cor_matrix)){
    if (floor((i-1)/5) == floor((j-1)/5) & i != j){
      same_first_total <- same_first_total + cor_matrix[i,j]
      same_first_n <- same_first_n + 1
    } else if(((i-1)%%5)*5+floor((i-1)/5)+1 == j & i != j){
      reverse_total <- reverse_total + cor_matrix[i,j]
      reverse_n <- reverse_n + 1
      
      rev_corrs_and_coefs <- rbind(rev_corrs_and_coefs, 
                                   c(cor_matrix[i,j],
                                     mean(coefs[,i])))
      #print(paste(colnames(cor_matrix)[i], ", ", colnames(cor_matrix)[j]))
    } else if (i %% 5 == j %% 5 & i != j){
      same_last_total <- same_last_total + cor_matrix[i,j]
      same_last_n <- same_last_n + 1
      #print(paste(colnames(cor_matrix)[i], ", ", colnames(cor_matrix)[j]))
    } else {
      other_total <- other_total + cor_matrix[i,j]
      other_n <- other_n + 1
    }
    if(i == j){
      p_vals[i,j] = 1
    }
    if (i != j){
      model_summary <- lm(coefs_diff[,i] ~ coefs_diff[,j]) %>% summary()
      p_vals[i,j] <- model_summary$coefficients[, "Pr(>|t|)"][2][[1]]
    }
  }
}
same_first_mean <- same_first_total / same_first_n
reverse_mean <- reverse_total / reverse_n
same_last_mean <- same_last_total / same_last_n
other_mean <- other_total / other_n

names(rev_corrs_and_coefs) <- c("corr", "coef")

ggplot(rev_corrs_and_coefs, aes(x=corr, y=coef)) + 
  geom_point()
sum(rev_corrs_and_coefs$corr)

model <- lm(coefs_diff[,1] ~ coefs_diff[,2])
model_summary <- summary(model)
model_summary$coefficients[, "Pr(>|t|)"][2][[1]]
model <- lm(AVIA_ARAL ~ ARAL_AVIA, data = coefs_diff)
summary(model)
 
colMeans(coefs %>% select(-c(date))) %>% write.csv(, file = "stored_dfs/means.csv")
