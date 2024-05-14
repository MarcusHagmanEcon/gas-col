competition_metrics <- function(index, df){
  
  cat("Processing station:", index, "\n")
  
  target_lat <- df$lat[index]
  target_lng <- df$lng[index]
  target_brand <- df$brand[index]
  
  phdis <- mapply(haversine_distance, 
                      lat1 = rep(target_lat, nrow(df)), 
                      lng1 = rep(target_lng, nrow(df)), 
                      lat2 = df$lat, 
                      lng2 = df$lng)
  
  phdis <- replace(phdis, phdis == 0, Inf)
  phdis_rank <- rank(phdis, ties.method = "first")
  
  number_close <- 15 # Indicates number of stations to find duration to
  drdis_drdur_nearest <- mapply(driving_duration,
                                lat1 = rep(target_lat, number_close),
                                lng1 = rep(target_lng, number_close),
                                lat2 = df$lat[which(phdis_rank <= number_close)],
                                lng2 = df$lng[which(phdis_rank <= number_close)])
  
  drdis <- drdis_drdur_nearest[1,]
  drdur <- drdis_drdur_nearest[2,]
  drdis_rank <- rank(drdis, ties.method = "first")
  drdur_rank <- rank(drdur, ties.method = "first")
  
  distance_rank_of_nearest_station_drdur <-
    phdis_rank[which(phdis_rank <= number_close)][which(drdur_rank == 1)]
  distance_rank_of_nearest_station_drdis <-
    phdis_rank[which(phdis_rank <= number_close)][which(drdis_rank == 1)]
  
  brand_of_nearest_station_drdur <- 
    df$brand[which(phdis_rank <= number_close)][which(drdur_rank == 1)]
  brand_of_2nd_nearest_station_drdur <- 
    df$brand[which(phdis_rank <= number_close)][which(drdur_rank == 2)]
  brand_of_3rd_nearest_station_drdur <- 
    df$brand[which(phdis_rank <= number_close)][which(drdur_rank == 3)]
  
  drdur_to_nearest_station <- drdur[which(drdur_rank == 1)]/600
  drdur_to_2nd_nearest_station <- drdur[which(drdur_rank == 2)]/600
  drdur_to_3rd_nearest_station <- drdur[which(drdur_rank == 3)]/600
  
  brand_of_nearest_station_drdis <- 
    df$brand[which(phdis_rank <= number_close)][which(drdis_rank == 1)]
  brand_of_2nd_nearest_station_drdis <- 
    df$brand[which(phdis_rank <= number_close)][which(drdis_rank == 2)]
  brand_of_3rd_nearest_station_drdis <- 
    df$brand[which(phdis_rank <= number_close)][which(drdis_rank == 3)]
  
  drdis_to_nearest_station <- drdis[which(drdis_rank == 1)]
  drdis_to_2nd_nearest_station <- drdis[which(drdis_rank == 2)]
  drdis_to_3rd_nearest_station <- drdis[which(drdis_rank == 3)]
  
  brand_of_nearest_station_phdis <- df$brand[which(phdis_rank == 1)]
  brand_of_2nd_nearest_station_phdis <- df$brand[which(phdis_rank == 2)]
  brand_of_3rd_nearest_station_phdis <- df$brand[which(phdis_rank == 3)]
  
  phdis_to_nearest_station <- phdis[which(phdis_rank == 1)]
  phdis_to_2nd_nearest_station <- phdis[which(phdis_rank == 2)]
  phdis_to_3rd_nearest_station <- phdis[which(phdis_rank == 3)]
  
  stid_of_nearest_station_phdis <- df$id[which(phdis_rank == 1)]
  stid_of_2nd_nearest_station_phdis <- df$id[which(phdis_rank == 2)]
  stid_of_3rd_nearest_station_phdis <- df$id[which(phdis_rank == 3)]
  
  stid_of_nearest_station_drdur <- df$id[which(drdur_rank == 1)]
  stid_of_2nd_nearest_station_drdur <- df$id[which(drdur_rank == 2)]
  stid_of_3rd_nearest_station_drdur <- df$id[which(drdur_rank == 3)]
  
  stid_of_nearest_station_drdis <- df$id[which(drdis_rank == 1)]
  stid_of_2nd_nearest_station_drdis <- df$id[which(drdis_rank == 2)]
  stid_of_3rd_nearest_station_drdis <- df$id[which(drdis_rank == 3)]
  
  stations_within_5km <- sum(phdis < 5)
  stations_within_10km <- sum(phdis < 10)
  stations_within_15km <- sum(phdis < 15)
  
  result <- c("brand_of_nearest_station_drdur" = brand_of_nearest_station_drdur,
              "brand_of_2nd_nearest_station_drdur" = brand_of_2nd_nearest_station_drdur,
              "brand_of_3rd_nearest_station_drdur" = brand_of_3rd_nearest_station_drdur,
              "drdur_to_nearest_station" = drdur_to_nearest_station,
              "drdur_to_2nd_nearest_station" = drdur_to_2nd_nearest_station,
              "drdur_to_3rd_nearest_station" = drdur_to_3rd_nearest_station,
              "brand_of_nearest_station_drdis" = brand_of_nearest_station_drdis,
              "brand_of_2nd_nearest_station_drdis" = brand_of_2nd_nearest_station_drdis,
              "brand_of_3rd_nearest_station_drdis" = brand_of_3rd_nearest_station_drdis,
              "drdis_to_nearest_station" = drdis_to_nearest_station,
              "drdis_to_2nd_nearest_station" = drdis_to_2nd_nearest_station,
              "drdis_to_3rd_nearest_station" = drdis_to_3rd_nearest_station,
              "brand_of_nearest_station_phdis" = brand_of_nearest_station_phdis,
              "brand_of_2nd_nearest_station_phdis" = brand_of_2nd_nearest_station_phdis,
              "brand_of_3rd_nearest_station_phdis" = brand_of_3rd_nearest_station_phdis,
              "phdis_to_nearest_station" = phdis_to_nearest_station,
              "phdis_to_2nd_nearest_station" = phdis_to_2nd_nearest_station,
              "phdis_to_3rd_nearest_station" = phdis_to_3rd_nearest_station,
              "distance_rank_of_nearest_station_drdur" = distance_rank_of_nearest_station_drdur,
              "distance_rank_of_nearest_station_drdis" = distance_rank_of_nearest_station_drdis,
              "stations_within_5km" = stations_within_5km,
              "stations_within_10km" = stations_within_10km,
              "stations_within_15km" = stations_within_15km,
              "stid_of_nearest_station_phdis" = stid_of_nearest_station_phdis)
  
  return(result)
}

# Returns driving distance and duration
driving_duration <- function(lat1, lng1, lat2, lng2) {
  
  repeat {
    
    success <- FALSE  # This variable will keep track if the code executed without errors
    
    # 3. Error Handling using tryCatch
    tryCatch({
      route <- osrmRoute(src = cbind(lng1, lat1),
                         dst = cbind(lng2, lat2))
      drdis <- route$distance
      drdur <- route$duration * 600
      
      success <- TRUE  # If the code reached here, it means no errors occurred
    }, error = function(e) {
      cat("Error:", e$message, "\n")
      # Additional error handling, if necessary, goes here
    })
    
    if (success) {
      break  # Exit the loop if the code executed without errors
    }
  }
  
  return(c("drdis" = drdis,
           "drdur" = drdur,
           "ratio" = 10^4 * drdis/drdur))
}



#### tie_breaker functions have been made obsolete after finding ties.method argument in rank()

# This function takes in a list of integers and adds a number x, st 0 <= x <1
#which enables identical integers to have the same ranking.
# tie_breaker_drdur <- function(input_list){
#   n <- length(input_list)
#   
#   result <- lapply(seq_along(input_list), function(i) input_list[[i]] + (i-1)/n)
#   
#   # Convert the result to a vector
#   result_vector <- unlist(result)
#   
#   # Print the result
#   return(result_vector)
# }
# 
# # Very similar to above, but downscaled by 10^(-4) because of different smallest unit.
# tie_breaker_drdis <- function(input_list){
#   n <- length(input_list)
#   
#   result <- lapply(seq_along(input_list), function(i) input_list[[i]] + 10^(-4)*(i-1)/n)
#   
#   # Convert the result to a vector
#   result_vector <- unlist(result)
#   
#   # Print the result
#   return(result_vector)
# }
# 
# competition_metrics_0 <- function(index, df){
#   
#   cat("Processing station:", index, "\n")
#   
#   target_lat <- df$lat[index]
#   target_lng <- df$lng[index]
#   target_brand <- df$brand[index]
#   
#   distances <- mapply(haversine_distance, 
#                       lat1 = rep(target_lat, nrow(df)), 
#                       lng1 = rep(target_lng, nrow(df)), 
#                       lat2 = df$lat, 
#                       lng2 = df$lng)
#   
#   distances <- replace(distances, distances == 0, Inf)
#   
#   brand_of_nearest_station <- df$brand[which.min(distances)]
#   number_within_5km <- sum(distances < 5)
#   number_within_10km <- sum(distances < 10)
#   number_within_15km <- sum(distances < 15)
#   
#   result <- c("brand_of_nearest_station" = brand_of_nearest_station,
#               "number_within_5km" = number_within_5km,
#               "number_within_10km" = number_within_10km,
#               "number_within_15km" = number_within_15km)
#   
#   return(result)
# }