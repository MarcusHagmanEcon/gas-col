brand_of_nearest_station <- function(index, df){
  
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