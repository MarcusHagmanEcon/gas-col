# Returns distance in km

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