#Takes input coordinates as long lat
#Radius in meters
population_within_radius <- function(lat, lng, pop_data, radius){
  points_sf <- c(lng, lat) %>% st_point() %>% st_sfc(crs = 4326)
  point_transformed <- st_transform(points_sf, 3035)
  coords <- st_coordinates(point_transformed)
  
  N <- coords[1, "Y"]
  E <- coords[1, "X"]
  N_round <- 10^3 * floor(N * 10^(-3))
  E_round <- 10^3 * floor(E * 10^(-3))
  
  pop <- pop_data %>% filter((north - N_round)^2 + (east - E_round)^2 < radius ^2) %>%
    select(inhabitants) %>% pull() %>% sum() %>% as.integer()
  
  return(pop)
}