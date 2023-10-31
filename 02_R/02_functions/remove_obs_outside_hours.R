remove_obs_outside_hours <- function(df){
  result <- df 
  result <- result %>% mutate(hour = hour(datetime),
                          time_within_range = hour >= 7 & hour < 21)
  result <- result %>% filter(time_within_range)
  
  return(result)
  
}