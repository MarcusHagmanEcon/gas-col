# add_obs_start_end1 <- function(df){
#   unique_stid <- unique(df$stid)
#   unique_dates <- unique(df$date) %>% sort()
#   
#   result <- data.frame()
#   
#   for (station in unique_stid){
#     print(paste("Adding start and end of day observations: ", which(unique(df$stid) == station ) / length(unique_stid) ))
#     station_dates <- df %>% filter(stid == station) %>% select(date) %>% pull()
#     first_date <- min(station_dates)
#     last_date <- max(station_dates)
#     for (date_index in 1:length(unique_dates) ){
#       date <- unique_dates[date_index]
#       if (date >= first_date & date <= last_date) {
#         result <- result %>%
#           rbind( data.frame("stid" = station, 
#                             "e5"= NA,"e10" = NA, "diesel" = NA,
#                             "date" = date, 
#                             "datetime" = ymd_hms(paste(as.character(date), "07:00:00")), 
#                             "price_changed" = FALSE))
#         result <- result %>%
#           rbind( data.frame("stid" = station, 
#                             "e5"= NA,"e10" = NA, "diesel" = NA,
#                             "date" = date, 
#                             "datetime" =ymd_hms(paste(as.character(date), "21:00:00")), 
#                             "price_changed" = FALSE))
#       }
#     }
#   }
#   return(result)
# }

add_obs_start_end <- function(df){
  unique_stid <- unique(df$stid)
  unique_dates <- unique(df$date) %>% sort()
  result <- expand.grid(date = unique_dates, stid = unique_stid)
  result <- result %>%
    slice(rep(1:n(), each=2)) %>%
    mutate(time = ifelse(row_number() %% 2 == 1, "07:00:00", "21:00:00"))
  result <- result %>% mutate(e5 = NA, e10 = NA, diesel = NA,
                      datetime = ymd_hms(paste(date, time)),
                      price_changed = FALSE) %>% select(-c(time))
  return(result)
}