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