fill_in_prices <- function(df){
  result <- df
  finished <- FALSE
  while (!finished){
    finished <- TRUE
    result <- result %>% mutate(fill_in = !price_changed &
                                          lag(stid) == stid &
                                          ((!is.na(lag(e5)) & is.na(e5))|
                                             (!is.na(lag(e10)) & is.na(e10)) |
                                             (!is.na(lag(diesel))  & is.na(diesel))  ) )


    if (result$fill_in %>% sum > 0 ){
      finished <- FALSE
      result <- result %>% mutate(e5 = ifelse(fill_in, lag(e5), e5),
                                  e10 = ifelse(fill_in, lag(e10), e10),
                                  diesel = ifelse(fill_in, lag(diesel), diesel))
    }
  }

  return(result)
}
