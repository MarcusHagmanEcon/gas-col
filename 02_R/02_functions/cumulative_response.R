# Calculates the cumulative response by adding up coefficient estimates
#and estimates standard errors of the cumulative responses using the delta method

# model = regression model
# row_indices = vector of integers of elements to calculate cumulative response over
# col_label = string for labling result
cumulative_response <- function(model, row_indices, col_label){
  result <- data.frame(rep(NA, length(row_indices)), rep(NA, length(row_indices)))
  names(result) <- c(paste0(col_label, "_coef"), paste0(col_label, "_se"))
  for (i in row_indices){
    result[which(row_indices == i), paste0(col_label, "_coef")] <-
      sum(model$coefficients[row_indices[1:which(row_indices == i)]])
    result[which(row_indices == i), paste0(col_label, "_se")] <-
      sum(vcov(model)[row_indices[1:which(row_indices == i)],
                      row_indices[1:which(row_indices == i)]]) %>%
      sqrt()
  }
  return(result)
}
