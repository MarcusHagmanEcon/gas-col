run_and_plot_model <- function(data, diff_oil_terms, diff_e5_terms, response_function, response_args, plot_shapes = c(16, 17)) {
  # Create the formula
  formula_string <- "diff_e5_1 ~ " %>%
    paste(paste(diff_oil_terms, collapse = " + ")) %>%
    paste(paste(diff_e5_terms, collapse = " + "), sep = " + ") %>%
    paste0(" + lag_ect | stid | 0 | date + stid") %>%
    formula()
  
  # Fit the model
  model <- felm(formula_string, data = data)
  summary(model)
  
  # Compute cumulative response
  model_crf <- do.call(response_function, c(list(model), response_args))
  model_crf$lag <- seq_len(nrow(model_crf)) - 1
  
  # Plot
  crf_plot <- ggplot() +
    geom_point(data = model_crf, aes(x = lag, y = all_coef, color = "Series", shape = plot_shapes[1])) +
    geom_line(data = model_crf, aes(x = lag, y = all_coef, color = "Series")) +
    geom_errorbar(data = model_crf, aes(x = lag, ymin = all_coef - 1.96 * all_se, ymax = all_coef + 1.96 * all_se, color = "Series"), width = 0.1) +
    labs(x = "Days After Change in Oil Price", y = "Cumulative Effect on Gasoline price (95% Confidence Interval)", color = "Series") +
    theme_minimal() +
    scale_shape_manual(values = plot_shapes)
  
  return(crf_plot)
}