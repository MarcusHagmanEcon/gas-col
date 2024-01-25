
cumulative_response_plot <- function(crf_list, legend_labels, color_list = c("#000080", "#9400D3", "#228B22", "#556B2F")) {
  
  if(length(crf_list) != length(legend_labels)) {
    stop("The number of dataframes and legend labels must be equal.")
  }
  
  # Apply offset to each dataframe and combine them with a label
  for(i in 1:length(crf_list)) {
    crf_list[[i]]$lag <- crf_list[[i]]$lag + 0.1 * (i - 1)  # Apply offset
    crf_list[[i]]$Label <- factor(legend_labels[i], levels = legend_labels)
  }
  combined_crf <- do.call(rbind, crf_list)
  
  # Initialize the plot
  crf_plot <- ggplot(combined_crf, aes(x = lag, y = coef, group = Label, color = Label)) + 
    labs(x = "Days After Change in Oil Price", y = "Cumulative Effect on Gasoline price (95% Confidence Interval)") +
    theme_minimal()
  
  # Add points, lines, and error bars
  crf_plot <- crf_plot + 
    geom_point(alpha = 0.7) +
    geom_line(alpha = 0.7) +
    geom_errorbar(aes(ymin = coef - 1.96 * se, ymax = coef + 1.96 * se), width = 0.1, alpha = 0.7)
  
  # Add color legend
  crf_plot <- crf_plot + scale_color_manual(values = color_list, name = "")
  
  return(crf_plot)
}