event_study_graph <- function(agges, start_time, end_time, title_text){
  df <- cbind(event_time  = agges$egt,
              est = agges$att.egt,
              se  = agges$se.egt) %>% 
    as.data.frame() %>% 
    filter(event_time >= start_time & event_time <= end_time)
  
  graph <- ggplot(df, aes(x = event_time, y = est)) + 
    geom_point(stat = "identity", position = position_dodge(width = 1), 
               aes(color = ifelse(event_time >= 0, "darkblue", "darkred"))) + # Color points conditionally
    geom_errorbar(aes(ymin = est - agges$crit.val.egt*se, ymax = est + agges$crit.val.egt*se, 
                      color = ifelse(event_time >= 0, "darkblue", "darkred")), width = 0.4) + # Color error bars conditionally
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
    scale_color_identity() + 
    theme_minimal() +
    labs(x = "Days Relative to Nearby Station Opening", y = "Estimate (95% CI)", 
         title = title_text) +
    theme(plot.title = element_text(size = 20),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14))
  
  return(graph)
}