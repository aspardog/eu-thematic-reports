scatterPlot <- function(data2plot,
                        xvariable  = 'value2plot_id1',
                        yvariable =  'value2plot_id2',
                        category   = 'nuts_id') {
  
  # Convert xvariable and yvariable to numeric if they are not already
  data2plot[[xvariable]] <- as.numeric(data2plot[[xvariable]]) * 100
  data2plot[[yvariable]] <- as.numeric(data2plot[[yvariable]]) * 100
  
  # Check for NA or non-numeric values and replace with NA
  data2plot[[xvariable]][!is.na(data2plot[[xvariable]]) & !is.numeric(data2plot[[xvariable]])] <- NA
  data2plot[[yvariable]][!is.na(data2plot[[yvariable]]) & !is.numeric(data2plot[[yvariable]])] <- NA
  
  # Calculate axis limits
  x_limits <- range(data2plot[[xvariable]], na.rm = TRUE)
  y_limits <- range(data2plot[[yvariable]], na.rm = TRUE)
  
  # Tooltip data
  # tooltip_data <- data2plot %>%
  #   mutate(tooltip = glue::glue(
  #     "<i>{nuts_id}</i>, <b>{country_name_ltn}</b><br>",
  #     "Value 1 Percentage: {xvariable}%<br>",
  #     "Value 2 Percentage: {yvariable}%"
  #   ))
  
  plot <- ggplot(data = data2plot, aes_string(x = xvariable, y = yvariable, color = category)) + 
    geom_point(show.legend = FALSE) + 
    geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") + 
    # geom_text(data = tooltip_data, aes(label = tooltip), vjust = -0.5, color = "black",
    #           family = "Lato Full", size = 2.460219 * .pt, box.color = "white",
    #           hjust = 0, parse = FALSE, show.legend = FALSE) +
    scale_y_continuous(limits = y_limits + c(-10, 10),
                       breaks = seq(0, 100, 20),
                       labels = paste0(seq(0, 100, 20), "%"),
                       position = "left", expand = c(0, 0)) +
    scale_x_continuous(limits = x_limits + c(-10, 10),
                       breaks = seq(0, 100, 20),
                       labels = paste0(seq(0, 100, 20), "%"),
                       position = "bottom", expand = c(0, 0)) +
    WJP_theme() +
    theme(axis.line        = element_line(color    = "#5e5c5a", linetype = "solid"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = 'none')
  
  # Return the ggplot object
  print(plot)
  return(plot)
}






