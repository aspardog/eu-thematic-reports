gen_dots <- function(data, region_names) {
  
  # Apply population weights and get national level data
  data_wght <- data %>%
    left_join(region_names, by = "nuts_id") %>%
    mutate(
      weighted_value_id1 = value2plot_id1 * pop_weight,
      weighted_value_id2 = value2plot_id2 * pop_weight,
      level = "regional"
    )
  
  country_avg <- data_wght %>%
    group_by(country_name_ltn, chart_id, demographic) %>%
    summarise(
      nuts_id = first(nuts_id),
      value2plot_id1 = sum(weighted_value_id1, na.rm = TRUE),
      value2plot_id2 = sum(weighted_value_id2, na.rm = TRUE),
      count = sum(count),
      .groups = "keep"
    ) %>%
    mutate(
      nuts_id = substr(nuts_id, 1, 2),
      nameSHORT = country_name_ltn,
      level = "national"
    )
  
  # Transform data for plotting
  data_long <- country_avg %>%
    filter(level == "national") %>%
    mutate(
      value2plot_id1 = value2plot_id1 * 100,
      value2plot_id2 = value2plot_id2 * 100
    ) %>%
    pivot_longer(cols = starts_with("value2plot_id"), names_to = "variable", values_to = "value") %>%
    mutate(variable = ifelse(variable == "value2plot_id1", "Value 1", "Value 2"))
  
  # Add tooltip information
  data_long <- data_long %>%
    mutate(
      tooltip = glue("<b>{country_name_ltn}</b><br>{variable}: {round(value, 1)}%")
    )
  
  # Create strips for the background pattern
  strips <- data_long %>%
    group_by(country_name_ltn) %>%
    summarise() %>%
    mutate(
      ymin = 0,
      ymax = 100,
      xposition = rev(1:nrow(.)),
      xmin = xposition - 0.5,
      xmax = xposition + 0.5,
      fill = rep(c("grey", "white"), length.out = nrow(.))
    ) %>%
    pivot_longer(c(xmin, xmax), names_to = "cat", values_to = "x") %>%
    select(-cat) %>%
    filter(fill != "white")
  
  # Generate plot
  plt <- ggplot() +
    geom_blank(data = data_long, aes(x = reorder(country_name_ltn, -value), y = value, color = variable)) +
    geom_ribbon(data = strips, aes(x = x, ymin = ymin, ymax = ymax, group = xposition, fill = fill), show.legend = FALSE) +
    geom_point(data = data_long, aes(x = reorder(country_name_ltn, -value), y = value, color = variable), size = 4, show.legend = FALSE) +
    geom_richtext(data = data_long, aes(x = reorder(country_name_ltn, -value), y = value, label = tooltip), 
                  size = 2.5, hjust = 0.5, vjust = -1, fill = NA, label.color = NA, text.color = "black") +
    scale_fill_manual(values = c("grey" = "#EBEBEB", "white" = "#FFFFFF"), na.value = NULL) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), labels = paste0(seq(0, 100, 20), "%"), position = "right") +
    coord_flip() +
    scale_color_manual(values = c("Value 1" = "blue", "Value 2" = "red")) +
    WJP_theme() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.background = element_blank(), 
      panel.ontop = TRUE,
      axis.text.y = element_text(color = "#222221", hjust = 0)
    )
  
  # Add error bars
  alpha <- 0.05  # 95% confidence interval
  plt <- plt + 
    geom_errorbar(
      data = data_long,
      aes(
        x = reorder(country_name_ltn, -value),
        ymin = value - qnorm(1 - alpha / 2) * sqrt((value * (100 - value)) / count),
        ymax = value + qnorm(1 - alpha / 2) * sqrt((value * (100 - value)) / count),
        color = variable
      ),
      width = 0.2,
      show.legend = FALSE
    )
  
  print(plt)
  return(plt)
}





