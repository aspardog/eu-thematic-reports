
gen_dots <- function(data, region_names) {
  
  # apply population weights and get national level data
  data_wght <- data %>%
    left_join(region_names, by = "nuts_id") %>%
    mutate(
      weighted_value_id1 = value2plot_id1 * pop_weight,
      weighted_value_id2 = value2plot_id2 * pop_weight,
      level = "regional"
    )
  
  # new colors for countries
  base_grey <- "#A6A6A6"
  base_black <- "#000000"
  
  existing_colors <- unique(c(region_names$unique_border, region_names$unique_label))
  unique_grey_colors <- generate_unique_colors(existing_colors, base_grey, 27)
  unique_black_colors <- generate_unique_colors(existing_colors, base_black, 27)
  
  country_avg <- data_wght %>%
    group_by(country_name_ltn, chart_id, demographic) %>%
    summarise(
      nuts_id = first(nuts_id),
      value2plot_id1 = sum(weighted_value_id1*100, na.rm = TRUE),
      value2plot_id2 = sum(weighted_value_id2*100, na.rm = TRUE),
      count = sum(count),
      .groups = "keep"
    ) %>%
    mutate(
      nuts_id = substr(nuts_id, 1, 2),
      nameSHORT = country_name_ltn,
      level = "national"
    )
  
  # Make a tibble with the country nuts_id and border and label colors
  country_colors <- tibble(
    nuts_id = unique(country_avg$nuts_id),
    unique_border = unique_grey_colors,
    unique_label = unique_black_colors
  )
  
  # Left join with country avg
  country_avg <- left_join(country_avg, country_colors, by = "nuts_id")
  
  # Apply names of nuts_id to colors
  border_color <- setNames(country_avg$unique_border, country_avg$nuts_id)
  label_color <- setNames(country_avg$unique_label, country_avg$nuts_id)
  
  # Generate the plot
  plt <- ggplot(country_avg) +
    geom_point(aes(x = reorder(country_name_ltn, -value2plot_id1), y = value2plot_id1, color = nuts_id, fill = "Value 1"), shape = 21, size = 3,stroke = .025, show.legend = FALSE) +
    geom_point(aes(x = reorder(country_name_ltn, -value2plot_id2), y = value2plot_id2, color = nuts_id, fill = "Value 2"), shape = 21, size = 3, stroke = .025,show.legend = FALSE) +
    # fill = value 1 or value 2
    scale_fill_manual(values = dots_palette) + 
    # geom point outline = border color
    scale_color_manual(values = border_color) + 
    new_scale_color() + 
    geom_errorbar(
      aes(
        x = reorder(country_name_ltn, -value2plot_id1),
        ymin = value2plot_id1 - qnorm(1 - .05 / 2) * sqrt((value2plot_id1 * (100 - value2plot_id1)) / count),
        ymax = value2plot_id1 + qnorm(1 - .05 / 2) * sqrt((value2plot_id1 * (100 - value2plot_id1)) / count),
        color = "Value 1"
      ),
      width = 0.2,
      show.legend = FALSE) + 
    geom_errorbar(
      aes(
        x = reorder(country_name_ltn, -value2plot_id2),
        ymin = value2plot_id2 - qnorm(1 - .05 / 2) * sqrt((value2plot_id2 * (100 - value2plot_id2)) / count),
        ymax = value2plot_id2 + qnorm(1 - .05 / 2) * sqrt((value2plot_id2 * (100 - value2plot_id2)) / count),
        color = "Value 2"
      ),
      width = 0.2,
      show.legend = FALSE) + 
    scale_color_manual(values = dots_palette) +
    new_scale_color() + 
    geom_richtext(
      aes(x = reorder(country_name_ltn, -value2plot_id1), 
          y = (value2plot_id1 + value2plot_id2)/2, 
          label = paste0(
            "<b><i>", country_name_ltn,"</i></b><br>",
            "<span style='color:#49178e;'>",scales::percent(value2plot_id1 / 100, accuracy = 0.1), "</span><br>",
            "<span style='color:#dd58b1;'>", scales::percent(value2plot_id2 / 100, accuracy = 0.1), "</span>"
          ),
          color = nuts_id), 
      size = 3, 
      hjust = "inward", 
      vjust = "inward", 
      fill = "white") +
    scale_color_manual(values = label_color) + 
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), labels = paste0(seq(0, 100, 20), "%"), position = "right") +
    scale_x_discrete(
      limits = rev(levels(factor(
        country_avg %>%
          pull(country_name_ltn)
      )))
    ) + 
    coord_flip() +
    WJP_theme() +
    theme(
      axis.title.y     = element_blank(),
      axis.title.x     = element_blank(),
      axis.text.x      = element_text(family = "Lato Full",
                                      face   = "plain",
                                      size   = 3.063138 * .pt,
                                      color  = "#524F4C",
                                      margin = margin(10, 0, 0, 0)),
      axis.text.y       = element_text(family = "Lato Full",
                                           face   = "bold",
                                           size   = 4 * .pt,
                                           color  = "#524F4C",
                                           margin = margin(10, 0, 0, 0),
                                           hjust = 0),
      axis.line.x      = element_line(linewidth = 0.25,
                                      colour    = "#5e5c5a",
                                      linetype  = "solid"),
      axis.ticks.y     = element_blank(),
      panel.grid       = element_blank(),
      # panel.grid.major.x = element_line(linewidth = 0.25,
      #                                   colour = "#5e5c5a",
      #                                   linetype = "dashed"),
      panel.grid.major.x = element_blank(),
      panel.background = element_blank(),
      legend.position  = "none"
    )
  
  
  print(plt)
  return(plt)
}






