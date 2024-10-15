gen_dots <- function(data, legend, static = FALSE) {

  country_avg <- data %>%
    left_join(region_names %>% select(nuts_id, pop_weight), 
              by = "nuts_id") %>%
    mutate(
      weighted_value_id1 = value2plot_id1 * pop_weight,
      weighted_value_id2 = value2plot_id2 * pop_weight,
    ) %>%
    group_by(country_name_ltn) %>%
    summarise(
      nuts_id        = first(nuts_id),
      value2plot_id1 = sum(weighted_value_id1*100, na.rm = TRUE),
      value2plot_id2 = sum(weighted_value_id2*100, na.rm = TRUE),
      count          = sum(count),
      .groups        = "keep"
    ) %>%
    mutate(
      nuts_id   = substr(nuts_id, 1, 2),
      label_pos = (value2plot_id1+value2plot_id2)/2
    ) 
  
    hover_data <- country_avg
  
    country_avg <- country_avg %>% pivot_longer(
      !c(country_name_ltn, nuts_id, count, label_pos),
      values_to = "value2plot",
      names_to  = "category"
    ) %>%
    mutate(
      category = case_when(
        category == "value2plot_id1" ~ "Value 1",
        category == "value2plot_id2" ~ "Value 2"
      )
    )
  
  # Apply names of nuts_id to colors
  border_color <- color_range %>% 
    filter(level == "national") %>% 
    pull(unique_border) %>% 
    setNames(country_avg %>% filter(category == "Value 1") %>% pull(nuts_id))
  label_color <- color_range %>% 
    filter(level == "national") %>% 
    pull(unique_label) %>% 
    setNames(country_avg %>% filter(category == "Value 1") %>% pull(nuts_id))
  
  # Filter data for richtext
  value1_data <- country_avg %>% filter(category == "Value 1")
  value2_data <- country_avg %>% filter(category == "Value 2")
  
  # Generate the plot
  plt <- ggplot(country_avg %>% filter(country_name_ltn != "Ireland")) +
    geom_point(
      aes(x = country_name_ltn, 
          y = value2plot, 
          colour1 = nuts_id, 
          fill    = category), 
      shape  = 21, 
      size   = 3,
      stroke = .025, 
      show.legend = c(fill = TRUE)
    ) %>% rename_geom_aes(new_aes = c("colour" = "colour1")) +
    geom_errorbar(
      aes(
        x    = country_name_ltn,
        ymin = value2plot - qnorm(1 - .05 / 2) * sqrt((value2plot * (100 - value2plot)) / count),
        ymax = value2plot + qnorm(1 - .05 / 2) * sqrt((value2plot * (100 - value2plot)) / count),
        colour2 = category
      ),
      width = 0.2,
      show.legend = FALSE
    ) %>% rename_geom_aes(new_aes = c("colour" = "colour2")) + 
    geom_vline(
      xintercept = seq(0.5, 26.5, 1),
      color      = "#a0a0a0",
      linetype   = "solid",
      linewidth  = 0.15
    ) 
    if (static == FALSE){
    plt <- plt + geom_richtext(
      data = (hover_data %>% filter(country_name_ltn != "Ireland")),
      aes(x = country_name_ltn, 
          y = label_pos, 
          label = paste0(
            "<b><i>", country_name_ltn,"</i></b><br>",
            "<span style='color:#49178e;'>",
            "<b><i>", legend[1], ": ",
            scales::percent(value2plot_id1 / 100, accuracy = 0.1), 
            "</i></b></span><br>",
            "<span style='color:#dd58b1;'>", 
            "<b><i>", legend[2], ": ",
            scales::percent(value2plot_id2 / 100, accuracy = 0.1), 
            "</i></b></span>"
          ),
          colour3 = nuts_id), 
      size = 3, 
      hjust = "inward", 
      vjust = "inward", 
      fill = "white",
      show.legend = FALSE
    ) %>% rename_geom_aes(new_aes = c("colour" = "colour3"))}
  
    plt <- plt + scale_fill_manual("",
                      values = dots_palette,
                      labels = legend) + 
    scale_colour_manual(aesthetics = "colour1",
                        values     = border_color,
                        guide      = "none") + 
    scale_colour_manual(aesthetics = "colour2",
                        values     = dots_palette,
                        guide      = "none") + 
    scale_colour_manual(aesthetics = "colour3",
                        values     = label_color,
                        guide      = "none") + 
    scale_y_continuous(
      limits   = c(0, 105), 
      breaks   = seq(0, 100, 20), 
      labels   = paste0(seq(0, 100, 20), "%"), 
      position = "right"
    ) +
    scale_x_discrete(
      limits = rev(levels(factor(
        country_avg %>% filter(country_name_ltn != "Ireland") %>%
          pull(country_name_ltn)
      )))
    ) + 
    coord_flip() +
    WJP_theme() +
    theme(
      axis.title.y     = element_blank(),
      axis.title.x     = element_blank(),
      axis.text.x      = element_text(family = "Lato Full",
                                      face   = "bold",
                                      size   = 4 * .pt,
                                      color  = "#524F4C",
                                      margin = margin(10, 0, 0, 0),
                                      hjust  = 0.5),
      axis.text.y      = element_text(family = "Lato Full",
                                      face   = "bold",
                                      size   = 4 * .pt,
                                      color  = "#524F4C",
                                      margin = margin(10, 0, 0, 0),
                                      hjust = 0),
      axis.line.x      = element_line(linewidth = 0.25,
                                      colour    = "#a0a0a0",
                                      linetype  = "solid"),
      axis.ticks.y       = element_blank(),
      panel.grid         = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(size     = 0.25,
                                        colour   = "#a0a0a0",
                                        linetype = "dashed"),
      panel.background   = element_blank(),
      legend.position  = "top",
      legend.text      = element_text(family = "Lato Full",
                                      face   = "bold", 
                                      size   = 3.514598 * .pt,
                                      color  = "#222221",
                                      hjust  = 0.5),
      legend.key.size      = unit(0.25, "inches"), 
      legend.justification = "left",
      legend.location      = "plot", 
      legend.margin        = margin(2,0,0,0),
    )+
    guides(
      fill = guide_legend(
        override.aes = list(
          colour = NA
        )
      )
    )
  return(plt)
}






