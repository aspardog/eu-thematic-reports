gen_bars <- function(data) {
  
  data <- data %>% filter(demographic == "Total Sample")
  
  # left join with data to get country name
  region_names <- region_names %>%
    left_join(
      data %>% 
        select(country_name_ltn, nuts_id, demographic) %>% 
        distinct(), 
      by = "nuts_id", 
      relationship = "many-to-many"
    ) %>%
    distinct() %>%
    ungroup()
  
  # take the first color for that country
  country_colors <- region_names %>%
    group_by(country_name_ltn) %>%
    summarise(
      unique_border = first(unique_border),
      unique_label  = first(unique_label),
      .groups = "drop"
    ) %>% ungroup()
  
  # set of border colors
  border_color <- country_colors %>%
    pull(unique_border) %>%
    setNames(country_colors$country_name_ltn)
  
  # set of label colors
  label_color <- country_colors %>%
    pull(unique_label) %>%
    setNames(country_colors$country_name_ltn)
  
  data <- data %>%
    filter(level == "national") %>%
    mutate(
      color_group = cut(value2plot, 
                        breaks = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1),
                        labels = c("0%-10%", "10%-25%", "25%-50%", 
                                   "50%-75%", "75%-90%", "90%-100%")),
      color_group = factor(color_group, levels = c("0%-10%", "10%-25%", "25%-50%", 
                                                   "50%-75%", "75%-90%", "90%-100%"))
    ) %>%
    arrange(value2plot) %>% ungroup()
  
  plt <- ggplot(data, aes(x = reorder(country_name_ltn, value2plot), 
                          y = value2plot, 
                          fill = color_group, colour = country_name_ltn)) +
    geom_bar(stat = "identity", width = 0.9, linewidth = 0.025) +
    labs(y = "% of respondents", x = "Country") +
    # geom_rect(aes(xmin = as.numeric(reorder(country_name_ltn, value2plot)) - 0.5,
    #               xmax = as.numeric(reorder(country_name_ltn, value2plot)) + 0.5,
    #               ymin = 0,
    #               ymax = value2plot),
    #           fill = NA, size = .025, linewidth = .025) +
    scale_color_manual(values = border_color) + 
    scale_fill_manual(values = cat_palette) +
    new_scale_color() + 
    geom_richtext(
      aes(x = reorder(country_name_ltn, value2plot),
          y = value2plot,
          colour = country_name_ltn,
          label = paste0(
            "<b>", country_name_ltn, ", ",
            scales::percent(value2plot, accuracy = 0.1)
          )),
      vjust = "inward", 
      size = 2.5,
      hjust = "inward", 
      fill = "white"
    ) +
    scale_color_manual(values = label_color) + 
    scale_y_continuous(limits = c(0, 1), 
                       labels = scales::percent_format(accuracy = 1),
                       expand = c(0,0)) +
    coord_flip() +
    theme(axis.text.x = element_text(family = "Lato Full", 
                                     face = "plain", 
                                     size = 3.514598 * .pt, 
                                     color = "#524F4C", 
                                     angle = 45, 
                                     hjust = 1),
          axis.text.y = element_text(family = "Lato Full", 
                                     face = "bold", 
                                     size = 3.514598 * .pt, 
                                     color = "#524F4C",
                                     hjust = 0),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.position = "none",
          panel.grid.major = element_blank(),  
          panel.grid.minor = element_blank(),  
          panel.background = element_blank(),  
          plot.background = element_blank())
  
  print(plt)
  
  return(plt)
}








