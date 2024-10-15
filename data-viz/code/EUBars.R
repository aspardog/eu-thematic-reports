gen_bars <- function(data, direction, static = FALSE) {
  
  data <- data %>% filter(demographic == "Total Sample")
  
  if (direction %in% c("positive", "neutral")){
    cpal  <- cat_palette
  } else {
    cpal  <- cat_palette_inverted
  }
  
  # Defining unique color codes
  border_color <- color_range %>%
    filter(level == "national") %>%
    pull(unique_border) %>%
    setNames(
      color_range %>% filter(level == "national") %>% pull(nameSHORT)
    )
  
  label_color <- color_range %>%
    filter(level == "national") %>%
    pull(unique_label) %>%
    setNames(
      color_range %>% filter(level == "national") %>% pull(nameSHORT)
    )
  
  # Wrangling data for chart
  data <- data %>%
    filter(level == "national") %>%
    mutate(
      color_group = cut(value2plot, 
                        breaks = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1),
                        labels = c("0%-10%",  "10%-25%", "25%-50%", 
                                   "50%-75%", "75%-90%", "90%-100%")),
      color_group = factor(
        color_group, 
        levels = c("0%-10%",  "10%-25%", "25%-50%", 
                   "50%-75%", "75%-90%", "90%-100%")
      )
    ) %>%
    arrange(value2plot) %>% ungroup()
  
  #  Drawing ggplot
  plt <- ggplot(data, aes(x = reorder(country_name_ltn, value2plot), 
                          y = value2plot, 
                          fill = color_group, 
                          colour = country_name_ltn)) +
    geom_bar(stat        = "identity", 
             width       = 0.75, 
             linewidth   = 0.025,
             show.legend = c(fill = TRUE)) +
    labs(y = "% of respondents", x = "Country") +
    scale_color_manual(values = border_color,
                       guide  = "none") + 
    scale_fill_manual(name   = "",
                      values = cpal,
                      drop   = FALSE) +
    new_scale_color() 
    if (static == FALSE){
    plt <- plt + geom_richtext(
      aes(x = reorder(country_name_ltn, value2plot),
          y = value2plot,
          colour = country_name_ltn,
          label = paste0(
            "<b>", country_name_ltn, ": ",
            scales::percent(value2plot, accuracy = 0.1)
          )),
      vjust = "inward", 
      size  = 3,
      hjust = "inward", 
      fill  = "white"
    ) +
    scale_color_manual(values = label_color,
                       guide  = "none") }
    plt <- plt + scale_y_continuous(limits = c(0, 1.05), 
                       labels = scales::percent_format(accuracy = 1),
                       expand = c(0,0),
                       position = "right") + 
    coord_flip() +
    theme(
      axis.text.x      = element_text(family = "Lato Full", 
                                      face   = "plain", 
                                      size   = 3.514598 * .pt, 
                                      color  = "#524F4C", 
                                      angle  = 0, 
                                      hjust  = 0.5),
      axis.text.y      = element_text(family = "Lato Full", 
                                      face   = "bold", 
                                      size   = 3.514598 * .pt, 
                                      color  = "#524F4C",
                                      hjust  = 0),
      axis.ticks       = element_blank(),
      axis.title       = element_blank(),
      legend.position  = "top",
      legend.text      = element_text(family = "Lato Full",
                                      face   = "bold", 
                                      size   = 2.914598 * .pt,
                                      color  = "#222221",
                                      hjust  = 0.5),
      legend.key.size      = unit(0.15, "inches"), 
      legend.justification = "left",
      legend.location      = "plot", 
      legend.margin        = margin(2,0,0,0),
      panel.grid.major.x   = element_line(size     = 0.25,
                                          colour   = "#a0a0a0",
                                          linetype = "dashed"),  
      panel.grid.minor     = element_blank(),  
      panel.background     = element_blank(),  
      plot.background      = element_blank()
    ) +
    guides(fill = guide_legend(nrow = 1))
  
  return(plt)
}








