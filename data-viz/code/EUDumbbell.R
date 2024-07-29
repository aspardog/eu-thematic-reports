genDumbbells <- function(dta) {
  
  #  Defining unique colors
  border_color        <- c(region_names$unique_border)
  names(border_color) <- c(region_names$nuts_id)
  label_color         <- c(region_names$unique_label)
  names(label_color)  <- c(region_names$nuts_id)
  
  # Wrangling data for chart
  data2plot <- dta %>%
    filter(level == "regional" & !is.na(country_name_ltn) & !is.na(value2plot)) %>%
    mutate(
      color_group = case_when(
        value2plot > 0.00 & value2plot <= 0.10 ~ "0%-10%",
        value2plot > 0.10 & value2plot <= 0.25 ~ "10%-25%",
        value2plot > 0.25 & value2plot <= 0.50 ~ "25%-50%",
        value2plot > 0.50 & value2plot <= 0.75 ~ "50%-75%",
        value2plot > 0.75 & value2plot <= 0.90 ~ "75%-90%",
        value2plot > 0.90 & value2plot <= 1.00 ~ "90%-100%"
      ),
      color_group = factor(
        color_group, 
        levels = c("0%-10%",  "10%-25%", "25%-50%", 
                   "50%-75%", "75%-90%", "90%-100%")
      )
    )
  
  data4segments <- data2plot %>%
    group_by(country_name_ltn) %>%
    summarise(
      min_y = min(value2plot, na.rm = TRUE),
      max_y = max(value2plot, na.rm = TRUE)
    )
  
  # Drawing plot
  chart <- ggplot() +
    geom_vline(
      xintercept = seq(0.5, nrow(data4segments)-0.5, 1),
      color      = "gray90",
      linetype   = "solid",
      linewidth  = 0.15
    ) +
    geom_segment(
      data = data4segments,
      aes(
        x    = country_name_ltn,
        xend = country_name_ltn,
        y    = min_y,
        yend = max_y
      ),
      color  = "gray95",
      size   = 4
    ) +
    geom_point(
      data = data2plot,
      aes(
        x      = country_name_ltn,
        y      = value2plot,
        fill   = color_group,
        colour = nuts_id,
      ),
      shape  = 21,
      stroke = 0.025,
      size   = 4,
      show.legend = c(fill = TRUE)
    ) +
    geom_rect(
      data = data2plot,
      aes(
        xmin = as.numeric(country_name_ltn) - 0.2,
        xmax = as.numeric(country_name_ltn) + 0.2,
        ymin = value2plot - 2,
        ymax = value2plot + 2
      ),
      fill = "white", colour = "black", size = 1
    ) +
    scale_fill_manual(name   = "",
                      values = cat_palette,
                      drop   = FALSE) +
    scale_colour_manual(values = border_color,
                        guide  = "none") +
    new_scale_colour() +
    geom_richtext(
      data = data2plot,
      aes(
        x = country_name_ltn,
        y = value2plot,
        colour = nuts_id,
        label  = paste0(
          "<b>", str_trim(nameSHORT), "</b><br>",
          "<i>", str_trim(country_name_ltn), "</i><br>",
          "Score: ", scales::number(value2plot, accuracy = 0.01)
        )
      ),
      vjust = "inward",
      size  = 2.5,
      hjust = "inward",
      fill  = "white"
    ) +
    scale_colour_manual(values = label_color,
                        guide  = "none") +
    scale_x_discrete(
      limits = rev(levels(factor(
        data2plot %>%
          filter(country_name_ltn != "European Union") %>%
          pull(country_name_ltn)
      )))
    ) +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, 1),
      breaks = seq(0, 1, .2),
      labels = paste0(seq(0, 1, .2))
    ) +
    coord_flip() +
    theme_minimal() +
    theme(
      axis.text.y        = element_text(family = "Lato Full", 
                                        size   = 8,
                                        hjust  = 0,
                                        color  = "#524F4C"),
      axis.text.x        = element_text(family = "Lato Full", 
                                        size   = 8,
                                        hjust  = 0.5,
                                        color  = "#524F4C"),
      axis.title         = element_blank(),
      panel.grid.major   = element_blank(),
      panel.grid.minor   = element_blank(),
      legend.position    = "top",
      legend.key         = element_rect(colour = NA)
    ) +
    guides(
      fill = guide_legend(
        nrow = 1,
        override.aes = list(colour = NA)
      )
    )

  return(chart)
}





