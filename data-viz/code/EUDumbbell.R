genDumbbells <- function(dta) {
  
  #  defining unique colors
  border_color        <- c(region_names$unique_border)
  names(border_color) <- c(region_names$nuts_id)
  label_color         <- c(region_names$unique_label)
  names(label_color)  <- c(region_names$nuts_id)
  
  # wrangling data for chart
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
  
  national_averages <- dta %>% 
    filter(level == "national" & !is.na(country_name_ltn) & !is.na(value2plot)) %>%
    select(country_name_ltn, country_av = value2plot)

  
  # index country names for y axis
  country_levels <- unique(data4segments$country_name_ltn)
  country_indices <- setNames(seq_along(country_levels), country_levels)
  
  # Drawing plot
  chart <- ggplot() +
    # Manually draw gridlines
    geom_segment(
      data = data4segments,
      aes(
        x = country_indices[country_name_ltn] - 0.5,
        xend = country_indices[country_name_ltn] - 0.5,
        y = 0,
        yend = 1
      ),
      color = "#a0a0a0",
      linetype = "solid",
      size = 0.25
    ) +
    annotate(
      "segment",
      x     = max(country_indices) + 0.5, 
      xend  = max(country_indices) + 0.5,   
      y     = 0,                            
      yend  = 1,                          
      color = "#a0a0a0",
      size  = 0.3
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
    geom_text(
      data = national_averages,
      aes(
        x      = country_name_ltn, 
        y      = -.1,
        label  = scales::number(country_av, accuracy = 0.01)
      ),
      hjust = -0.1,  
      vjust = 0.25,  
      color = "black",
      size  = 3,
      family = "Lato Full"
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
                      labels = c("0.00 - 0.10", "0.10 - 0.25", "0.25 - 0.50", "0.50 - 0.75", "0.75 - 0.90", "0.90 - 1.00"),
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
          scales::number(value2plot, accuracy = 0.01)
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
      expand   = c(0, 0),
      limits   = c(-.1, 1),
      breaks   = seq(0, 1, .2),
      # labels   = round(seq(0, 1, .2), 2),
      labels = scales::number_format(accuracy = 0.01),
      position = "right" 
    ) +
    annotation_custom(
      grob = textGrob("*Country \nAvg", 
                      x = unit(.0125, "npc"),
                      y = unit(1.035, "npc"), 
                      just = c("left", "top"), 
                      gp = gpar(col =  "#524F4C", fontsize = 6, family = "Lato Full",
                                face = "bold"))
    ) +
    
    coord_flip(clip = "off") +
    theme_minimal() +
    theme(
      axis.text.y        = element_text(family = "Lato Full", 
                                        size   = 8,
                                        hjust  = 0,
                                        color  = "#524F4C",
                                        margin = margin(r = 20)),
      axis.text.x        = element_text(family = "Lato Full", 
                                        size   = 8,
                                        hjust  = 0.5,
                                        color  = "#524F4C"),
      
      axis.line.x        = element_blank(),
      axis.title         = element_blank(),
      panel.grid.major   = element_blank(),
      panel.grid.minor   = element_blank(),
      legend.position    = "top",
      legend.text        = element_text(family = "Lato Full",
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
                                          linetype = "dashed")
    ) +
    guides(
      fill = guide_legend(
        nrow = 1,
        override.aes = list(colour = NA)
      )
    )

  return(chart)
}
