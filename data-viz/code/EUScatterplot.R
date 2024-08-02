scatterPlot <- function(dta, legend) {
  
  # Defining unique colors
  border_color <- region_names %>%
    pull(unique_border) %>%
    setNames(
      region_names %>% pull(nuts_id)
    )
  label_color <- region_names %>%
    pull(unique_label) %>%
    setNames(
      region_names %>% pull(nuts_id)
    )
  
  #Preparing tooltips
  data2plot <- dta %>%
    mutate(
      across(
        c(value2plot_id1,value2plot_id2),
        \(x) as.numeric(x)*100
      ),
      tooltip = glue(
        "<b>{nuts_id}</b><br>",
        "<i>{country_name_ltn}</i><br>",
        "{legend[1]}: {round(value2plot_id1, 1)}%<br>",
        "{legend[2]}: {round(value2plot_id2, 1)}%"
      ),
      color_group = cut(value2plot_id1, 
                        breaks = c(0, 10, 25, 50, 75, 90, 100),
                        labels = c("0%-10%",  "10%-25%", "25%-50%", 
                                   "50%-75%", "75%-90%", "90%-100%")),
      color_group = factor(
        color_group, 
        levels = c("0%-10%",  "10%-25%", "25%-50%", 
                   "50%-75%", "75%-90%", "90%-100%")
      )
    )
  
  # Drawing ggplot
  plot <- ggplot(data2plot) + 
    geom_smooth(
      aes(
        x = value2plot_id1, 
        y = value2plot_id2
      ),
      method    = "lm", 
      se        = TRUE, 
      color     = "gray25",
      linetype  = "dashed",
      linewidth = 0.75,
      fill      = "#e5e5e5"
    ) + 
    geom_point(
      aes(x = value2plot_id1, 
          y = value2plot_id2, 
          colour1 = nuts_id,
          fill    = country_name_ltn),
      shape  = 21,
      size   = 2,
      stroke = .025,
      show.legend = c(fill = TRUE)
    ) %>% rename_geom_aes(new_aes = c("colour" = "colour1")) +
    geom_richtext(aes(x = value2plot_id1,
                      y = value2plot_id2,
                      colour2 = nuts_id,
                      label   = tooltip,
                  ),
                  vjust = "inward",
                  size  = 3,
                  hjust = "inward",
                  fill  = "white") %>% rename_geom_aes(new_aes = c("colour" = "colour2")) +
    labs(
      x = paste(legend[1], "(%)"),
      y = paste(legend[2], "(%)")
    ) +
    scale_fill_manual(
      "",
      values = scatter_palette,
      labels = color_range %>% filter (level == "national") %>% pull(nuts_id),
      drop   = FALSE
    ) +
    scale_colour_manual(aesthetics = "colour1", 
                        values     = border_color,
                        guide      = "none") + 
    scale_colour_manual(aesthetics = "colour2",
                        values     = label_color,
                        guide      = "none") +
    scale_y_continuous(breaks   = seq(0, 100, 20),
                       labels   = paste0(seq(0, 100, 20), "%"),
                       position = "left", 
                       expand   = c(0.1, 0.1)) +
    scale_x_continuous(breaks   = seq(0, 100, 20),
                       labels   = paste0(seq(0, 100, 20), "%"),
                       position = "bottom", 
                       expand   = c(0.1, 0.1)) +
    WJP_theme() +
    theme(
      axis.line            = element_line(color    = "#5e5c5a", 
                                          linetype = "solid"),
      legend.position      = "top",
      legend.text          = element_text(family = "Lato Full",
                                          face   = "bold", 
                                          size   = 2.914598 * .pt,
                                          color  = "#222221",
                                          hjust  = 0.5,
                                          margin = margin(0,0,0,0)),
      legend.key.size      = unit(0.25, "inches"), 
      legend.justification = "left",
      legend.box.just      = "left",
      legend.location      = "plot", 
      legend.margin        = margin(2,0,0,0),
      legend.title         = element_text(family = "Lato Full",
                                          face   = "bold", 
                                          size   = 3.014598 * .pt,
                                          color  = "#222221",
                                          hjust  = 0.5)
    ) +
    guides(
      fill = guide_legend(
        nrow  = 2,
        byrow = TRUE,
        override.aes = list(
          colour = NA
        )
      )
    )
  
  return(plot)
}
