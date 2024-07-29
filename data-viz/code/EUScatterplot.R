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
      )
    )
  
  # Drawing ggplot
  plot <- ggplot(data2plot) + 
    geom_smooth(
      aes(
        x = value2plot_id1, 
        y = value2plot_id2
      ),
      method   = "lm", 
      se       = TRUE, 
      color    = "black",
      linetype = "dashed"
    ) + 
    geom_point(
      aes(x = value2plot_id1, 
          y = value2plot_id2, 
          colour1 = nuts_id,
          fill    = country_name_ltn),
      shape  = 21,
      size   = 2,
      stroke = .025,
      show.legend = FALSE
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
      x = legend[1],
      y = legend[2]
    ) +
    scale_colour_manual(aesthetics = "colour1", 
                        values     = border_color) + 
    scale_colour_manual(aesthetics = "colour2",
                        values     = label_color) +
    scale_y_continuous(breaks   = seq(0, 100, 20),
                       labels   = paste0(seq(0, 100, 20), "%"),
                       position = "left", 
                       expand   = c(0.1, 0.1)) +
    scale_x_continuous(breaks   = seq(0, 100, 20),
                       labels   = paste0(seq(0, 100, 20), "%"),
                       position = "bottom", 
                       expand   = c(0.1, 0.1)) +
    WJP_theme() +
    theme(axis.line       = element_line(color    = "#5e5c5a", 
                                         linetype = "solid"),
          legend.position = "none")
  
  return(plot)
}






