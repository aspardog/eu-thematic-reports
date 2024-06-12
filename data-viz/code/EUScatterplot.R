scatterPlot <- function(data2plot,
                        xvariable  = 'value2plot_id1',
                        yvariable =  'value2plot_id2',
                        category   = 'nuts_id') {
  
  ggplot(data = data2plot, aes_string(x = xvariable, y = yvariable, color = category)) + 
    geom_point(show.legend = FALSE) + # Show dots with legend
    geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
    labs(x = "Trust",
         y = "Corruption", 
         caption = "Note: Dotted line represents linear regression line") + 
    scale_y_continuous(limits = c(0,1),
                       breaks = seq(0,1, 0.2),
                       labels = paste0(seq(0,1, 0.2)),
                       position = "left", expand = c(0, 0)) +
    scale_x_continuous(limits = c(0,1),
                       breaks = seq(0,1, 0.2),
                       position = "bottom", expand = c(0, 0)) +
    WJP_theme() +
    theme(axis.line        = element_line(color    = "#5e5c5a", linetype = "solid"),
          plot.caption     = element_text(family = "Lato Full",
                                          face   = "plain",
                                          size   = 2.460219 * .pt,
                                          color  = "#524F4C", 
                                          vjust = 1, hjust = 0),
          legend.position = 'none')
}

