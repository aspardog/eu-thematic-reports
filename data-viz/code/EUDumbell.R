genDumbbells <- function(dta){
  
  data2plot <- dta %>%
    filter(level == "regional") %>%
    mutate(
      color_group = case_when(
        value2plot >  0.00 & value2plot <= 0.10 ~ "0%-10%",
        value2plot >  0.10 & value2plot <= 0.25 ~ "10%-25%",
        value2plot >  0.25 & value2plot <= 0.50 ~ "25%-50%",
        value2plot >  0.50 & value2plot <= 0.75 ~ "50%-75%",
        value2plot >  0.75 & value2plot <= 0.90 ~ "75%-90%",
        value2plot >  0.90 & value2plot <= 1.00 ~ "90%-100%"
      ),
      color_group = as.factor(color_group),
      value2plot  = value2plot*100
    )
  
  data4segments <- data2plot %>%
    group_by(country_name_ltn) %>%
    summarise(
      min_y = min(value2plot, na.rm = T),
      max_y = max(value2plot, na.rm = T)
    )
  
  chart <- ggplot() +
    geom_segment(
      data = data4segments,
      aes(
        x       = country_name_ltn,
        xend    = country_name_ltn,
        y       = min_y,
        yend    = max_y
      ),
      color     = "gray95",
      linewidth = 4
    ) +
    geom_point(
      data = data2plot,
      aes(
        x     = country_name_ltn,
        y     = value2plot,
        color = color_group
      ),
      size    = 4
    ) +
    scale_color_manual(
      values = cat_palette
    ) +
    scale_x_discrete(
      limits = rev(
        levels(
          factor(
            data4chart %>% 
              filter(country_name_ltn != "European Union") %>%
              pull(country_name_ltn)
          )
        )
      )
    ) +
    scale_y_continuous(
      expand = c(0,0),
      limits = c(0,105),
      breaks = seq(0,100,20),
      labels = paste0(
        seq(0,100,20), "%"
      )
    ) +
    coord_flip() +
    theme(panel.background   = element_blank(),
          plot.background    = element_blank(),
          panel.grid.major   = element_blank(), 
          panel.grid.minor   = element_blank(),
          axis.title.y       = element_blank(), 
          axis.title.x       = element_blank(), 
          axis.text.y        = element_text(family   = "Lato Full",
                                            face     = "plain",
                                            size     = 3.514598*.pt,
                                            color    = "#524F4C"),
          axis.text.x        = element_text(family = "Lato Full",
                                            face   = "plain",
                                            size   = 3.514598*.pt,
                                            color  = "#524F4C"),
          axis.ticks         = element_blank(),
          plot.margin        = unit(c(0, 0, 0, 0), "points"),
          legend.position    = "none" 
    )
  
  return(chart)
  
}