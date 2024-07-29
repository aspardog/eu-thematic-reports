genLollipop <- function(dta) {
  
  dta <- dta %>% filter(demographic == "Total Sample") %>% ungroup()
  
  # Defining unique color codes
  border_color <- color_range %>% pull(unique_border) %>% setNames(color_range$nuts_id)
  label_color  <- color_range %>% pull(unique_label) %>% setNames(color_range$nuts_id)
  
  # Splitting panels
  panels <- list(
    "A" = c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France"),
    "B" = c("Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania"),
    "C" = c("Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")
  )
  
  # Wrangling data
  region_data <- dta %>%
    filter(level == "regional") %>%
    ungroup() %>%
    select(country = country_name_ltn, nameSHORT, nuts_id, level, value2plot)
  
  country_data <- dta %>%
    filter(level == "national") %>%
    select(country = country_name_ltn, nameSHORT, nuts_id, level, value2plot) %>%
    group_by(country) 

  
  data4plot <- region_data %>%
    bind_rows(country_data) %>%
    mutate(
      level = factor(level, levels = c("national", "regional"))
    ) %>%
    arrange(country, level) %>%
    distinct(country, nameSHORT, .keep_all = TRUE) %>%
    mutate(
      order    = row_number(),
      axis_lab = if_else(level == "national", paste0("**", nuts_id, "**"), nuts_id)
    )
  
  # Drawing individual panels
  chart_panels <- lapply(
    panels,
    function(group){
      
      subset_data <- data4plot %>%
        filter(country %in% group)
      
      bchart <- ggplot() +
        geom_segment(data = subset_data,
                     aes(x       = reorder(axis_lab, desc(order)),
                         xend    = reorder(axis_lab, desc(order)),
                         y       = 0,
                         yend    = value2plot,
                         colour1 = level),
                     linewidth   = 1) %>% rename_geom_aes(new_aes = c("colour" = "colour1")) +
        geom_point(data         = subset_data,
                   aes(x        = reorder(axis_lab, desc(order)),
                       y        = value2plot,
                       colour2  = nuts_id,
                       fill     = level
                   ),
                   shape  = 21,
                   stroke = .025, 
                   size   = 4) %>% rename_geom_aes(new_aes = c("colour" = "colour2")) +
        geom_richtext(data  = subset_data,
                      aes(x = reorder(axis_lab, desc(order)),
                          y = value2plot,
                          colour3 = nuts_id,
                          label   = paste0(
                            "<b>",nameSHORT,"</b>,<br>",
                            "<i>",country, "</i><br>",
                            scales::percent(value2plot, accuracy = 0.1)
                          ),
                      ),
                      vjust = "inward",
                      size  = 2.25,
                      hjust = "inward",
                      fill  = "white") %>% rename_geom_aes(new_aes = c("colour" = "colour3")) +
        scale_colour_manual(aesthetics = "colour1", 
                            values = lpop_palette) + 
        scale_colour_manual(aesthetics = "colour2",
                            values = border_color) + 
        scale_colour_manual(aesthetics = "colour3",
                            values   = label_color) + 
        scale_fill_manual(values  = lpop_palette) +
        scale_y_continuous(position = "right",
                           breaks   = seq(0, 1, 0.25),
                           labels   = paste0(seq(0, 1, 0.25) * 100, "%"),
                           limits   = c(-0.01, 1.03),
                           expand   = expansion(mult = 0)) +
        coord_flip(clip = "off") +
        theme(
          axis.title.y     = element_blank(),
          axis.title.x     = element_blank(),
          axis.text.x      = element_text(family = "Lato Full",
                                          face   = "plain",
                                          size   = 3.063138 * .pt,
                                          color  = "#524F4C",
                                          margin = margin(10, 0, 0, 0)),
          axis.text.y       = element_markdown(family = "Lato Full",
                                               face   = "plain",
                                               size   = 3.063138 * .pt,
                                               color  = "#524F4C",
                                               margin = margin(7, 0, 0, 0),
                                               hjust = 0),
          axis.line.x      = element_line(linewidth = 0.25,
                                          colour    = "#5e5c5a",
                                          linetype  = "solid"),
          axis.ticks.y     = element_blank(),
          panel.grid       = element_blank(),
          panel.grid.major.x = element_line(linewidth = 0.25,
                                            colour = "#5e5c5a",
                                            linetype = "dashed"),
          panel.background = element_blank(),
          legend.position  = "none"
        )
      
      return(bchart)
    }
  )
  
  # Assembling patchwork
  patch <- plot_grid(chart_panels[["A"]] , chart_panels[["B"]] , chart_panels[["C"]], nrow = 1)
  
  return(patch)
}




  