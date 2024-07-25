genLollipop <- function(dta) {
  dta <- dta %>% filter(demographic == "Total Sample") %>% ungroup()
  # generate new similar colors for the countries
  base_grey <- "#A6A6A6"
  base_black <- "#000000"
  

  existing_colors <- unique(c(region_names$unique_border, region_names$unique_label))
  unique_grey_colors <- generate_unique_colors(existing_colors, base_grey, 27)
  unique_black_colors <- generate_unique_colors(existing_colors, base_black, 27)

  
  
  # add new colors to the region names
  
  country_colors <- tibble(
    country_name_ltn = dta %>% filter(level == "national") %>% pull(country_name_ltn),
      unique_border = unique_grey_colors, unique_label = unique_black_colors,
    nameSHORT = dta %>% filter(level == "national") %>% pull(country_name_ltn),
    nuts_id = dta %>% filter(level == "national") %>% pull(nuts_id))


  
  # add to region names
  region_names_extended <- bind_rows(region_names, country_colors)
  
  # apply names of nuts_id to colors
  border_color <- setNames(region_names_extended$unique_border, region_names_extended$nuts_id)
  label_color <- setNames(region_names_extended$unique_label, region_names_extended$nuts_id)
  

  panels <- list(
    "A" = c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France"),
    "B" = c("Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania"),
    "C" = c("Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")
  )
  
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
      order = row_number(),
      nuts_id = if_else(level == "national", paste0("**", nuts_id, "**"), nuts_id)
    )
  
  chart_panels <- lapply(
    panels,
    function(group){
      
      subset_data <- data4plot %>%
        filter(country %in% group)
      
      bchart <- ggplot() +
        geom_segment(data      = subset_data,
                     aes(x     = reorder(nuts_id, desc(order)),
                         xend  = reorder(nuts_id, desc(order)),
                         y     = 0,
                         yend  = value2plot,
                         color = level),
                     linewidth = 1) +
        scale_color_manual(values = lpop_palette) + 
        new_scale_color() + 
        geom_point(data      = subset_data,
                   aes(x     = reorder(nuts_id, desc(order)),
                       y     = value2plot,
                       color = nuts_id,
                       fill   = level
                   ),
                   shape  = 21,
                   stroke = .025, 
                   size   = 4) +
        scale_color_manual(values = border_color) + 
        scale_fill_manual(values = lpop_palette) + 
        new_scale_color() + 
        geom_richtext(data = subset_data,
                      aes(x = reorder(nuts_id, desc(order)),
                          y = value2plot - 0.116,
                          colour = nuts_id,
                          label = paste0(
                            "<b>", nameSHORT,"</b>,<br>",
                            "<i>", country, "</i><br>",
                            "Percentage: ", scales::percent(value2plot, accuracy = 0.1)
                          ),
                      ),
                      vjust = "inward",
                      size = 1.9,
                      hjust = "inward",
                      fill = "white"
        ) +
        scale_color_manual(values = label_color) + 
        scale_y_continuous(position = "right",
                           breaks   = seq(0, 1, 0.25),
                           labels   = paste0(seq(0, 1, 0.25) * 100, "%"),
                           limits   = c(-0.01, 1.03),
                           expand   = expansion(mult = 0)) +
        coord_flip() +
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
  
  patch <- chart_panels[["A"]] | chart_panels[["B"]] | chart_panels[["C"]]
  print(patch)
  return(patch)
}




  