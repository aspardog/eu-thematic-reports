gen_catMap <- function(dta) {
  
  # Define the colors for categories
  unique_categories <- c(sort(setdiff(unique(dta$value2plot), "Other")), "Other")
  category_colors <- cat_map_palette[1:length(unique_categories)] %>%
    setNames(unique_categories)
  
  # Defining unique colors for map regions
  inner_regions <- dta %>%
    ungroup() %>%
    distinct(nuts_id) %>%
    pull(nuts_id)
  outer_regions <- base_map %>%
    filter(!polID %in% inner_regions) %>%
    pull(polID)
  border_color        <- c(region_names$unique_border,
                           rep("#ABA1A7", length((outer_regions))))
  names(border_color) <- c(region_names$nuts_id,
                           outer_regions)
  label_color         <- c(region_names$unique_label,
                           rep("#212429", length((outer_regions))))
  names(label_color)  <- c(region_names$nuts_id,
                           outer_regions)
  
  # Merge data with base map
  data4map <- base_map %>%
    left_join(dta, 
              by = c("polID" = "nuts_id")) %>%
    left_join(region_names %>% select(nuts_id, nameSHORT),
              by = c("polID" = "nuts_id"))
  
  inset_dimensions <- list(
    # y range longer than x
    "Canarias/Madeira" = list(
      "x" = c(1491672, 2091880.2),
      "y" = c(941748.3, 1541956.5)
    ),
    # the y range is longer than the x
    "Açores"  = list(
      "x" = c(864542.6, 1407344),
      "y" = c(2250319.3, 2793120.7)
    ),
    # x range is longer than y
    "Cyprus" = list(
      "x" = c(6332001, 6525814),
      "y" = c(1585147, 1767640)
    )
  )
  
  # Convert insets to grob objects using ggplotGrob
  inset_grobs <- imap(insets, function(inset, inset_name) {
    
    region_data <- data4map %>%
      filter(polID %in% inset$polID)
    
    centroids <- region_data %>%
      filter(polID %in% inset$polID) %>%
      st_centroid() %>%
      mutate(
        lon = st_coordinates(.)[,1],
        lat = st_coordinates(.)[,2],
        lat = if_else(polID == "MT00", lat + 5000, lat),
        tooltip = paste0(
          "**",str_trim(nameSHORT),"**<br>",
          "_", str_trim(country_name_ltn),"_<br>",
          value2plot
        ),
        hjust = 0,
        vjust = 1
      ) 
    
    # ggplot object for insets
    gg_inset <- ggplot() +
      geom_sf(
        data = region_data,
        aes(
          fill    = value2plot,
          colour  = polID
        ),
        size = 0.5,
        show.legend = c(fill = TRUE)
      ) +
      scale_colour_manual(values     = border_color,
                          guide      = "none") +
      scale_fill_manual(values       = category_colors, 
                        na.value     = "#d8d8d8") +
      new_scale_colour() +
      geom_textbox(
        data = centroids,
        aes(
          y      = lat,
          x      = lon,
          label  = tooltip,
          colour = polID,
          hjust  = hjust,
          vjust  = vjust
        ),
        family   = "Lato Full",
        fontface = "plain",
        width    = unit(1.25, "inch"),
        size     = 3,
        fill     = "white"
      ) +
      scale_colour_manual(values     = label_color,
                          guide      = "none") +
      scale_y_continuous(limits  = inset_dimensions[[inset_name]][["y"]]) + 
      scale_x_continuous(limits  = inset_dimensions[[inset_name]][["x"]]) +
      coord_sf(clip = "off") +
      theme_minimal() +
      theme(
        axis.title       = element_blank(),
        axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        panel.grid       = element_blank(),
        panel.background = element_blank(),
        legend.title     = element_blank(),
        legend.position  = "none",
        panel.border     = element_rect(color = "#e6e7e8", 
                                        fill  = NA, 
                                        linewidth = 0.5)
      ) +
      labs(fill = "Category") +
      ggtitle(inset_name) +
      theme(plot.title = element_text(size = 8, hjust = 0.5))
    
    # Convert to grob
    ggplotGrob(gg_inset)
  })
  
  country_level <- data4map %>%
    group_by(CNTR_CODE) %>%
    summarise()
  
  # Main map
  centroids <- data4map %>%
    filter(polID %in% inner_regions) %>%
    st_centroid() %>%
    mutate(
      lon = st_coordinates(.)[,1],
      lat = st_coordinates(.)[,2],
      lat = if_else(polID == "MT00", lat + 5000, lat),
      tooltip = paste0(
        "**",str_trim(nameSHORT),"**<br>",
        "_", str_trim(country_name_ltn),"_<br>",
        value2plot
      ),
      hjust = case_when(
        lon <  4299365 ~ 0,
        lon >= 4299365 ~ 1,
      ),
      vjust = case_when(
        lat <  3383059 ~ 0,
        lat >= 3383059 ~ 1
      )
    ) 
  
  p <- ggplot() +
    geom_sf(
      data = data4map,
      aes(
        fill   = value2plot,
        colour = polID
      ),
      size = 0.5,
      show.legend = c(fill = TRUE)
    ) +
    geom_sf(data  = country_level,
            fill  = NA,
            color = "grey25") +
    scale_colour_manual(values     = border_color,
                        guide      = "none") +
    scale_fill_manual("",
                      values       = category_colors, 
                      na.value     = "#d8d8d8",
                      drop         = F,
                      na.translate = F) +
    new_scale_colour() +
    geom_textbox(
      data = centroids,
      aes(
        y      = lat,
        x      = lon,
        label  = tooltip,
        colour = polID,
        hjust  = hjust,
        vjust  = vjust
      ),
      family   = "Lato Full",
      fontface = "plain",
      width    = unit(1.25, "inch"),
      size     = 3,
      fill     = "white"
    ) +
    # coord_sf(crs = st_crs(base_map)) +
    scale_colour_manual(values = label_color,
                        guide  = "none") +
    scale_y_continuous(limits  = c(1442631, 5323487)) +
    scale_x_continuous(limits  = c(2581570, 6017160)) +
    theme_minimal() +
    theme(
      axis.title.x     = element_blank(),
      axis.title.y     = element_blank(),
      axis.text        = element_blank(),
      axis.ticks       = element_blank(),
      panel.grid       = element_blank(),
      panel.border     = element_rect(color     = "#e6e7e8", 
                                      fill      = NA, 
                                      linewidth = 0.55),
      panel.background   = element_blank(),
      plot.margin        = margin(0, 0, 0, 0),
      legend.text        = element_text(family = "Lato Full",
                                        face   = "bold", 
                                        size   = 3 * .pt,
                                        color  = "#222221",
                                        hjust  = 0.5,
                                        margin = margin(0,5,0,12)),
      legend.position      = "top",
      legend.byrow         = TRUE, 
      legend.key.size      = unit(0.15, "inches"), 
      # legend.key.spacing   = unit(0.25, "inches"),
      legend.justification = "left",
      legend.margin        = margin(2,0,0,0),
      
    ) + 
    guides(
      fill = guide_legend(
        nrow  = 1,
        byrow = TRUE,
        override.aes = list(
          colour = NA
        )
      )
    )
  
  # Convert the main plot to a grob
  p_grob <- ggplotGrob(p)
  
  # Create a new gTree to hold the main plot and insets
  combined_grob <- gTree(children = gList(p_grob))
  
  # Add insets to the main map
  viewport_positions <- list(
    viewport(x = unit(0.23, "npc"), y = unit(0.82, "npc"), width = unit(0.15, "npc"), height = unit(0.15, "npc")),
    viewport(x = unit(0.34, "npc"), y = unit(0.82, "npc"), width = unit(0.15, "npc"), height = unit(0.15, "npc")),
    viewport(x = unit(0.45, "npc"), y = unit(0.82, "npc"), width = unit(0.15, "npc"), height = unit(0.15, "npc"))
  )
  
  for (i in seq_along(inset_grobs)) {
    inset_grob <- inset_grobs[[i]]
    vp <- viewport_positions[[i]]
    
    # Add the inset grob as an annotation
    combined_grob <- gTree(
      children = gList(
        combined_grob, 
        grobTree(children = gList(inset_grob), 
                 vp = vp)
      )
    )
  }
  
  return(combined_grob)
}
