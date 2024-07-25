gen_catMap <- function(dta, base_map) {
  
  # Define the colors for categories
  unique_categories <- unique(dta$value2plot)
  category_colors <- setNames(cat_map_palette[1:length(unique_categories)], unique_categories)
  
  # Merge data with base map
  data4map <- base_map %>%
    left_join(dta, by = c("polID" = "nuts_id"))
  
  inset_dimensions <- list(
    # y range longer than x
    "Canarias/Madeiras" = list(
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
  
  # Define insets with labels
  insets <- list(
    list(region = c("ES7", "PT3"), label = "Canarias/Madeiras"),
    list(region = "PT2", label = "Açores"),
    list(region = "CY0", label = "Cyprus")
  )
  
  # Convert insets to grob objects using ggplotGrob
  inset_grobs <- lapply(insets, function(inset) {
    region <- inset$region
    label <- inset$label
    region_data <- data4map %>%
      filter(polID %in% region)
    
    # ggplot object for insets
    gg_inset <- ggplot(region_data) +
      geom_sf(aes(fill = value2plot), color = "black", size = 0.1) +
      scale_fill_manual(values = category_colors, na.value = "grey95") +
      scale_y_continuous(limits = inset_dimensions[[label]][["y"]]) + 
      scale_x_continuous(limits = inset_dimensions[[label]][["x"]]) +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)
      ) +
      labs(fill = "Category") +
      ggtitle(label) +
      theme(plot.title = element_text(size = 8, hjust = 0.5))
    
    # Convert to grob
    ggplotGrob(gg_inset)
  })
  
  # Main map
  p <- ggplot(data4map) +
    geom_sf(aes(fill = value2plot), color = "black", size = 0.1) +
    scale_fill_manual(values = category_colors, na.value = "grey95") +
    scale_y_continuous(limits = c(1442631, 5323487)) + # Increase max long for more space on the right
    scale_x_continuous(limits = c(2581570, 6017160)) +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      panel.border = element_rect(color = "#e6e7e8", fill = NA, size = 0.55),
      panel.background = element_blank(),
      plot.margin = margin(10, 50, 10, 10)
    ) + 
    labs(fill = "Category") + 
    coord_sf(crs = st_crs(base_map)) 
  
  
  # Convert the main plot to a grob
  p_grob <- ggplotGrob(p)
  
  # Create a new gTree to hold the main plot and insets
  combined_grob <- gTree(children = gList(p_grob))
  
  # Add insets to the main map
  viewport_positions <- list(
    viewport(x = unit(0.2, "npc"), y = unit(0.87, "npc"), width = unit(0.15, "npc"), height = unit(0.15, "npc")),
    viewport(x = unit(0.31, "npc"), y = unit(0.87, "npc"), width = unit(0.15, "npc"), height = unit(0.15, "npc")),
    viewport(x = unit(0.42, "npc"), y = unit(0.87, "npc"), width = unit(0.15, "npc"), height = unit(0.15, "npc"))
  )
  
  for (i in seq_along(inset_grobs)) {
    inset_grob <- inset_grobs[[i]]
    vp <- viewport_positions[[i]]
    
    # Add the inset grob as an annotation
    combined_grob <- gTree(children = gList(combined_grob, grobTree(children = gList(inset_grob), vp = vp)))
  }
  
  # Draw the plot with insets
  # grid.newpage()
  # grid.draw(combined_grob)
  
  return(combined_grob)
}












