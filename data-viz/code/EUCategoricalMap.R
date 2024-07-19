gen_catMap <- function(dta, base_map) {
  # Filter out Iceland
  base_map <- base_map %>%
    filter(polID != 'IS')
  
  # Define the category colors
  cat_map_palette <- c("#49178e", "#dd58b1", "#24a0b5", "#ffc55d", "#d8d8d8")
  unique_categories <- unique(dta$value2plot)
  category_colors <- setNames(cat_map_palette[1:length(unique_categories)], unique_categories)
  
  # Merge data with the base map
  data4map <- base_map %>%
    left_join(dta, by = c("polID" = "nuts_id"))
  
  # Define insets with labels
  insets <- list(
    list(region = c("ES7", "PT3"), label = "Canarias\nMadeiras"),
    list(region = "PT2", label = "Açores"),
    list(region = "CY0", label = "Cyprus")
  )
  
  # Convert insets to grob objects using ggplotGrob
  inset_grobs <- lapply(insets, function(inset) {
    region <- inset$region
    label <- inset$label
    region_data <- data4map %>%
      filter(polID %in% region)
    
    gg_inset <- ggplot(region_data) +
      geom_sf(aes(fill = value2plot), color = "black", size = 0.5) +
      scale_fill_manual(values = category_colors, na.value = "grey95") +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)
      ) +
      labs(fill = "Category") +
      coord_sf(crs = st_crs(base_map), default_crs = NULL) +
      ggtitle(label) +
      theme(plot.title = element_text(size = 8, hjust = 0.5))
    
    # Convert to grob
    ggplotGrob(gg_inset)
  })
  
  # Main map
  p <- ggplot(data4map) +
    geom_sf(aes(fill = value2plot), color = "grey65", size = 0.5) +
    scale_fill_manual(values = category_colors, na.value = "grey95") +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      legend.position.inside = c(0.05, 0.95),  # Top-left corner
      legend.justification = c(0, 1),  # Align top-left
      legend.title = element_blank(),
      legend.background = element_rect(fill = alpha('white', 0.6), color = NA),  
      panel.border = element_rect(color = "#e6e7e8", fill = NA, size = 0.55),
      plot.margin = margin(0, 0, 0, 0)
    ) + WJP_theme()
    labs(fill = "Category") +
    coord_sf(crs = st_crs(base_map), default_crs = NULL)
  
  # Convert the main plot to a grob
  p_grob <- ggplotGrob(p)
  
  # Create a new gTree to hold the main plot and insets
  combined_grob <- gTree(children = gList(p_grob))
  
  # Add insets to the main map
  viewport_positions <- list(
    viewport(x = unit(0.1, "npc"), y = unit(0.56, "npc"), width = unit(0.12, "npc"), height = unit(0.12, "npc")),
    viewport(x = unit(0.15, "npc"), y = unit(0.55, "npc"), width = unit(0.1, "npc"), height = unit(0.1, "npc")),
    viewport(x = unit(0.2, "npc"), y = unit(0.55, "npc"), width = unit(0.1, "npc"), height = unit(0.1, "npc"))
  )
  
  for (i in seq_along(inset_grobs)) {
    inset_grob <- inset_grobs[[i]]
    vp <- viewport_positions[[i]]
    
    # Add the inset grob as an annotation
    combined_grob <- gTree(children = gList(combined_grob, grobTree(children = gList(inset_grob), vp = vp)))
  }
  
  # Draw the plot with insets
  grid.newpage()
  grid.draw(combined_grob)
  
  return(combined_grob)
}












