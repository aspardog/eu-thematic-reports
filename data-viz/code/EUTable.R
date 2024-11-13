# function to create legal problem prevalence table
genTable <- function(data) {
  # Pivot and mutate data for table creation
  table_data <- data %>%
    select(country = country_name_ltn, category, value2plot) %>%
    pivot_wider(names_from = category, values_from = value2plot) %>%
    mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
  
  # Create flextable with theme and standardized font
  prevalence_table <- table_data %>%
    flextable() %>%
    theme_zebra(odd_header = "transparent", odd_body = "#e2e0df") %>%
    colformat_double(digits = 1, suffix = "%") %>%  
    set_table_properties(width = 1, layout = "fixed", align = 'center') %>%
    width(width = 2.25) %>%
    align_nottext_col(align = "center", header = TRUE) %>%
    align_text_col(align = "left", header = FALSE) %>%
    set_header_labels(
      country = " ",
      `citizenship and ID` = "Citizenship \nand ID",
      `community` = "Community",
      consumer = 'Consumer',
      education = "Education",
      employment = "Employment",
      family = "Family",
      housing = "Housing",
      injury = "Injury",
      land = "Land",
      `law enforcement` = "Law \nEnforcement",
      `money and debt` = "Money \nand Debt",
      `public services` = "Public \nServices"
    ) %>%
    bold(part = "header") %>%
    fontsize(j = 2:ncol(table_data), size = 25, part = "body") %>%
    fontsize(j = 1, size = 22, part = "body") %>%
    bold(j = 1, part = "body") %>%  # Bold country names
    bold(j = 2:ncol(table_data), part = "body") %>%  # Make numbers bold
    # Slightly reduced font size
    fontsize(size = 22, part = "header") 
  
  # Function to create a wider, shorter horizontal gradient legend at the bottom
  create_horizontal_legend <- function() {
    legend_plot <- ggplot() +
      geom_tile(aes(y = 1, x = seq(0, 30, length.out = 100), fill = seq(0, 30, length.out = 100))) +
      scale_fill_gradientn(
        colors = c("#46B5FF", "#0C75B6", "#FFC818", "#FF7900", "#E03849"),
        limits = c(0, 30),
        name = "Prevalence (%)",
        guide = guide_colorbar(
          barwidth = 10,  # Wider legend bar
          barheight = 0.3,  # Reduced height for the legend bar
          title.position = "top",
          title.hjust = 0.5
        )
      ) +
      theme_void() +
      theme(
        legend.position = "bottom",
        legend.title = element_text(family = "Lato Regular", size = 6.5, face = "bold", color = "#524F4C"),
        legend.text = element_text(family = "Lato Regular", size = 6.5)
      )
    
    # Extract the legend as a grob
    legend_grob <- cowplot::get_legend(legend_plot)
    return(legend_grob)
  }
  
  # Apply background colors
  for (i in 1:nrow(table_data)) {
    for (j in 2:ncol(table_data)) {
      value <- as.numeric(table_data[i, j]) 
      if (!is.na(value)) {  
        color <- if (value > 24) {
          "#E03849"
        } else if (value > 18) {
          "#FF7900"
        } else if (value > 12) {
          "#FFC818"
        } else if (value > 6) {
          "#0C75B6"  
        } else {
          "#46B5FF"
        }
        prevalence_table <- bg(prevalence_table, i = i, j = j, bg = color, part = "body")
      }
    }
  }
  
  # Convert flextable to grob
  table_grob <- flextable::gen_grob(prevalence_table)
  
  # Create ggplot object with the table grob
  plot_data <- ggplot() +
    annotation_custom(
      table_grob,
      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
    ) +
    theme_void() +
    coord_flip() +  # Flip coordinates for horizontal display
    theme(
      axis.text = element_text(family = "Lato Full", 
                               size   = 11,
                               color  = "#524F4C"), 
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) +
    WJP_theme()  
  
  # Generate the horizontal legend grob
  legend_grob <- create_horizontal_legend()
  
  # Combine the legend and table using cowplot
  combined_plot <- plot_grid(legend_grob, plot_data, ncol = 1, rel_heights = c(0.08, 1))
  
  return(combined_plot)
}

