# function to create legal problem prevalence table
genTable <- function(data) {
  # Pivot and mutate data for table creation
  table_data <- data %>%
    select(country = country_name_ltn, category, value2plot) %>%
    pivot_wider(names_from = category, values_from = value2plot) %>%
    mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
  
  # Create flextable with theme and formatting
  prevalence_table <- table_data %>%
    flextable() %>%
    theme_zebra(odd_header = "transparent", odd_body = "#e2e0df") %>%
    colformat_double(digits = 1, suffix = "%") %>%  # Format value2plot with one decimal and %
    set_table_properties(width = 1, layout = "fixed", align = 'center') %>%
    width(width = 2) %>% align_nottext_col(align = "center", header = TRUE) %>%
    align_text_col(align = "left", header = FALSE) %>%
    set_header_labels(
      country = " ",
      `citizenship and ID` = "Citizenship and ID",
      `community` = "Community",
      consumer = 'Consumer',
      education = "Education",
      employment = "Employment",
      family = "Family",
      housing = "Housing",
      injury = "Injury",
      land = "Land",
      `law enforcement` = "Law Enforcement",
      `money and debt` = "Money and Debt",
      `public services` = "Public Services"
    )  %>% bold(part = "header") %>% fontsize(size = 18, part = "body") %>%
    fontsize(size = 15, part = "header") 
  
  # Apply background colors based on prevalence levels
  for (i in 1:nrow(table_data)) {
    for (j in 2:ncol(table_data)) {
      value <- as.numeric(table_data[i, j])  # Get numeric value
      if (!is.na(value)) {  # Check for NA values
        color <- if (value > 75) {
          "#E03849"
        } else if (value > 50) {
          "#FF7900"
        } else if (value > 25) {
          "#FFC818"
        } else if (value > 10) {
          "#46B5FF"
        } else {
          "#0C75B6"   
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
  
  return(plot_data)
}

