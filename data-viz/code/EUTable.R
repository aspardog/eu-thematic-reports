# function to create legal problem prevalence table
genTable <- function(data) {
  # Pivot and mutate data for table creation
  table_data <- data %>%
    select(country_name_ltn, category, value2plot) %>%
    pivot_wider(names_from = category, values_from = value2plot) %>%
    mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>%
    rename(
      "Citizenship and ID" = "citizenship and ID",
      "Community Resources" = "community resources",
      "Consumer" = 'consumer',
      "Education" = "education",
      "Employment" = "employment",
      "Family" = "family",
      "Housing" = "housing",
      "Injury" = "injury",
      "Land" = "land",
      "Law Enforcement" = "law enforcement",
      "Money and Debt" = "money and debt",
      "Public Services" = "public services"
    )
  
  # Create flextable with theme and formatting
  prevalence_table <- table_data %>%
    flextable() %>%
    theme_zebra(odd_header = "transparent", odd_body = "#e2e0df") %>%
    colformat_double(digits = 1, suffix = "%") %>%  # Format value2plot with one decimal and %
    set_table_properties(width = 0.9, layout = "autofit")
  
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
      axis.text.x = element_blank(),  # Hide x-axis labels
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) +
    WJP_theme()  
  
  return(plot_data)
}

