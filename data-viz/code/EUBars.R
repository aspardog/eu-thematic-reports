library(ggplot2)
library(dplyr)
library(ggtext)

gen_bars <- function(data) {
  data <- data %>%
    filter(demographic == "Total Sample", level == "national") %>%
    mutate(
      color_group = cut(value2plot, 
                        breaks = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1),
                        labels = c("0%-10%", "10%-25%", "25%-50%", 
                                   "50%-75%", "75%-90%", "90%-100%")),
      color_group = factor(color_group, levels = c("0%-10%", "10%-25%", "25%-50%", 
                                                   "50%-75%", "75%-90%", "90%-100%"))
    ) %>%
    arrange(value2plot)
  
  plt <- ggplot(data, aes(x = reorder(country_name_ltn, value2plot), 
                          y = value2plot, 
                          fill = color_group)) +
    geom_bar(stat = "identity", width = 0.9) +
    geom_richtext(aes(label = paste0(
      "<b>", country_name_ltn, "</b><br>",
      "Percentage: ", scales::percent(value2plot, accuracy = 0.1)
    )),
    vjust = 0.5, 
    hjust = -0.1, 
    position = position_stack(vjust = 0.5), 
    fill = NA, 
    label.color = NA, 
    text.color = "black", 
    size = 3) +
    scale_fill_manual(values = cat_palette) +
    labs(y = "% of respondents", x = "Country") +
    scale_y_continuous(limits = c(0, 1), 
                       labels = scales::percent_format(accuracy = 1)) +
    coord_flip() +
    theme(axis.text.x = element_text(family = "Lato Full", 
                                     face = "plain", 
                                     size = 3.514598 * .pt, 
                                     color = "#524F4C", 
                                     angle = 45, 
                                     hjust = 1),
          axis.text.y = element_text(family = "Lato Full", 
                                     face = "plain", 
                                     size = 3.514598 * .pt, 
                                     color = "#524F4C"),
          axis.title = element_blank(),
          legend.position = "none",
          panel.grid.major = element_blank(),  # Remove major gridlines
          panel.grid.minor = element_blank(),  # Remove minor gridlines
          panel.background = element_blank(),  # Remove panel background color
          plot.background = element_blank())   # Remove plot background color
  
  print(plt)
  return(plt)
}

# Example usage:
# Replace `data` with your actual dataset
# gen_bars(data)





