library(ggplot2)
library(dplyr)
library(ggtext)

genDumbbells <- function(dta) {
  
  data2plot <- dta %>%
    filter(level == "regional" & !is.na(country_name_ltn) & !is.na(value2plot)) %>%
    mutate(
      color_group = case_when(
        value2plot > 0.00 & value2plot <= 0.10 ~ "0%-10%",
        value2plot > 0.10 & value2plot <= 0.25 ~ "10%-25%",
        value2plot > 0.25 & value2plot <= 0.50 ~ "25%-50%",
        value2plot > 0.50 & value2plot <= 0.75 ~ "50%-75%",
        value2plot > 0.75 & value2plot <= 0.90 ~ "75%-90%",
        value2plot > 0.90 & value2plot <= 1.00 ~ "90%-100%"
      ),
      color_group = as.factor(color_group),
      value2plot = value2plot * 100
    )
  
  data4segments <- data2plot %>%
    group_by(country_name_ltn) %>%
    summarise(
      min_y = min(value2plot, na.rm = TRUE),
      max_y = max(value2plot, na.rm = TRUE)
    )
  
  # Base plot
  chart <- ggplot() +
    geom_segment(
      data = data4segments,
      aes(
        x = country_name_ltn,
        xend = country_name_ltn,
        y = min_y,
        yend = max_y
      ),
      color = "gray95",
      size = 4
    ) +
    geom_point(
      data = data2plot,
      aes(
        x = country_name_ltn,
        y = value2plot,
        color = color_group
      ),
      size = 4
    ) +
    geom_rect(
      data = data2plot,
      aes(
        xmin = as.numeric(country_name_ltn) - 0.2,
        xmax = as.numeric(country_name_ltn) + 0.2,
        ymin = value2plot - 2,
        ymax = value2plot + 2
      ),
      fill = "white", colour = "black", size = 1
    ) +
    geom_richtext(
      data = data2plot,
      aes(
        x = country_name_ltn,
        y = value2plot,
        label = paste0(
          "<b>", nameSHORT, "</b>,<br>",
          "<i>", country_name_ltn, "</i><br>",
          "Percentage: ", scales::percent(value2plot / 100, accuracy = 0.1)
        )
      ),
      vjust = -0.5,
      size = 2.5,
      hjust = 1.2,
      fill = "white",
      label.color = "black",
      text.color = "black"
    ) +
    scale_color_manual(values = cat_palette) +
    scale_x_discrete(
      limits = rev(levels(factor(
        data2plot %>%
          filter(country_name_ltn != "European Union") %>%
          pull(country_name_ltn)
      )))
    ) +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, 105),
      breaks = seq(0, 100, 20),
      labels = paste0(seq(0, 100, 20), "%")
    ) +
    coord_flip() +
    theme_minimal() +
    theme(
      axis.text = element_text(family = "Lato Full", size = 8, color = "#524F4C"),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
    )
  
  print(chart)
  
  return(chart)
}





