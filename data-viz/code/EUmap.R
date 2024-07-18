genMap <- function(dta){
  


  # Creating table
  table <- dta %>%
    ungroup() %>%
    filter(level == "national" & demographic == "Total Sample") %>%
    mutate(
      ` ` = "",
      `%` = round(value2plot*100, 1)
    ) %>%
    arrange(country_name_ltn) %>%
    select(
      Country = country_name_ltn, ` `, `%`
    ) %>%
    flextable() %>%
    theme_zebra(
      odd_header = "transparent",
      odd_body   = "#e2e0df"
    ) %>%

    padding(j = 1, padding.right = 30) %>%
    padding(j = 1, padding.left  = 10) %>%
    padding(j = 3, padding.left  = 10) %>%

    width(j = " ", width = 0.8, unit = "mm") %>%
    width(j = "%", width = 1,   unit = "mm") %>%

    bg(i = ~ `%` <  10, j = ' ', bg = "#E03849", part = "body") %>%
    bg(i = ~ `%` >= 10, j = ' ', bg = "#FF7900", part = "body") %>%
    bg(i = ~ `%` >= 25, j = ' ', bg = "#FFC818", part = "body") %>%
    bg(i = ~ `%` >= 50, j = ' ', bg = "#46B5FF", part = "body") %>%
    bg(i = ~ `%` >= 75, j = ' ', bg = "#0C75B6", part = "body") %>%
    bg(i = ~ `%` >= 90, j = ' ', bg = "#18538E", part = "body") %>%


    align(j     = 3,
          align = "center",
          part  = "all") %>%
    bold(bold = FALSE,
         part = "header") %>%
    flextable::style(pr_t = fp_text(font.size   = 12,
                                    color       = "#524F4C",
                                    font.family = "Lato Full"),
                     part = "header") %>%
    flextable::style(pr_t = fp_text(font.size   = 8,
                                    color       = "#524F4C",
                                    font.family = "Lato Full"),
                     part = "body") %>%
    italic(italic = TRUE,
           part = "header") %>%
    surround(j = 2,
             border.top    = fp_border("white"),
             border.bottom = fp_border("white"),
             part = "body"
    )

  tpanel <- gen_grob(table,
                     fit = "fixed",
                     scaling = "fixed",
                     heights = c(0.5), # Adjust the height of the table
                     just = c("left", "top"),
                     wrapping = TRUE)

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

  # Drawing individual panels
  panels <- imap(
    map_layers, function(panel, panel_name){

      # Merging map layers with data
      data4map <- panel %>%
        left_join(
          dta %>%
            filter(level == "regional" & demographic == "Total Sample"),
          by = c("polID" = "nuts_id")
        ) %>%
        mutate(
          color_group = case_when(
            value2plot >  0.00 & value2plot <= 0.10 ~ "0%-10%",
            value2plot >  0.10 & value2plot <= 0.25 ~ "10%-25%",
            value2plot >  0.25 & value2plot <= 0.50 ~ "25%-50%",
            value2plot >  0.50 & value2plot <= 0.75 ~ "50%-75%",
            value2plot >  0.75 & value2plot <= 0.90 ~ "75%-90%",
            value2plot >  0.90 & value2plot <= 1.00 ~ "90%-100%"
          ),
          color_group  = as.factor(color_group)
        )

      centroids <- data4map %>%
        filter(polID %in% inner_regions) %>%
        st_centroid() %>%
        mutate(
          lon = st_coordinates(.)[,1],
          lat = st_coordinates(.)[,2],
          lat = if_else(polID == "MT00", lat + 5000, lat),
          tooltip = paste0(
            "**",nameSHORT,"**<br>",
            "_",country_name_ltn,"_<br>",
            "Percentage:",
            paste0(
              format(round(value2plot*100, 1),
                     nsmall = 1),
              "%"
            )
          )
        ) 

      country_level <- data4map %>%
        group_by(CNTR_CODE) %>%
        summarise()

      # Drawing plot
      p <- ggplot() +
        geom_sf(data  = data4map,
                aes(
                  fill  = color_group,
                  color = polID
                ),
                # color = "grey65",
                size  = 0.5) +
        geom_sf(data  = country_level,
                fill  = NA,
                color = "grey25") +
        scale_fill_manual("",
                          values   = cat_palette,
                          na.value = "grey95",
                          drop = F) +
        scale_colour_manual("",
                            values   = border_color,
                            na.value = "#ABA1A7",
                            drop = F) +
        new_scale_colour() +
      geom_richtext(data = centroids,
                 aes(
                   y      = lat,
                   x      = lon,
                    label  = tooltip,
                   colour = polID,
                 ),
                 family   = "Lato Full",
                 fontface = "plain",
                 size     = 3,
                 fill  = "white",
                 vjust = "inward",
                 hjust = "inward") +
      scale_colour_manual("",
                          values   = label_color,
                          na.value = "#212429",
                          drop = F)

      if (panel_name == "Main"){
        p <- p +
          scale_y_continuous(limits = c(1442631, 5323487)) + # increase max long for more space on the right
          scale_x_continuous(limits = c(2581570, 6017160)) +
          theme_minimal() +
          theme(
            axis.title.x    = element_blank(),
            axis.title.y    = element_blank(),
            axis.text       = element_blank(),
            legend.position = "none",
            panel.grid      = element_blank(),
            panel.border    = element_rect(colour    = "#e6e7e8",
                                           fill      = NA,
                                           linewidth = 0.75),
            plot.margin     = margin(0,0,0,0)
          )
      } else {
        p <- p +
          labs(x = panel_name) +
          coord_sf(clip = "off") +
          theme_minimal() +
          theme(
            panel.background = element_rect(fill = "white"),
            axis.title.x    = element_text(family = "Lato Full",
                                           face   = "plain",
                                           size   = 9,
                                           color  = "#524F4C"),
            axis.text       = element_blank(),
            axis.title.y = element_blank(),
            legend.position = "none",
            panel.grid      = element_blank(),
            panel.border    = element_rect(colour    = "#e6e7e8",
                                           fill      = NA,
                                           linewidth = 0.75),
            plot.margin = margin(2,0,0,2)
          )
      }

      return(p)
    }
  )

  # Inserting inset map boxes
  main_map <- panels[["Main"]]
  insets <- list(panels[["Canarias/Madeiras"]], panels[["Açores"]], panels[["Cyprus"]])

  inset_grobs <- lapply(insets, ggplotGrob)

  main_map_with_insets <- main_map +
    annotation_custom(grob = inset_grobs[[1]], xmin = 2400000, xmax = 2600000, ymin = 4323487, ymax = 4968487) +
    annotation_custom(grob = inset_grobs[[2]], xmin = 2800000, xmax = 3000000, ymin = 4323487, ymax = 4968487) +
    annotation_custom(grob = inset_grobs[[3]], xmin = 3200000, xmax = 3400000, ymin = 4323487, ymax = 4968487)
  


  layout <- "
  ABB
  ABB
  ABB
  ABB
  "

  patch <- wrap_elements(tpanel) + main_map_with_insets +
    plot_layout(
      design = layout, heights = c(1))
  
  
  print(patch)

  return(patch)

}

