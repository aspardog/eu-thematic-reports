genMap <- function(dta, direction, static = FALSE){
  
  # Creating flextable
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
      odd_body   = "#efefef"
    ) %>%
    
    padding(j = 1, padding.right = 30) %>%
    padding(j = 1, padding.left  = 10) %>%
    padding(j = 3, padding.left  = 10) %>%
    
    width(j = " ", width = 0.8, unit = "mm") %>%
    width(j = "%", width = 1,   unit = "mm")
  
  if (direction %in% c("positive", "neutral")){
    cpal  <- cat_palette
    table <- table %>%
      bg(i = ~ `%` <  10, j = ' ', bg = "#E03849", part = "body") %>%
      bg(i = ~ `%` >= 10, j = ' ', bg = "#FF7900", part = "body") %>%
      bg(i = ~ `%` >= 25, j = ' ', bg = "#FFC818", part = "body") %>%
      bg(i = ~ `%` >= 50, j = ' ', bg = "#46B5FF", part = "body") %>%
      bg(i = ~ `%` >= 75, j = ' ', bg = "#0C75B6", part = "body") %>%
      bg(i = ~ `%` >= 90, j = ' ', bg = "#18538E", part = "body")
  } else {
    cpal  <- cat_palette_inverted
    table <- table %>%
      bg(i = ~ `%` <  10, j = ' ', bg = "#18538E", part = "body") %>%
      bg(i = ~ `%` >= 10, j = ' ', bg = "#0C75B6", part = "body") %>%
      bg(i = ~ `%` >= 25, j = ' ', bg = "#46B5FF", part = "body") %>%
      bg(i = ~ `%` >= 50, j = ' ', bg = "#FFC818", part = "body") %>%
      bg(i = ~ `%` >= 75, j = ' ', bg = "#FF7900", part = "body") %>%
      bg(i = ~ `%` >= 90, j = ' ', bg = "#E03849", part = "body")
  }
    
  table <- table %>%
    align(j     = 3,
          align = "center",
          part  = "all") %>%
    bold(bold = FALSE,
         part = "header") %>%
    flextable::style(pr_t = fp_text(font.size   = 12,
                                    color       = "#222221",
                                    font.family = "Lato Full",
                                    bold        = T),
                     part = "header") %>%
    flextable::style(pr_t = fp_text(font.size   = 8,
                                    color       = "#222221",
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
  
  # Defining inset dimensions
  inset_dimensions <- list(
    # y range longer than x
    "Canaries/Madeira" = list(
      "x" = c(1491672, 2091880.2),
      "y" = c(941748.3, 1541956.5 )
    ),
    # the y range is longer than the x
    "Azores"  = list(
      "x" = c(864542.6, 1407344),
      "y" = c(2250319.3, 2793120.7)
    ),
    # x range is longer than y
    "Cyprus" = list(
      # "x" = c(6342221, 6524714),
      "x" = c(6332001, 6525814),
      "y" = c(1585147, 1767640)
    )
  )
  
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
          color_group = factor(
            color_group, 
            levels = c("0%-10%",  "10%-25%", "25%-50%", 
                       "50%-75%", "75%-90%", "90%-100%")
          )
        )
      
      centroids <- data4map %>%
        filter(polID %in% inner_regions) %>%
        st_centroid() %>%
        mutate(
          lon = st_coordinates(.)[,1],
          lat = st_coordinates(.)[,2],
          lat = if_else(polID == "MT00", lat + 5000, lat),
          nameSHORT = str_trim(nameSHORT),
          nameSHORT = case_when(
            nameSHORT == "Auvergne-Rhône-Alpes"   ~ "Auvergne-Rhône- Alpes",
            nameSHORT == "Burgundy-Franche-Comté" ~ "Burgundy- Franche-Comté",
            nameSHORT == "Mecklenburg-Vorpommern" ~ "Mecklenburg- Vorpommern",
            nameSHORT == "South -West/-Central"   ~ "South -West/ -Central",
            TRUE ~ nameSHORT
          ),
          tooltip = paste0(
            "<b>",nameSHORT,"</b><br>",
            "<i>",str_trim(country_name_ltn),"</i><br>",
            paste0(
              format(round(value2plot*100, 1),
                     nsmall = 1),
              "%"
            )
          ),
          hjust = case_when(
            polID %in% c("ES7", "PT3", "PT2", "CY0") ~ 0,
            lon <  4299365 ~ 0,
            lon >= 4299365 ~ 1,
          ),
          vjust = case_when(
            polID %in% c("ES7", "PT3", "PT2", "CY0") ~ 1,
            lat <  3383059 ~ 0,
            lat >= 3383059 ~ 1
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
                size  = 0.5,
                show.legend = c(fill = TRUE)) +
        geom_sf(data  = country_level,
                fill  = NA,
                color = "grey25") +
        scale_fill_manual("",
                          values   = cpal,
                          na.value = "#d8d8d8",
                          drop     = F,
                          na.translate = F) +
        scale_colour_manual("",
                            values   = border_color,
                            na.value = "#ABA1A7",
                            guide    = "none") +
        new_scale_colour() 
      
        if (static == FALSE){
        p <- p + geom_textbox(
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
        )+ scale_colour_manual("",
                               values   = label_color,
                               na.value = "#212429",
                               guide    = "none")} 

      
      if (panel_name == "Main"){
        p <- p +
          scale_y_continuous(limits = c(1442631, 5323487)) +
          scale_x_continuous(limits = c(2581570, 6017160)) +
          theme_minimal() +
          theme(
            axis.title.x     = element_blank(),
            axis.title.y     = element_blank(),
            axis.text        = element_blank(),
            panel.grid       = element_blank(),
            panel.border     = element_rect(colour    = "#e6e7e8",
                                            fill      = NA,
                                            linewidth = 0.75),
            plot.margin      = margin(0,0,0,0),
            legend.position  = "top",
            legend.text      = element_text(family = "Lato Full", 
                                            face   = "bold", 
                                            size   = 2.8 * .pt,
                                            margin = margin(0,2,0,2)),
            legend.key.size      = unit(0.15, "inches"), 
            legend.key.spacing   = unit(0.165, "inches"),
            legend.margin        = margin(2,0,0,0),
            legend.justification = "left" 
          ) +
          guides(
            fill = guide_legend(
              nrow  = 1,
              byrow = TRUE,
              override.aes = list(
                size   = 0.25,
                colour = NA
              )
            )
          )
      } else {
        p <- p +
          labs(
            # x = str_replace(panel_name, "/","\n")
            title = panel_name
          ) +
          scale_y_continuous(
            limits   = inset_dimensions[[panel_name]][["y"]]
          ) +
          scale_x_continuous(
            limits   = inset_dimensions[[panel_name]][["x"]],
            position = "top"
          ) +
          coord_sf(clip = "off") +
          theme_minimal() +
          theme(
            panel.background  = element_rect(fill = NA,
                                             color = "#e6e7e8",
                                             linewidth = .5),
            plot.title        = element_text(family = "Lato Full",
                                             face   = "plain", 
                                             size   = 2.663138 * .pt,
                                             color  = "#222221",
                                             hjust  = 0.5),
            axis.text         = element_blank(),
            axis.title.x      = element_blank(),
            axis.title.y      = element_blank(),
            legend.position   = "none",
            panel.grid        = element_blank()
          )
      }
      
      return(p)
    }
  )
  
  # Inserting inset map boxes
  main_map <- panels[["Main"]]
  insets   <- list(panels[["Canaries/Madeira"]], panels[["Azores"]], panels[["Cyprus"]])
  inset_grobs <- lapply(insets, ggplotGrob)
  
  main_map_with_insets <- main_map +
    annotation_custom(grob = inset_grobs[[1]], ymin = 4.5e6, ymax = 5.5e6, xmin = 2.5e6, xmax = 3e6) +
    annotation_custom(grob = inset_grobs[[2]], ymin = 4.5e6, ymax = 5.5e6, xmin = 3e6,   xmax = 3.5e6) + 
    annotation_custom(grob = inset_grobs[[3]], ymin = 4.5e6, ymax = 5.5e6, xmin = 3.5e6, xmax = 4e6)
  
  patch <- plot_grid(
    tpanel, 
    main_map_with_insets, 
    rel_widths = c(1, 3)
  )

  return(patch)
  
}
