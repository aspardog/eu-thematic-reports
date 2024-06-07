genMap <- function(dta){
  
  # Creating table
  table <- dta %>%
    group_by(country_name_ltn) %>%
    summarise(
      avg_value = mean(value2plot, na.rm = T),
      .groups   = "keep"
    ) %>%
    
    mutate(
      ` ` = "",
      `%` = round(avg_value*100, 1)
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
    flextable::style(pr_t = fp_text(font.size   = 16, 
                                    color       = "#524F4C",
                                    font.family = "Lato Full"), 
                     part = "header") %>%
    flextable::style(pr_t = fp_text(font.size   = 13, 
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
                     fit      = "fixed",
                     scaling  = "fixed", 
                     just     = c("left", "top"),
                     wrapping = T)
  
  # Drawing individual panels
  panels <- imap(
    map_layers,
    function(panel, panel_name){
      
      # Merging map layers with data
      data4map <- panel %>% 
        left_join(dta, 
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
          color_group = as.factor(color_group)
        )
      
      country_level <- data4map %>%
        group_by(CNTR_CODE) %>%
        summarise()
      
      # Drawing plot
      p <- ggplot() +
        geom_sf(data  = data4map,
                aes(fill = color_group),
                color = "grey65",
                size  = 0.5) +
        geom_sf(data  = country_level,
                fill  = NA,
                color = "grey25") +
        scale_fill_manual("",
                          values   = cat_palette,
                          na.value = "grey95",
                          drop = F)
      
      if (panel_name == "Main"){
        p <- p +
          scale_y_continuous(limits = c(1445631, 5273487)) +
          scale_x_continuous(limits = c(2581570, 5967160)) +
          theme_minimal() +
          theme(
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
          theme_minimal() +
          theme(
            axis.title.x    = element_text(family = "Lato Full",
                                           face   = "plain",
                                           size   = 13,
                                           color  = "#524F4C"),
            axis.text       = element_blank(),
            legend.position = "none",
            panel.grid      = element_blank(),
            panel.border    = element_rect(colour    = "#e6e7e8", 
                                           fill      = NA, 
                                           linewidth = 0.75),
            plot.margin = margin(2,2,2,2)
          )
      }
      
      return(p)
    }
  )
  
  # Assembling the patchwork
  layout <- "ABBB
             ABBB
             ABBB
             ABBB
             #CDE"
  patch <- wrap_elements(tpanel) + 
    panels[[1]] + panels[[2]] + panels[[3]] + panels[[4]] +
    plot_layout(design = layout)
  
  return(patch)
  
}
