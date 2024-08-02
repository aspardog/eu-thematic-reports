## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Thematic Report - Settings
##
## Author(s):         Isabella Coddington   (icoddington@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     June 7, 2024
##
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Required packages                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(pacman)
p_load(char = c(
  # Visualizations
  "showtext", "ggtext", "grid", "patchwork", "cowplot", "flextable", "officer", "ggnewscale", 
  "ggExtra", "relayer",
  
  # Data Loading
  "haven", "writexl", "openxlsx",
  
  # GIS
  "tmaptools", "rmapshaper", "sf", "units",
  
  # Utilities
  "margins", "kableExtra", "glue",
  
  # Good 'ol Tidyverse
  "tidyverse"
  
))


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  SharePoint Path                                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if (Sys.info()["user"] == "ctoruno") {
  path2EU <- "/Users/ctoruno/OneDrive - World Justice Project/EU Subnational"
  path2DA <- "/Users/ctoruno/OneDrive - World Justice Project/Data Analytics"
} else if (Sys.info()["user"]=="Dhabiby"){
  path2EU <- "/Users/Dhabiby/World Justice Project/Research - EU Subnational"
  path2DA <- "/Users/Dhabiby/World Justice Project/Research - Data Analytics"
} else if(Sys.info()["user"] == "icoddington"){
  path2EU <- "/Users/icoddington/OneDrive - World Justice Project/EU Subnational"
  path2DA <- "/Users/icoddington/OneDrive - World Justice Project/Data Analytics"
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Loading Fonts                                                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

path2fonts<- file.path(
  path2DA, "6. Country Reports/0. Fonts", 
  fsep = "/"
)
font_add(family     = "Lato Full",
         regular    = file.path(path2fonts, "Lato-Regular.ttf", fsep = "/"),
         italic     = file.path(path2fonts, "Lato-LightItalic.ttf", fsep = "/"),
         bold       = file.path(path2fonts, "Lato-Bold.ttf", fsep = "/"),
         bolditalic = file.path(path2fonts, "Lato-BoldItalic.ttf", fsep = "/"))
font_add(family  = "Lato Light",
         regular = file.path(path2fonts, "Lato-Light.ttf", fsep = "/"))
font_add(family  = "Lato Black",
         regular = file.path(path2fonts, "Lato-Black.ttf", fsep = "/"))
font_add(family  = "Lato Black Italic",
         regular = file.path(path2fonts, "Lato-BlackItalic.ttf", fsep = "/"))
font_add(family  = "Lato Medium",
         regular = file.path(path2fonts, "Lato-Medium.ttf", fsep = "/"))
showtext_auto()


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4.  WJP theme                                                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

WJP_theme <- function() {
  theme(panel.background   = element_blank(),
        plot.background    = element_blank(),
        panel.grid.major   = element_line(size     = 0.25,
                                          colour   = "#5e5c5a",
                                          linetype = "dashed"),
        panel.grid.minor   = element_blank(),
        axis.title.y       = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C",
                                          margin   = margin(0, 10, 0, 0)),
        axis.title.x       = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C",
                                          margin   = margin(10, 0, 0, 0)),
        axis.text.y        = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C"),
        axis.text.x = element_text(family = "Lato Full",
                                   face   = "plain",
                                   size   = 3.514598*.pt,
                                   color  = "#524F4C"),
        axis.ticks  = element_blank(),
        plot.margin  = unit(c(0, 0, 0, 0), "points")
  ) 
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 5.  Color Palettes                                                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cat_palette <- c(
  "0%-10%"   = "#E03849",
  "10%-25%"  = "#FF7900",
  "25%-50%"  = "#FFC818",
  "50%-75%"  = "#46B5FF",
  "75%-90%"  = "#0C75B6",
  "90%-100%" = "#18538E"
)
cat_palette_inverted <- c(
  "0%-10%"   = "#18538E",
  "10%-25%"  = "#0C75B6",
  "25%-50%"  = "#46B5FF",
  "50%-75%"  = "#FFC818",
  "75%-90%"  = "#FF7900",
  "90%-100%" = "#E03849"
)

lpop_palette <- c("national" = "#454545",
                  "regional" = "#18a6ad")

cat_map_palette <- c("#49178e", "#dd58b1", "#24a0b5", "#ffc55d", "#006eb2")

dots_palette <- c("Value 1" = "#49178e",
                  "Value 2" = "#dd58b1")

scatter_palette <- c(
  "#A62639", "#E16F7C", "#EE6C4D", "#53131E", "#BA2D0B", "#F8C7CC", "#dd58b1",
  "#E8C547", "#F4F482", "#FBD1A2", "#ffc55d", "#FAA275",
  "#6D9F71", "#DEEFB7", "#2F4B26", "#D5F2E3", "#C1AE7C",
  "#24a0b5", "#99B2DD", "#414288", "#49178e",
  "#685044", "#36494E", "#222222", "#CDD1C4", "#d8d8d8", "#BEB7DF"
) %>%
  setNames(
    c("Austria",
      "Belgium",
      "Bulgaria",
      "Croatia",
      "Cyprus",
      "Czech Republic",
      "Denmark",
      "Estonia",
      "Finland",
      "France",
      "Germany",
      "Greece",
      "Hungary",
      "Ireland",
      "Italy",
      "Latvia",
      "Lithuania",
      "Luxembourg",
      "Malta",
      "Netherlands",
      "Poland",
      "Portugal",
      "Romania",
      "Slovakia",
      "Slovenia",
      "Spain",
      "Sweden")
  )




