## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Thematic Report - RunMe File
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##                    Isabella Coddington         (icoddington@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     June 7th, 2024
##
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Presettings                                                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Loading settings and functions
source("code/settings.R")
source("code/functions.R")
# source("code/EUmap.R")
# source("code/EUbars.R")
# source("code/EUlollipop.R")

# Loading data
master_data_gpp <- read_dta(
  file.path(
    path2EU,
    "EU-S Data/eu-gpp/1. Data/3. Merge/EU_GPP_2024.dta")
  )
  
master_data_qrq <- read_dta(
  file.path(
    path2EU,
    "EU-S Data/eu-qrq/1. Data/eu_qrq_nuts.dta"
  )
) %>%
  rename( # renaming so I can apply wrangleData
    country_name_ltn = country,
    nuts_id = nuts
  )


# master_data <- master_data %>%
#   left_join(
#     add_A2J(master_data)
#   )

# Loading outline
outline <- read.xlsx(
  "inputs/report_outline.xlsx",
  sheet = "outline"
)

# Loading map layers
base_map <- st_read(
  file.path(
    path2DA,
    "8. Data/EU-NUTS-GIS/EU_base_map.geojson"
  )
) %>%
  filter(!(polID %in% c("GL")))

insets <- getInsets(list(
  "Canarias/Madeiras" = c("ES7", "PT3"),
  "Açores"            = "PT2",
  "Cyprus"            = "CY0"
))

map_layers <- c(
  list("Main" = base_map), 
  insets
)

# Loading region names
region_names <- read.xlsx(
  "inputs/region_labels.xlsx"
) %>%
  select(nuts_id, nameSHORT, pop_weight = regionpoppct)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Wrangle data                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Creating a named list to loop over
chart_list <- c(2:3, 5, 7:8, 10:14, 16,18, 21:22, 25:26, 29,32:35, 38:42,
                44:46)    # GPP WITH ONE VARIABLE
chart_list_2 <- c(1, 4, 6, 9, 15, 17, 19, 23, 27, 30, 31, 36, 43) # QRQ
names(chart_list) <- paste("Chart", chart_list)
names(chart_list_2) <- paste("Chart", chart_list_2)


# Applying the wrangling function to gpp charts
data_points_gpp <- lapply(
  chart_list,
  function(chart_n) wrangleData(chart_n, 'gpp')
)

# Applying the wrangling function to qrq charts
data_points_qrq <- lapply(
  chart_list_2, 
  function(chart_n) wrangleData(chart_n, 'qrq')
)

# Combining data points from GPP and QRQ
data_points <- c(data_points_gpp, data_points_qrq)

# Collapsing and saving data points data
data_points_df <- bind_rows(
  imap(
    data_points,
    function(df, chart_n){
      df %>%
        mutate(
          chart = str_replace(chart_n, "Chart ", "")
        )
    }
  )
)

# # csv with totals and demographic breakdowns
# write_csv(data_points_df, "data_points_all.csv")
# 
# 
# 
# # regional data
data_points_df_regional <- data_points_df %>% filter(demographic == "Total")
data_points_df_regional <- getAvgData()
# 
# 
# 
# # saving region-level data
write_csv(data_points_df_regional, "data_points.csv")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Create Viz                                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Calling the visualizer for each chart
# lapply(
#   chart_list,
#   callVisualizer
# )
