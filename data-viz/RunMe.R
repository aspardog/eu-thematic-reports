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
## Outline: This is the single-call file for generating data points and visualizations for the EU Thematic
##          reports. 
##          
##          PRESETTINGS:  Load settings and function definitions, data
##          WRANGLE DATA: Create chart lists which are passed to the appropriate wrangle data function to acquire 
##                        data points for visualizations.
##          CREATE VIZ:   Call and apply the appropriate visualization function. 
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
source("code/EUDumbell.R")
source("code/EUmap.R")
source("code/EUScatterplot.R")

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

# Creating named lists to loop over
chart_list <- list(
  "GPP" = c(2:3, 5, 7:8, 10:14, 16,18, 21:22, 25:26, 29,32:35, 38:42,
                  44:46),
  "QRQ" = c(1, 4, 6, 9, 15, 17, 19, 23, 27, 30, 31, 36, 43),
  "Scatter" = c(20,24,28,37)
)

# getting names
chart_list <- lapply(chart_list, function(vec){
  names(vec) <- paste("Chart", vec)
  return(vec)
})

data_points <-list()

for (category in names(chart_list)){
  chart_n_vector <- chart_list[[category]]
  if (category == "GPP"){
    result <- lapply(chart_n_vector, function(chart_n) wrangleData(chart_n, 'gpp'))
  } else if (category == "QRQ"){
    result <- lapply(chart_n_vector, function(chart_n) wrangleData(chart_n, 'qrq'))
  } else if (category == "Scatter"){
    result <- lapply(chart_n_vector, function(chart_n) wrangleData_scatter(chart_n))
  }
  data_points <- c(data_points, result)
}



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
  ))

# regional data 
data_points_df_total <- getAvgData()   

write_csv(data_points_df_total, "data_points.csv")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Create Viz                                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Calling the visualizer for each chart
lapply(
   chart_list,
   callVisualizer
 )


