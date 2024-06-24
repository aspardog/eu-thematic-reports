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

# Create a named list to loop over
chart_list <- list(
  "GPP" = c(3,4,7,9,10,12:17,19:23,25:27,31,32,34:36,38:40,42:48,53:56,58:62,68,
            71:74,76,78,79,86:94,96,97,99,101,102,104:108,110,112,126,127,130,131,134,137:140,
            143:147,149:151,155:165,167:170,172,176,177,179:182),
  "QRQ" = c(1,2,5,6,8,11,18,24,28,29,30,33,37,49:52,57,63,64,65,70,75,77,82,84,85,95,98,
            100,103,109,111,124,128,132,135,136,141,148,154,171,173,174,175,178,184:186),
  "Scatter" = c(41, 80, 83, 125, 129, 133, 142)
)



# getting names
chart_list <- lapply(chart_list, function(vec){
  names(vec) <- paste("Chart", vec)
  return(vec)
})

data_points <-list()

# apply appropriate wrangling function and append to data points large list
for (category in names(chart_list)){
  chart_n_vector <- chart_list[[category]]
  if (category == "QRQ"){
    result <- lapply(chart_n_vector, function(chart_n) wrangleData(chart_n, 'qrq'))
  }else if (category == "GPP"){
    result <- lapply(chart_n_vector, function(chart_n) wrangleData(chart_n, 'gpp'))
  }else if (category == "Scatter"){
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


