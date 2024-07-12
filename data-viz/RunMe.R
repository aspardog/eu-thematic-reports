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
  ) %>%
  mutate(
    # Luxembourg age data is already categorized
    age = case_when(
      country_name_ltn == "Luxembourg" & age == 1 ~ 24,
      country_name_ltn == "Luxembourg" & age == 2 ~ 34,
      country_name_ltn == "Luxembourg" & age == 3 ~ 44,
      country_name_ltn == "Luxembourg" & age == 4 ~ 54,
      country_name_ltn == "Luxembourg" & age == 5 ~ 64,
      country_name_ltn == "Luxembourg" & age == 6 ~ 65,
      TRUE ~ age
    ),
    age_groups = case_when(
      age <= 24             ~ "18-24",
      age >= 25 & age <= 34 ~ "25-34",
      age >= 35 & age <= 44 ~ "35-44",
      age >= 45 & age <= 54 ~ "45-54",
      age >= 55 & age <= 64 ~ "55-64",
      age >= 65             ~ "+65",
    ),
    gender = case_when(
      gend == 1 ~ "Male",
      gend == 2 ~ "Female",
      gend >= 3 ~ "Other"
    ),
    iq_groups = case_when(
      income_quintile == 1 ~ "Income Quintile 1",
      income_quintile == 2 ~ "Income Quintile 2",
      income_quintile == 3 ~ "Income Quintile 3",
      income_quintile == 4 ~ "Income Quintile 4",
      income_quintile == 5 ~ "Income Quintile 5"
    ),
    urban_string = case_when(
      urban == 1 ~ "Urban",
      urban == 2 ~ "Rural"
    )
  )
master_data_gpp <- master_data_gpp %>%
  left_join(
    addSpecial(master_data_gpp),
    by = "country_year_id"
  )
  
master_data_qrq <- read_dta(
  file.path(
    path2EU,
    "EU-S Data/eu-qrq/1. Data/eu_qrq_nuts_final.dta"
  )
)

# Loading outline
outline <- read.xlsx(
  file.path(
    path2EU,
    "EU-S Data/reports/eu-thematic-reports/data-viz/inputs/report_outline.xlsx"),
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
  "GPP"     = outline %>% 
    filter(special_wrangling == F & description == "GPP") %>% 
    pull(chart_id),
  "QRQ"     = outline %>% 
    filter(special_wrangling == F & description == "QRQ") %>% 
    pull(chart_id),
  "Special" = outline %>%
    filter(special_wrangling == T) %>%
    pull(chart_id)
)

# Getting data points
data_points <- imap(
  chart_list,
  function(clist, source){
    
    # Applying wrangling function (different for GPP/QRQ)
    wrangled_data_list <- lapply(
      clist,
      function(chart){
        wrangled_data <- wrangleData(figid = chart, source = source)
      }
    )
    
    if (source %in% c("GPP", "QRQ")) {
      # Getting country+EU averages
      wrangled_data <- getAvgData(
        bind_rows(wrangled_data_list)
      )
      
      # Saving data for website
      save4web(
        wrangled_data %>%
          filter(!chart_id %in% c("R1B1")), # We can skip the data from the Report1, Box 1, it is repeated
        source = source
      )
      
      return(wrangled_data)
    }
    if (source %in% c("Special")){
      names(wrangled_data_list) <- chart_list[["Special"]]
      return(wrangled_data_list)
    }
  }
)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Create Viz                                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Calling the visualizer for each chart
lapply(
  outline %>% 
    filter(thematic_reports == T) %>% 
    filter(type %in% c("Dumbbells", "Map")) %>%
    pull(chart_id),
  callVisualizer
)
