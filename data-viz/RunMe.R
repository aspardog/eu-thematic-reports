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

# Loading additional code modules
modules <- c(
  "settings", "functions",
  "EUDumbbell", "EUmap", "EUScatterplot", "EUTable", "EUCategoricalMap", "EUDots", "EULollipop", "EUBars", "QRQ_bars"
)
for (mod in modules){
  source(
    paste0("code/",mod,".R")
  )
}

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
      gend == 2 ~ "Female"
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
  
qrq_regional <- read_dta(
  file.path(
    path2EU,
    "EU-S Data/eu-qrq/1. Data/eu_qrq_nuts_final.dta"
  )
) %>%
  mutate(level = "regional")

qrq_national <- read_dta(
  file.path(
    path2EU,
    "EU-S Data/eu-qrq/1. Data/eu_qrq_country_final.dta"
  )
) %>%
  mutate(level   = "national") %>%
  left_join(
    qrq_regional %>%
      group_by(country) %>%
      summarise(
        nuts = first(nuts)
      ) %>%
      mutate(
        nuts = substr(nuts, 1, 2)
      ),
    by = "country"
  )

master_data_qrq <- bind_rows(
  qrq_regional,
  qrq_national
)

# Loading outline
outline <- read.xlsx(
  file.path(
    path2EU,
    "EU-S Data/reports/eu-thematic-reports/data-viz/inputs/report_outline.xlsx"),
  sheet = "outline"
) %>%
  filter(type != "Box") # We don't need to produce data points or visualizations for boxes

# Loading map layers
base_map <- st_read(
  file.path(
    path2DA,
    "8. Data/EU-NUTS-GIS/EU_base_map_simp30.geojson"
  )
) %>%
  filter(!(polID %in% c("GL", "IS")))

insets <- getInsets(list(
  "Canaries/Madeira" = c("ES7", "PT3"),
  "Azores"           = "PT2",
  "Cyprus"           = "CY0"
))

map_layers <- c(
  list(
    "Main" = base_map %>% 
      filter(
        !polID %in% c("ES7", "PT3", "PT2", "CY0")
      )
  ), 
  insets
)

# Loading region names
region_names <- read.xlsx(
  "inputs/region_labels.xlsx"
) %>%
  select(country_name_ltn = country, 
         nuts_id, 
         nameSHORT, 
         pop_weight    = regionpoppct, 
         unique_border = border, 
         unique_label  = label)

# Loading region names
color_range <- read.xlsx(
  "inputs/color_range.xlsx"
)


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
        
        # Imputing low counts for these specific special charts
        # if (chart %in% c("R1F68", "R3F14")) {
        #   wrangled_data <- impute_values(wrangled_data)
        # }
        
        return(wrangled_data)
      }
    )
    
    if (source %in% c("GPP", "QRQ")) {
      
      wrangled_data <- bind_rows(wrangled_data_list)
      
      # Imputing low counts
      if (source %in% c("GPP")){
        wrangled_data <- impute_values(wrangled_data)
      }
      
      # Getting country+EU averages
      wrangled_data <- getAvgData(
        wrangled_data,
        source = source
      )
    
      
      # Saving data for website
      save4web(
        wrangled_data %>%
          filter(!chart_id %in% c("R1B1", "R2B1", "R3B1")), # We can skip the data from these Boxes. They are repeated.
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

# Saving data for special charts
writexl::write_xlsx(
  data_points$Special,
  file.path(
    path2EU,
    "EU-S Data/reports/eu-thematic-reports/data-viz/output/special_datapoints.xlsx"
  )
)

# Saving info from imputed data
write_csv(
  data_points$GPP %>% 
    filter(count < 30 & count > 0),
  file.path(
    path2EU,
    "EU-S Data/reports/eu-thematic-reports/data-viz/output/imputedDP_full.csv"
  )
)
write_csv(
  data_points$GPP %>% 
    filter(demographic == "Total Sample" & count < 30 & count > 0),
  file.path(
    path2EU,
    "EU-S Data/reports/eu-thematic-reports/data-viz/output/imputedDP_report.csv"
  )
)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Create Viz                                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Calling the visualizer for each chart
charts <- lapply(
  outline %>%
    filter(thematic_reports == T) %>%
    pull(chart_id),
  callVisualizer
)
