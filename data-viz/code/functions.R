## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Thematic Report - Functions
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
## 1.  General functions                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

resetDirs <- function(){
  
  # List and reset previous outputs
  prevOutputs <- list.files(
    "outputs", 
    include.dirs = F, 
    full.names   = T, 
    recursive    = T
  )
  file.remove(prevOutputs)
}

# 
# saveIT <- function(chart, n, w, h) {
#   ggsave(
#     plot   = chart,
#     file   = file.path(path2EU,
#                        "EU-S Data/reports/eu-thematic-reports/data-viz/outputs", paste0("chart_", n, ".svg"),
#                        fsep = "/"
#     ), 
#     width  = w, 
#     height = h,
#     units  = "mm",
#     dpi    = 72,
#     device = "svg"
#   )
# } 

getInsets <- function(targets){
  lapply(targets, 
         function(pol){
           base_map %>% filter(polID %in% pol)
         })
}

# callVisualizer <- function(chart_n) {
#   
#   type <- outline %>%
#     filter(n == chart_n) %>%
#     slice(1) %>%
#     pull(type) 
#   
#   data4chart <- data_points[[paste("Chart", chart_n)]]
#   
#   if (type == "Map"){
#     chart <- genMap(data4chart)
#   }
#   if (type == "Scatterplot"){
#     chart <- genScatterplot(data4chart)
#   }
#   if (type == "Dumbell"){
#     chart <- genDumbell(data4chart)
#   }
#   
#   saveIT(
#     chart = chart, 
#     n = chart_n, 
#     w = 189.7883, 
#     h = 168.7007
#   )
#   
#   return(chart)
#   
# }

getAvgData <- function(){
  
  data_wght <- data_points_df_regional %>%
    left_join(region_names) %>%
    mutate(
      weighted_value = value2plot*pop_weight,
      level = "regional"
    )
  
  country_avg <- data_wght %>%
    group_by(country_name_ltn, chart) %>%
    summarise(
      nuts_id    = first(nuts_id),
      value2plot = sum(weighted_value, na.rm = T) 
    ) %>%
    mutate(
      nuts_id   = substr(nuts_id, 1, 2),
      nameSHORT = country_name_ltn,
      level     = "national",
      weighted_value = value2plot  # Simple average for the European Union value. No special weights.
    )
  
  eu_avg <- country_avg %>%
    group_by(chart) %>%
    summarise(
      value2plot       = mean(weighted_value, na.rm = T),
      country_name_ltn = "European Union",
      nuts_id          = "EU",
      nameSHORT        = "European Union",
      level            = "eu"
    )
  
  data_out <- data_wght %>%
    select(country_name_ltn, level, nuts_id, nameSHORT, chart, value2plot) %>%
    bind_rows(
      country_avg %>% select(-weighted_value), 
      eu_avg
    )
  
  return(data_out)
  
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Wrangling function                                                                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

wrangleData <- function(chart_n, data_source){ # pass gpp or qrq as arg
  
  master_data <- if (data_source == 'gpp') master_data_gpp else master_data_qrq
  
  # Getting variable info
  id <- outline %>%
    filter(n == chart_n) %>%
    pull(id_var1)
  
  topic <- outline %>%
    filter(n == chart_n) %>%
    slice(1) %>%
    pull(topic)
  
  trfunc <- NULL

  # Defining a transforming function -- only applied to gpp
  if (data_source == 'gpp'){
  if (topic %in% c("Justice System Evaluation",
                   "Law Enforcement Performance",
                   "Criminal Justice Performance",
                   "Security",
                   "Trust"
                   )) 
    {
    
    trfunc <- function(value) {
      case_when(
        value <= 2  ~ 1,
        value <= 4  ~ 0,
        value == 98 ~ 0
      )
    }
  }

  
  if (topic %in% c("Corruption Perceptions")) {
    trfunc <- function(value) {
      case_when(
        value <= 2  ~ 0,
        value <= 4  ~ 1,
        value == 98 ~ 0
      )
    }
  }
  if (topic %in% c("Problem Selection", # % yes to >= 1 question
                   "Problem Resolution", 
                   "Problem Description", 
                   "Problem Evaluation",
                   "Demographics")){
    
    if (id == "AJP_*_bin"){
      
      
    }
    
  }
  
  
  # Getting the data to plot
  
  # create demographics
  master_data <- master_data %>%
    mutate(
      age_bin = case_when(
        age >= 18 & age <= 24 ~ "18-24",
        age >= 25 & age <= 34 ~ "25-34",
        age >= 35 & age <= 44 ~ "35-44",
        age >= 45 & age <= 54 ~ "45-54",
        age >= 55 & age <= 64 ~ "55-64",
        age >= 65 ~ "65+"
      ),
      gender_text = case_when(
        gend == 1 ~ "Male",
        gend == 2 ~ "Female"
      ),
      urban_text = if_else(urban == 1, "Urban", "Rural"),
      income_q = case_when(
        income_quintile == 1 ~ "Income Quintile 1",
        income_quintile == 2 ~ "Income Quintile 2",
        income_quintile == 3 ~ "Income Quintile 3",
        income_quintile == 4 ~ "Income Quintile 4",
        income_quintile == 5 ~ "Income Quintile 5"
      )
    )
  }
  
  
  
  # list of grouping variables (including dem categories)
  grouping_vars_gpp <- list(
    "Total"   = c("country_name_ltn", "nuts_id"),
    "Income"  = c("country_name_ltn", "nuts_id", "income_q"),
    "Gender"  = c("country_name_ltn", "nuts_id", "gender_text"),
    "Age Bin" = c("country_name_ltn", "nuts_id", "age_bin"),
    "Urban"   = c("country_name_ltn", "nuts_id", "urban_text")
  )
  
  grouping_vars_qrq <- list(
    "Total"   = c("country_name_ltn", "nuts_id")
  )
  
  grouping_vars <- if (data_source == 'gpp') grouping_vars_gpp else grouping_vars_qrq
  
  
  data2plot_list <- imap(grouping_vars, function(vars, demograph) {
    data2plot <- master_data %>%
      select(all_of(vars), target = all_of(id)) %>%
      {if (!is.null(trfunc)) mutate(., across(target, ~trfunc(.x))) else .} %>%
      group_by(across(all_of(vars))) %>%
      summarise(
        value2plot = mean(target, na.rm = T),
        .groups = "keep") %>%
      mutate(demographic = ifelse(demograph == "Total", "Total", as.character(get(vars[3])))) #make this the value of element3
    
    return(data2plot)
  })
  
  
  if (data_source == 'gpp'){
  combined_data2plot <- bind_rows(data2plot_list) %>%
    select(-income_q, -gender_text, -age_bin, -urban_text)}
  
  combined_data2plot <- bind_rows(data2plot_list)
  
  
  return(combined_data2plot)
  
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Special Wrangling functions (Access to Justice, Scatterplots)                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++