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
## Outline:  This file contains the definitions of several defining functions which are called in the upper
##           level of the EU Thematic Report project (RunMe.R). Here we define logic for saving generated 
##           outputs, call the appropriate visualization functions depending on the type of chart specified in
##           the report outline, apply appropriate weighting techniques depending on the spacial level of the
##           target data values, and apply wrangling techniques to ensure that data is in the correct form for 
##           the desired visualization. 
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

 
saveIT <- function(chart, n, w, h) {
   ggsave(
     plot   = chart,
     file   = file.path( #PLACEHOLDER UNTIL VIZ OUTPUT FINALIZED -- replace with local path
                        "C:\\Users\\icoddington\\Github\\eu-thematic-reports\\data-viz\\output", paste0("chart_", n, ".svg"),
                        fsep = "/"
     ), 
     width  = w, 
     height = h,
     units  = "mm",
     dpi    = 72,
     device = "svg"
   )
 } 

getInsets <- function(targets){
  lapply(targets, 
         function(pol){
           base_map %>% filter(polID %in% pol)
         })
}

 callVisualizer <- function(chart_n) {
   
   type <- outline %>%
     filter(n == chart_n) %>%
     slice(1) %>%
     pull(type) 
   
   data4chart <- data_points[[paste("Chart", chart_n)]]
   
   if (type == "map"){
     chart <- genMap(data4chart)
   }
   if (type == "scatterplot"){
     chart <- scatterPlot(data4chart)
   }
   if (type == "dumbell"){
     chart <- dumbellChart(data4chart)
   }
   
   saveIT(
     chart = chart, 
     n = chart_n, 
     w = 189.7883, 
     h = 168.7007
   )
   
   return(chart)
   
 }

getAvgData <- function(){
  
  data_wght <- data_points_df %>%
    left_join(region_names) %>%
    mutate(
      weighted_value = value2plot*pop_weight,
      weighted_id1 = value2plot_id1*pop_weight,
      weighted_id2 = value2plot_id2*pop_weight,
      level = "regional"
    )
  
  country_avg <- data_wght %>%
    group_by(country_name_ltn, chart) %>%
    summarise(
      nuts_id    = first(nuts_id),
      value2plot = sum(weighted_value, na.rm = T),
      value2plot_id1 = sum(weighted_id1, na.rm = T),
      value2plot_id2 = sum(weighted_id2, na.rm = T)
    ) %>%
    mutate(
      nuts_id   = substr(nuts_id, 1, 2),
      nameSHORT = country_name_ltn,
      level     = "national",
      weighted_value = value2plot,  # Simple average for the European Union value. No special weights.
      weighted_id1 = value2plot_id1,
      weighted_id2 = value2plot_id2
    )
  
  eu_avg <- country_avg %>%
    group_by(chart) %>%
    summarise(
      value2plot       = mean(weighted_value, na.rm = T),
      value2plot_id1   = mean(weighted_id1, na.rm = T),
      value2plot_id2   = mean(weighted_id2, na.rm = T),
      country_name_ltn = "European Union",
      nuts_id          = "EU",
      nameSHORT        = "European Union",
      level            = "eu"
    )
  
  data_out <- data_wght %>%
    select(country_name_ltn, level, nuts_id, nameSHORT, chart, value2plot, 
           value2plot_id1, value2plot_id2) %>%
    bind_rows(
      country_avg %>% select(-weighted_value, -weighted_id1, -weighted_id2), 
      eu_avg
    )
  
  return(data_out)
  
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Wrangling function                                                                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

wrangleData <- function(chart_n, data_source){ # pass GPP or QRQ as argument
  
  # determine if data comes from GPP or QRQ data frame
  master_data <- if (data_source == 'gpp') master_data_gpp else master_data_qrq
  
  # Getting variable info
  id <- outline %>%
    filter(n == chart_n) %>%
    pull(id_var1)
  
  topic <- outline %>%
    filter(n == chart_n) %>%
    slice(1) %>%
    pull(topic)
  
  # don't need trfunc unless data source is gpp
  trfunc <- NULL

  # Defining a transforming function
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

  
  if (topic %in% c("Corruption Perceptions")) { # leaving this in for now -- need for scatterplots
    trfunc <- function(value) {
      case_when(
        value <= 2  ~ 0,
        value <= 4  ~ 1,
        value == 98 ~ 0
      )
    }
  }
    
  # handling A2J
  if (topic %in% c("Problem Selection", # % yes to >= 1 question
                   "Problem Resolution", 
                   "Problem Description", 
                   "Problem Evaluation",
                   "Demographics")){
    
    if (id == "AJP_*_bin"){
      
      
    }
    
  }
  
  
  # create demographics -- also only for GPP
  master_data <- master_data %>%
    mutate(
      gender_text = case_when(
        gend == 1 ~ "Male",
        gend == 2 ~ "Female"
      ))
  } # end of if block
  
  
  # list of grouping variables -- dependent on GPP or QRQ
  grouping_vars_gpp <- list(
    "Total"   = c("country_name_ltn", "nuts_id")
    # "Gender"  = c("country_name_ltn", "nuts_id", "gender_text")
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
  
  
                          
  
  combined_data2plot <- bind_rows(data2plot_list)
  
  
  return(combined_data2plot)
  
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Special Wrangling functions (Access to Justice, Scatterplots)                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

wrangleData_scatter <- function(chart_n){
  
  # get ID1 -- TRT
  id1 <- outline %>% 
    filter(n == chart_n) %>%
    pull(id_var1)
  
  # get id2 -- COR
  id2 <- outline %>%
    filter(n == chart_n) %>%
    pull(id_var2)
  
  
  # define transformation functions
  trfunc_id1 <- function(value){
    case_when(
      value <= 2 ~ 1,
      value <= 4 ~ 0,
      value == 98 ~ 0
    )
  }
  
  trfunc_id2 <- function(value){
    case_when(
      value <= 2 ~ 0,
      value <= 4 ~ 1,
      value == 98 ~ 0
    )
  }
  
  
  # get data2plot -- similar to wrangleData, but keep two value2plot's
  data2plot <- master_data_gpp %>%
    select(country_name_ltn, nuts_id, all_of(id1), all_of(id2)) %>%
    mutate(
      target_TRT = trfunc_id1(.data[[id1]]),
      target_COR = trfunc_id2(.data[[id2]]),
      demographic = "Total"
    ) %>%
    group_by(country_name_ltn, nuts_id) %>%
    summarise(
      value2plot_id1 = mean(target_TRT, na.rm = TRUE),
      value2plot_id2 = mean(target_COR, na.rm = TRUE),
      .groups = "keep"
    )
  
  return(data2plot)
  
  }


# wrangleData_A2J <- function(chart_n){
# 
#   }
