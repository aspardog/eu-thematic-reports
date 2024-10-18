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

 
saveIT <- function(chart, static = F, figid, w, h) {
  if (static == FALSE){
   ggsave(
     plot   = chart,
     file   = 
       file.path(
       path2EU,
       paste0(
         "EU-S Data/reports/eu-thematic-reports/data-viz/output/charts/",
         str_match(figid, "R[^F]"), "/",
         figid, ".svg"
       ),
       fsep = "/"
     ),
     width  = w, 
     height = h,
     units  = "mm",
     dpi    = 72,
     device = "svg"
   )
  }
  else{
    ggsave(
      plot   = chart,
      file   = 
        file.path(
          path2EU,
          paste0(
            "EU-S Data/reports/eu-thematic-reports/final-charts/no-tooltip/",
            str_match(figid, "R[^F]"), "/",
            figid, ".svg"
          ),
          fsep = "/"
        ),
      width  = w, 
      height = h,
      units  = "mm",
      dpi    = 72,
      device = "svg"
    )
  }
 } 

getInsets <- function(targets){
  lapply(targets, 
         function(pol){
           base_map %>% filter(polID %in% pol)
         })
}


callVisualizer <- function(figid) {
  
  print(glue("Working on Chart {figid}"))
  
  # Retrieve parameters from outline
  
  params <- outline %>%
    filter(chart_id == figid) %>%
    select(description, level, demographic, type, direction) %>%
    as.list()
  
  # Define demographic options
  demographic_options <- list(
    "Total"  = c("Total Sample"),
    "Gender" = c("Female", "Male")
  )
  
  # Special legends for Dot Charts
  if (figid %in% c("R1F41", "R1F82")) {
    params[["legend_labels"]] <- c("Congress/Parliament", "Local Government")
  }
  if (figid %in% c("R1F80", "R3F31")) {
    params[["legend_labels"]] <- c("Budget Figures", "Government Contracts")
  }
  # Special legends for Scatterplots
  if (figid %in% c("R1F79")) {
    params[["legend_labels"]] <- c("Easy to find online", "Easy to find offline")
  }
  if (figid %in% c("R2F31", "R2F35", "R2F39", "R2F48")) {
    params[["legend_labels"]] <- c("Trust", "Perceptions of Corruption")
  }
  

  if (figid %in% chart_list$Special) {
    data4chart <- data_points[["Special"]][[figid]]
  } else {
    data4chart <- data_points[[params$description]] %>%
      filter(chart_id == figid & !is.na(value2plot))
  }

  if (params$type == "Map") {
    chart <- genMap(data4chart, direction = params[["direction"]])
    chart_static <- genMap(data4chart, direction = params[["direction"]], static = TRUE)
  }
  if (params$type == "Dumbbells") {
    chart <- genDumbbells(data4chart)
    chart_static <- genDumbbells(data4chart, static = TRUE)
  }
  if (params$type == "QRQ Bars"){
    chart <- gen_qrq_bars(data4chart, direction = params[["direction"]])
    chart_static <- gen_qrq_bars(data4chart, direction = params[["direction"]], static = TRUE)
  }
  if (params$type == "Table") {
    chart <- genTable(data4chart)
    chart_static <- genTable(data4chart)
  }
  if (params$type == "Map (Categorical)") {
    if (figid == "R3F14"){
      chart <- gen_catMap(data4chart, break_legend = T)
      chart_static <- gen_catMap(data4chart, break_legend = T, static = TRUE)
    } else{
      chart <- gen_catMap(data4chart)
      chart_static <- gen_catMap(data4chart, static = TRUE)
    }
  }
  if (params$type == "Scatterplot") {
    chart <- scatterPlot(data4chart, legend = params[["legend_labels"]])
    chart_static <- scatterPlot(data4chart, legend = params[["legend_labels"]], static = TRUE)
  }
  if (params$type == "Dots") {
    chart <- gen_dots(data4chart, legend = params[["legend_labels"]])
    chart_static <- gen_dots(data4chart, legend = params[["legend_labels"]], static = TRUE)
    
  }
  if (params$type == "Bars") {
    chart <- gen_bars(data4chart, direction = params[["direction"]])
    chart_static <- gen_bars(data4chart, direction = params[["direction"]], static = TRUE)
  }
  if (params$type == "Lollipop") {
    chart <- genLollipop(data4chart)
    chart_static <- genLollipop(data4chart, static = T)
  }
  
  
  # Check if chart is still NULL, indicating an unsupported type
  if (is.null(chart)) {
    stop("Unsupported chart type")
  }
  
  # Special height for lollipops
  if (params$type == "Lollipop") {
    h = 192.8467
  } else {
    h = 168.7007
  }
  
  if (params$type == "Table"){
    h = 194
  }
  
  
  # Save chart locally
  saveIT(
    chart = chart,
    figid = figid,
    w = 189.7883,
    h = h
  )
  
  # Save static charts
  saveIT(
    chart = chart_static,
    static = T,
    figid = figid,
    w = 189.7883,
    h = h
  )
  
  
  return(
    list(
      "plot" = chart,
      "data" = data4chart
    )
  )
}


getAvgData <- function(data, source){
  
  if (source %in% c("GPP")) {
    data_wght <- data %>%
      left_join(
        region_names,
        by = c("country_name_ltn", "nuts_id")
      ) %>%
      mutate(
        level = "regional"
      )
    
    data_wght_filtered <- data_wght %>% 
      filter(!is.na(value2plot)) %>% 
      group_by(country_name_ltn, chart_id, demographic) %>%
      mutate(
        total_pop_weight = sum(pop_weight, na.rm = T),
        reweighted = pop_weight / total_pop_weight  # Adjusting pop weight for the remaining regions
      ) %>%
      ungroup() %>%
      mutate(
        weighted_value = value2plot * reweighted,
        level          = "regional"
        )
    
    country_avg <- data_wght_filtered %>%
      group_by(country_name_ltn, chart_id, demographic) %>%
      summarise(
        nuts_id        = first(nuts_id),
        value2plot     = sum(weighted_value, na.rm = T),
        target_var     = first(target_var),
        count          = sum(count, na.rm = T),
        .groups        = "keep"
      ) %>%
      mutate(
        nuts_id        = substr(nuts_id, 1, 2),
        nameSHORT      = country_name_ltn,
        level          = "national",
        weighted_value = value2plot
      )
  }
  
  if (source %in% c("QRQ")) {
    
    data_wght <- data %>%
      filter(
        level == "regional"
      ) %>%
      mutate(
        weighted_value = NA_real_
      ) %>%
      left_join(
        region_names,
        by = c("country_name_ltn", "nuts_id")
      )
    
    country_avg <- data %>%
      filter(
        level == "national"
      ) %>%
      mutate(
        nameSHORT      = country_name_ltn,
        weighted_value = value2plot
      )

  }
  
  eu_avg <- country_avg %>%
    group_by(chart_id, demographic) %>%
    summarise(
      value2plot       = mean(weighted_value, na.rm = T),
      country_name_ltn = "European Union",
      nuts_id          = "EU",
      nameSHORT        = "European Union",
      level            = "eu",
      target_var       = first(target_var),
      count            = sum(count, na.rm = T),
      .groups          = "keep"
    )
  
  data_out <- data_wght %>% # use original df so we don't lose the imputed values
    select(country_name_ltn, level, nuts_id, nameSHORT, chart_id, target_var, demographic, value2plot, count) %>%
    bind_rows(
      country_avg %>% select(-weighted_value), 
      eu_avg
    )

  return(data_out)
}


impute_values <- function(data) {
  
  data <- data %>%
    mutate(
      low_count = if_else(
        count < 30, TRUE, FALSE,
        missing = FALSE
      ),
      
      value2plot = if_else(
        low_count, NA, value2plot
      )
    )
  
  return(data %>% select(-low_count))
}


save4web <- function(data, source){
  
  # Matching agreed data structure
  if (source == "GPP"){
    data4web <- data %>%
      select(
        chart_id,
        country  = country_name_ltn, 
        level, 
        nuts_ltn = nameSHORT,
        nuts_id,
        id       = target_var,
        demographic,
        value    = value2plot
      ) %>%
      left_join(
        outline %>%
          select(
            chart_id,
            direction,
            question,
            chapter    = report,
            section    = chapter,
            subsection = section,
            title,
            subtitle
          ),
        by = "chart_id"
      ) %>%
      
      select(
        country, level, nuts_ltn, nuts_id, id, chapter, section, subsection, question, 
        demographic, title, subtitle, value, direction
      ) %>%
      mutate(
        section = str_remove_all(section, "Chapter .+\\. "), 
      )
  }
  
  if (source == "QRQ"){
    data4web <- data %>%
      select(
        chart_id,
        country   = country_name_ltn, 
        level, 
        nuts_ltn  = nameSHORT,
        nuts_id,
        indicator = target_var,
        score     = value2plot
      ) %>%
      left_join(
        outline %>%
          select(
            chart_id,
            theme          = report,
            pillar_id      = pillar,
            pillar_name    = chapter,
            subpillar_name = title
          ),
        by = "chart_id"
      ) %>%
      mutate(
        pillar      = pillar_id,
        pillar_name = str_remove_all(pillar_name, "Chapter .+\\. "), 
        subpillar   = str_replace_all(indicator, "p_", ""),
        subpillar   = str_replace_all(subpillar, "_", ".")
      ) %>%
      select(
        country, level, nuts_ltn, nuts_id, 
        theme, pillar, pillar_name, pillar_id, subpillar, subpillar_name,
        indicator, score
      ) 
    
    # using NEW TARGET VARS (this is what is stored in indicator field)
    qrq_imputation_indicators <- c("p_1_01", "p_1_02", "p_1_03", "p_1_04", "p_1_05", "p_1_06",
                                   "p_1_07", "p_1_08", "p_1_09", "p_1_10", "p_1_11", "p_1_12", 
                                   "p_2_2")
      
    # grab only the national values for the given indicators
    national_values <- data4web %>%
      filter(level == "national" & indicator %in% qrq_imputation_indicators) %>%
      select(country, indicator, national_score = score)
    
    # join intermediate column with data4web
    data4web <- data4web %>%
      left_join(national_values, by = c("country", "indicator")) %>% 
      mutate(
        score = ifelse(!is.na(national_score), national_score, score) # replace regional w/ nat
      ) %>%
      select(-national_score) # remove extra column
      
  }
  
  # Saving data locally
  write.csv(
    data4web,
    file = file.path(
      path2EU,
      "EU-S Data/reports/eu-thematic-reports/data-viz/output",
      paste0("data4web_", tolower(source), ".csv")
    ),
    fileEncoding = "UTF-8",
    row.names    = FALSE
  )
  # writexl::write_xlsx(
  #   write_csv(
  #     data4web,
  #     file.path(
  #       path2EU,
  #       "EU-S Data/reports/eu-thematic-reports/data-viz/output",
  #       paste0("data4web_", tolower(source), ".xlsx")
  #     )
  #   )
  # )
  
  print("data4web successfully saved in /outputs/ folder")
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Special Wrangling functions (Access to Justice, Bivariate)                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

addSpecial <- function(data){
  
  # Discrimination
  discrimination_vars <- c("DIS_sex", "DIS_age", "DIS_health", "DIS_ethni", "DIS_migration", "DIS_ses", 
                           "DIS_location", "DIS_religion", "DIS_family", "DIS_gender", "DIS_politics")
  
  discrimination <- data %>%
    mutate(
      across(
        all_of(discrimination_vars),
        ~case_when(
          .x == 1 ~ 1,
          .x >= 2 ~ 0 
        )
      )
    ) %>%
    select(
      country_year_id, all_of(discrimination_vars)
    ) %>%
    pivot_longer(
      !country_year_id,
      names_to  = "category",
      values_to = "value"
    ) %>%
    group_by(country_year_id) %>%
    summarise(
      discrimination1 = sum(value, na.rm = T)
    ) %>%
    mutate(
      discrimination1 = if_else(discrimination1 > 0, 1, 0)
    )
  
  # Legal Problem prevalence
  legalProblems <- c(
    "A1", "A2", "A3", "B1", "B2", "B3", "B4", "C1", "C2", "C3", "C4", "D1", "D2", "D3", "D4", "D5", "D6", "E1", 
    "E2", "E3", "F1", "F2", "G1", "G2", "G3", "H1", "H2", "H3", "I1", "J1", "J2", "J3", "J4", "K1", "K2", "K3", 
    "L1", "L2"
  )
  legprob_bin <- paste0("AJP_", legalProblems, "_bin")
  legprob_sev <- paste0("AJP_", legalProblems, "_sev")
  
  selec_sev <- data %>%  # Extracting severity of problem selected
    pivot_longer(
      !c(country_year_id, AJP_problem), 
      names_to      = c("set", ".value"), 
      names_pattern = "AJP_(.*)_(.*)"
    ) %>%
    mutate(
      sev = if_else(AJP_problem == set, sev, NA_real_)
    ) %>%
    group_by(country_year_id) %>%
    summarise(
      AJP_problem = first(AJP_problem),
      sev_problem_selected = sum(sev, na.rm = T)
    ) %>%
    mutate(
      sev_problem_selected = if_else(
        AJP_problem == "", 
        NA_real_, 
        sev_problem_selected 
      )
    ) %>%
    select(-AJP_problem)
  
  probPrev <- reduce(   # Estimating problem prevalence 
    list(
      
      # Data 1: incidence
      data %>%
        select(country_year_id, all_of(legprob_bin)) %>%
        pivot_longer(
          !country_year_id,
          names_to  = "problem",
          values_to = "answer"
        ) %>%
        mutate(
          problem = str_remove_all(problem, "AJP|_|bin")
        ),
      
      # Data 2: severity
      data %>%
        select(country_year_id, all_of(legprob_sev)) %>%
        pivot_longer(
          !country_year_id,
          names_to  = "problem",
          values_to = "severity"
        ) %>%
        mutate(
          problem = str_remove_all(problem, "AJP|_|sev")
        )
    ),
    left_join,
    by = "country_year_id"
  ) %>%
    mutate(
      prevalence1 = case_when(
        answer == 1 ~ 1,
        answer == 2 ~ 0
      ),
      prevalence2 = case_when(
        answer == 1 & severity >= 4 ~ 1,
        answer == 1 & severity  < 4 ~ 0,
        answer == 2 ~ 0
      )
    ) %>%
    group_by(country_year_id) %>%
    summarise(
      across(
        starts_with("prevalence"),
        \(x) sum(x, na.rm = T)
      )
    ) %>%
    mutate(
      across(
        starts_with("prevalence"),
        \(x) if_else(x > 0, 1, 0)
      )
    )
  
  # Other special wranglings
  agg_data <- data %>%
    left_join(
      selec_sev,
      by = "country_year_id"
    ) %>%
    mutate(
      
      # Legal vulnerability: official proof of identity
      vulnerability1 = case_when(
        A5_1 == 1  | A5_2 == 1  ~ 1,
        A5_1 == 99 & A5_2 == 99 ~ NA_real_,
        A5_1 >= 2  | A5_2 >= 2  ~ 0,
      ),
      
      # Legal vulnerability: official proof of housing or land tenure
      vulnerability2 = case_when(
        A5b == 1  ~ 1,
        A5b <= 98 ~ 0
      ),
      
      # Access to appropriate information and advice
      access2info = case_when(
        is.na(sev_problem_selected) ~ NA_real_,
        sev_problem_selected <=3    ~ NA_real_,
        AJE_infosource <= 2  ~ 1,
        AJE_infosource <= 98 ~ 0
      ),
      
      # Access to appropriate assistance and representation
      access2rep = case_when(
        is.na(sev_problem_selected) ~ NA_real_,
        sev_problem_selected <=3    ~ NA_real_,
        AJD_inst_advice == 1 & (
          AJD_adviser_2 == 1 | AJD_adviser_3 == 1 | AJD_adviser_4 == 1 | 
            AJD_adviser_5 == 1 | AJD_adviser_6 == 1 | AJD_adviser_8 == 1
        ) ~ 1,
        # AJD_inst_advice == 1 & AJD_adviser_1 == 1 & AJD_expert_adviser == 1 ~ 1, # Friend/Family with legal background
        AJD_inst_advice == 2 & (AJD_noadvice_reason %in% c(1,3)) ~ 1,  
        AJD_inst_advice == 2 | AJD_inst_advice == 98             ~ 0
      ),
      
      # Access to a dispute resolution mechanism
      access2drm = case_when(
        is.na(sev_problem_selected) ~ NA_real_,
        sev_problem_selected <=3    ~ NA_real_,
        AJR_resolution == 1 ~ 1,
        AJR_resolution == 2 & (AJR_noresol_reason %in% c(3,5,6,7,8)) ~ 0
        # AJR_resolution == 98 (We don't know if they really needed the DRM, so we exclude 98s)
      ),
      
      # Timeliness of the resolution process
      rp_time = case_when(
        AJR_state_resol    %in% c(1,2,98,99) ~ NA_real_,
        AJR_settle_noresol %in% c(1,2,98,99) ~ NA_real_,
        is.na(sev_problem_selected) ~ NA_real_,
        sev_problem_selected <=3    ~ NA_real_,
        AJR_solvingtime == -9999    ~ NA_real_,
        AJR_solvingtime == -8888    ~ 0,
        AJR_solvingtime >  12       ~ 0,
        AJR_solvingtime <= 12       ~ 1
      ),
      
      # Costliness of the resolution process
      rp_cost = case_when(
        AJR_state_resol    %in% c(1,2,98,99) ~ NA_real_,
        AJR_settle_noresol %in% c(1,2,98,99) ~ NA_real_,
        is.na(sev_problem_selected) ~ NA_real_,
        sev_problem_selected <=3    ~ NA_real_,
        AJR_solvingcosts == 2       ~ 1,
        AJR_solvingcosts == 1 & (AJR_costdiff %in% c(1,2)) ~ 1,
        AJR_solvingcosts == 1 & (AJR_costdiff %in% c(3,4,98)) ~ 0
      ),
      
      # Fairness of the resolution process
      rp_fair = case_when(
        AJR_state_resol    %in% c(1,2,98,99) ~ NA_real_,
        AJR_settle_noresol %in% c(1,2,98,99) ~ NA_real_,
        is.na(sev_problem_selected) ~ NA_real_,
        sev_problem_selected <=3    ~ NA_real_,
        AJR_fair == 1               ~ 1,
        AJR_fair %in% c(2,98)       ~ 0
      ),
      
      # Outcome of the resolution process
      rp_outcome = case_when(
        AJR_state_resol    %in% c(1,2,98,99) ~ NA_real_,
        AJR_settle_noresol %in% c(1,2,98,99) ~ NA_real_,
        is.na(sev_problem_selected) ~ NA_real_,
        sev_problem_selected <=3    ~ NA_real_,
        AJR_state_resol    == 3     ~ 0,
        AJR_state_resol    == 4     ~ 1,
        AJR_settle_noresol == 3     ~ 0,
        AJR_settle_noresol == 4     ~ 1
      ),
      
      # Police and Community Safety
      psafe1 = case_when(
        (LEP_safecom %in% c(1,2)) & (LEP_safefam %in% c(1,2)) & 
          (LEP_policehelp %in% c(1,2)) & (LEP_kindpol %in% c(1,2)) & 
          (LEP_polservcom %in% c(1,2)) ~ 1,
        (LEP_safecom %in% c(3,4,98)) & (LEP_safefam %in% c(3,4,98)) & 
          (LEP_policehelp %in% c(3,4,98)) & (LEP_kindpol %in% c(3,4,98)) & 
          (LEP_polservcom %in% c(3,4,98)) ~ 0,
        is.na(LEP_safecom) & is.na(LEP_safefam) & is.na(LEP_policehelp) &
          is.na(LEP_kindpol) & is.na(LEP_polservcom) ~ NA_real_
      ),
      
      # Government Services and Bribes
      bribery1 = case_when(
        (
          BRB_permit_B == 1 | BRB_benefits_B == 1 | BRB_id_B == 1 |
            BRB_school_B == 1 | BRB_health_B == 1
        ) ~ 1,
        (
          BRB_permit_B == 2 | BRB_benefits_B == 2 | BRB_id_B == 2 |
            BRB_school_B == 2 | BRB_health_B == 2
        ) ~ 0,
        (
          BRB_permit_B == 98 | BRB_benefits_B == 98 | BRB_id_B == 98 |
            BRB_school_B == 98 | BRB_health_B == 98
        ) ~ 0,
      )
    ) %>%
    select(
      country_year_id, 
      vulnerability1, vulnerability2, 
      access2info, access2rep, access2drm,
      rp_time, rp_cost, rp_fair, rp_outcome,
      psafe1, bribery1
    )
  
  # Listing individual data
  specialData <- reduce(
    list(
      discrimination,
      probPrev,
      agg_data
    ),
    left_join,
    by = "country_year_id"
  )
  
  return(specialData)
}

wrangleBivariate <- function(figid){
  
  # Getting variable info
  id <- c(
    "id_1" = outline %>%
      filter(chart_id %in% figid) %>%
      pull(target_var_1),
    "id_2" = outline %>%
      filter(chart_id %in% figid) %>%
      pull(target_var_2)
  )
  
  # Wrangling data
  data_wide <- lapply(
    id,
    function(tvar){
      
      # Defining a transformation function
      if ( str_detect(tvar, "CPA_|IPR_|IRE_|TRT_") ){
        trfunc <- function(value) {
          case_when(
            value <= 2  ~ 1,
            value <= 4  ~ 0,
            value == 98 ~ 0
          )
        } 
      }
      if ( str_detect(tvar, "COR_") ){
        trfunc <- function(value) {
          case_when(
            value <= 2  ~ 0,
            value <= 4  ~ 1,
            value == 98 ~ 0
          )
        } 
      }
      
      # Sub-setting data
      data_subset <- master_data_gpp %>%
        select(
          country_name_ltn, nuts_id,
          target = all_of(tvar)
        ) %>%
        mutate(
          
          # Recording chart ID
          chart_id = figid,
          
          # Applying Transformation function (trfunc)
          across(
            target, 
            ~trfunc(.x)
          ),
          
          # Creating an anchor variable for grouping
          demographic = "Total Sample"
          
        ) %>%
        group_by(
          country_name_ltn, nuts_id, chart_id, demographic
        ) %>%
        summarise(
          value2plot = mean(target, na.rm = T),
          count = n(),
          .groups = "keep"
        )
      
      return(data_subset)
      
    }
  ) %>%
    reduce(
      left_join, 
      by = c("country_name_ltn", "nuts_id", "demographic", "chart_id", "count"), 
      suffix = c("_id1", "_id2")
    )
  
  return(data_wide)
  
}


wrangleMostFrequent <- function(figid) {
  
  # Defining target variables
  discrimination_reason_vars <- c("DIS_sex", "DIS_age", "DIS_health", "DIS_ethni", "DIS_migration", 
                                  "DIS_ses", "DIS_location", "DIS_religion", "DIS_family", 
                                  "DIS_gender", "DIS_politics")
  
  discrimination_instance_vars <- c("DIS_exp_1", "DIS_exp_2", "DIS_exp_3", "DIS_exp_4", 
                                    "DIS_exp_5", "DIS_exp_6", "DIS_exp_7", "DIS_exp_8", 
                                    "DIS_exp_9", "DIS_exp_10", "DIS_exp_11", "DIS_exp_12")
  
  bribery_vars <- c("BRB_permit_B", "BRB_benefits_B", "BRB_id_B", "BRB_school_B", "BRB_health_B")   
  
  # Defining labels
  discrimination_full_names <- c(
    DIS_sex       = "Sex",
    DIS_age       = "Age",
    DIS_health    = "Disability or Health",
    DIS_ethni     = "Ethnicity, Skin Color or Language",
    DIS_migration = "Migration Status",
    DIS_ses       = "Socio-Economic Status",
    DIS_location  = "Geographic location",
    DIS_religion  = "Religion",
    DIS_family    = "Marital and Family Status",
    DIS_gender    = "Gender Identity",
    DIS_politics  = "Political Opinion"
  )
  
  bribery_full_names <- c(
    BRB_permit_B   = "Local Permits",
    BRB_benefits_B = "Public Benefits",
    BRB_id_B       = "Identity Documents",
    BRB_school_B   = "Public Education",
    BRB_health_B   = "Public Health"
  )
  
  discrimination_instance_names <- c(
    DIS_exp_1  = "Work-related",
    DIS_exp_2  = "Job Application",
    DIS_exp_3  = "Commercial Establishment",
    DIS_exp_4  = "Public Transport or Place",
    DIS_exp_5  = "Household",
    DIS_exp_6  = "Healthcare Services",
    DIS_exp_7  = "School",
    DIS_exp_8  = "Housing Search",
    DIS_exp_9  = "Police and Courts",
    DIS_exp_10 = "political Participation",
    DIS_exp_11 = "Social Media",
    DIS_exp_12 = "Miscellaneous"
  )
  
  # Defining parameters
  target <- outline %>%
    filter(chart_id %in% figid) %>%
    pull(target_var_1)
  
  # Sample size
  calculate_sample_size <- function(data, var) {
    data %>%
      group_by(nuts_id) %>%
      mutate(
        count = sum(get(var) %in% c(1, 2), na.rm = TRUE)
      ) %>%
      ungroup()
  }
  
  # Most common reason for discrimination
  if (target == "discrimination2"){
    
    master_data_gpp[discrimination_reason_vars] <- lapply(
      master_data_gpp[discrimination_reason_vars], 
      function(x) ifelse(x == 1, 1, 0)
    )
    
    discrimination_reason_summary <- master_data_gpp %>%
      group_by(nuts_id) %>%
      summarize(
        across(
          all_of(discrimination_reason_vars), 
          \(x) sum(x, na.rm = T)
        )
      ) %>%
      pivot_longer(
        -nuts_id, 
        names_to  = "discrimination_reason", 
        values_to = "count"
      ) %>%
      group_by(discrimination_reason) %>%
      summarize(
        total_count = sum(count, na.rm = TRUE)
      ) %>%
      arrange(desc(total_count)) %>%
      mutate(
        rank = row_number()
      ) %>%
      ungroup() %>%
      mutate(
        discrimination_reason = ifelse(rank > 4, "Other", discrimination_reason)
      ) %>%
      select(-rank) %>%
      group_by(discrimination_reason) %>%
      summarize(
        total_count = sum(total_count, na.rm = TRUE)
      )
    
    # Summarize discrimination reasons by nuts_id again for joining
    discrimination_by_nuts <- master_data_gpp %>%
      group_by(nuts_id) %>%
      summarize(
        across(
          all_of(discrimination_reason_vars), 
          \(x) sum(x, na.rm = T)
        )
      ) %>%
      pivot_longer(
        -nuts_id, 
        names_to  = "discrimination_reason", 
        values_to = "count"
      )
    
    # Join with the summarized discrimination reasons
    discrimination_reason_summary <- discrimination_reason_summary %>%
      left_join(discrimination_by_nuts, 
                by = "discrimination_reason") %>%
      group_by(nuts_id) %>%
      slice_max(order_by = count, n = 1) %>%
      ungroup() %>%
      rename(value2plot = discrimination_reason) %>%
      left_join(
        master_data_gpp %>% 
          select(nuts_id, country_name_ltn) %>% 
          distinct(), by = "nuts_id"
      ) %>%
      mutate(chartid = figid) %>%
      mutate(
        value2plot = case_when(
          value2plot %in% names(discrimination_full_names) ~ discrimination_full_names[value2plot],
          value2plot == "Other" ~ "Other"
        )
      )
    
    return(discrimination_reason_summary)
  }
  
  # Most common bribery situation
  if (target == "bribery2") {
    
    bribery <- c("BRB_permit_B")
    master_data_gpp <- calculate_sample_size(master_data_gpp, bribery)
    
    master_data_gpp[bribery_vars] <- lapply(
      master_data_gpp[bribery_vars], 
      function(x) ifelse(x == 1, 1, 0)
    )
    
    bribery_summary <- master_data_gpp %>%
      group_by(nuts_id) %>%
      summarize(
        across(all_of(bribery_vars), \(x) sum(x, na.rm = T)),
        count = first(count)
      ) %>%
      pivot_longer(
        -c(nuts_id, count), 
        names_to  = "bribery_instance", 
        values_to = "count_of_instance"
      ) %>%
      group_by(nuts_id) %>%
      slice_max(order_by = count_of_instance, n = 1) %>%
      ungroup() %>%
      rename(value2plot = bribery_instance) %>%
      left_join(master_data_gpp %>% 
                  select(nuts_id, country_name_ltn) %>% 
                  distinct(), by = "nuts_id") %>%
      mutate(chartid = figid) %>%
      mutate(value2plot = case_when(
        value2plot %in% names(bribery_full_names) ~ bribery_full_names[value2plot]
      ))
    
    return(bribery_summary)
  }
  
  # Most common instance of discrimination (discrimination3)
  if (target == "discrimination3") {
    
    DIS_var <- c("DIS_exp_2")
    master_data_gpp <- calculate_sample_size(master_data_gpp, DIS_var)
    
    master_data_gpp[discrimination_instance_vars] <- lapply(
      master_data_gpp[discrimination_instance_vars], 
      function(x) ifelse(x == 1, 1, 0)
    )
    
    overall_summary <- master_data_gpp %>%
      summarize(
        across(all_of(discrimination_instance_vars), 
               \(x) sum(x, na.rm = T))
      ) %>%
      pivot_longer(
        everything(), 
        names_to  = "discrimination_instance", 
        values_to = "count_of_instance"
      ) %>%
      arrange(desc(count_of_instance)) %>%
      slice_max(order_by = count_of_instance, n = 3) %>%
      pull(discrimination_instance) 
    
    discrimination_summary <- master_data_gpp %>%
      group_by(nuts_id) %>%
      summarize(
        across(
          all_of(discrimination_instance_vars), 
          \(x) sum(x, na.rm = T)
        ),
        count = first(count)
      ) %>%
      pivot_longer(
        -c(nuts_id, count), 
        names_to  = "discrimination_instance", 
        values_to = "count_disc"
      ) %>%
      group_by(nuts_id) %>%
      slice_max(order_by = count_disc, n = 1) %>%
      ungroup() %>%
      mutate(value2plot = ifelse(discrimination_instance %in% overall_summary, 
                                 discrimination_instance, "Other"),
             value2plot = case_when(
               value2plot %in% names(discrimination_instance_names) ~ discrimination_instance_names[value2plot],
               TRUE ~ "Other"
             )) %>%
      select(nuts_id, value2plot, count) %>%
      left_join(master_data_gpp %>% select(nuts_id, country_name_ltn) %>% distinct(), by = "nuts_id") %>%
      mutate(chartid = figid)
    
    return(discrimination_summary)
  }
}

wrangle_PrevalenceByCategory <- function(figid){
  legalProblems <- c(
    "A1", "A2", "A3", "B1", "B2", "B3", "B4", "C1", "C2", "C3", "C4", 
    "D1", "D2", "D3", "D4", "D5", "D6", "E1", "E2", "E3", "F1", "F2", 
    "G1", "G2", "G3", "H1", "H2", "H3", "I1", "J1", "J2", "J3", "J4", 
    "K1", "K2", "K3", "L1", "L2"
  )
  legprob_bin <- paste0("AJP_", legalProblems, "_bin")
  
  # categories for each legal problem
  legalProblemCategories <- c(
    rep("consumer", 3), rep("land", 4), rep("housing", 4), rep("family", 6),
    rep("education", 3), rep("injury", 2), rep("employment", 3), rep("public services", 3),
    rep("law enforcement", 1), rep("citizenship and ID", 4), rep("community", 3),
    rep("money and debt", 2)
  )
  names(legalProblemCategories) <- legprob_bin
  
  master_data_gpp <- master_data_gpp %>%
    mutate(
      across(all_of(legprob_bin),
             ~ ifelse(. == 1, 1, 0))
    )
  
  legal_problem_summary <- master_data_gpp %>%
    group_by(
      country_name_ltn) %>%
    summarise(
      across(all_of(legprob_bin),
             \(x) sum(x, na.rm = T)), 
      .groups = "drop") %>%
    pivot_longer(
      cols = all_of(legprob_bin), 
      names_to = "legal_problem", 
      values_to = "count") %>%
    mutate(
      category = legalProblemCategories[legal_problem]
    ) %>%
    group_by(
      country_name_ltn,
      category) %>%
    summarise(
      total_count = sum(count),
      .groups = "drop") %>%
    group_by(
      country_name_ltn) %>%
    mutate(total_incidents = sum(total_count)) %>%
    mutate(value2plot = (total_count / total_incidents) * 100) %>%
    ungroup() %>%
    select(country_name_ltn, category, total_count, total_incidents, value2plot) %>%
    mutate(chartid = figid)
  
  return(legal_problem_summary)
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Wrangling function                                                                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

wrangleData <- function(figid, source){
  
  if (source == "GPP"){
    
    # Getting variable info
    id <- outline %>%
      filter(chart_id %in% figid) %>%
      pull(target_var_1)
    
    topic <- outline %>%
      filter(chart_id %in% figid) %>%
      pull(topic)
    
    # Defining a transforming function
    if (topic %in% c("Trust", 
                     "Security", 
                     "Law Enforcement Performance", 
                     "Criminal Justice Performance",
                     "Perceptions on Authoritarian Behavior", 
                     "Justice System Evaluation",
                     "Civic Participation A",
                     "Civic Participation B",
                     "Opinions regarding Corruption",
                     "Information Provision",
                     "Information Requests",
                     "Citizen Perceptions")) {
      trfunc <- function(value) {
        case_when(
          value <= 2  ~ 1,
          value <= 4  ~ 0,
          value == 98 ~ 0
        )
      }
    }
    if (topic %in% c("Corruption Change")) {
      trfunc <- function(value) {
        case_when(
          value <= 2  ~ 1,
          value <= 5  ~ 0,
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
    if (topic %in% c("Security Violence",
                     "Bribe Victimization",
                     "Civic Participation A Civic Participation B",
                     "Discrimination")) {
      trfunc <- function(value) {
        case_when(
          value == 1  ~ 1,
          value == 2  ~ 0,
          value == 98 ~ 0
        )
      }
    }
    if (topic %in% c("Attitudes towards corruption")){
      trfunc <- function(value){
        case_when(
          value <= 3 ~ 0,
          value == 4 ~ 1,
          value == 98 ~ 0
        )
      }
    }
    if (topic %in% c("Transformed")){
      trfunc <- function(value){
        return(value)
      }
    }
    
    # Sub-setting data
    base_variables <- c("country_name_ltn", "nuts_id", "age_groups", "gender", "iq_groups", "urban_string")
    data_subset <- master_data_gpp %>%
      select(
        all_of(base_variables), 
        target = all_of(id)
      ) %>%
      mutate(
        
        # Applying Transformation function (trfunc)
        across(
          target, 
          ~trfunc(.x)
        ),
        
        # Creating an anchor variable for grouping
        total_anchor = "Total Sample"
      )
    
    # Getting extended (ALL) data points
    data_points_extended_list <- lapply(
      c("total_anchor", "age_groups", "gender", "iq_groups", "urban_string"), 
      function(gvar, demograph){
        
        data_subset %>%
          select(
            country_name_ltn, nuts_id, demographic = all_of(gvar), target
          ) %>%
          group_by(
            country_name_ltn, nuts_id, demographic
          ) %>% mutate(counter = if_else(!is.na(target), 1, 0)) %>%
          summarise(
            value2plot = mean(target, na.rm = T),
            count      = sum(counter, na.rm = T),
            .groups = "keep"
          )
      }
    )
    
    # Collapsing into a single data frame
    data_points_extended <- bind_rows(data_points_extended_list) %>%
      mutate(
        chart_id   = figid,
        target_var = id
      ) %>%
      filter(!is.na(demographic)) %>%
      filter(
        !(country_name_ltn == "Ireland" & target_var == "prevalence2")
      )
    
    return(data_points_extended)
  }
  
  if (source == "QRQ"){
    
    # Getting variable info
    id <- outline %>%
      filter(chart_id %in% figid) %>%
      pull(old_id_var)
    
    # Wrangling data
    data_subset <- master_data_qrq %>%
      rename(value2plot = all_of(id)) %>%
      select(country_name_ltn = country, nuts_id = nuts, value2plot, level) %>%
      mutate(
        demographic = "Total Sample",
        chart_id    = figid,
        target_var  = id,
        count       = n()
      )
    
    return(data_subset)
  }
  
  if (source == "Special"){

    # Getting chart info
    type <- outline %>%
      filter(chart_id %in% figid) %>%
      pull(type)

    # Special wrangling for bivariate charts
    if (type %in% c("Dots", "Scatterplot")){
      data2plot <- wrangleBivariate(figid = figid)
    }
    if (type %in% c("Map (Categorical)")){
      data2plot <- wrangleMostFrequent(figid = figid)
    }
    if (type %in% c("Table")){
      data2plot <- wrangle_PrevalenceByCategory(figid = figid)
    }
    
    if (figid %in% c("R2F20", "R3F31")){
      data2plot <- data2plot %>%
        filter(
          !(country_name_ltn == "Ireland")
        )
    }
    
    return(data2plot)
  }
}



