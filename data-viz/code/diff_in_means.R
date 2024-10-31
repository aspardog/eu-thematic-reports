
groupings4analysis <- c("female", "urban", "young", "fintight")

capitals <- c(
  "AT1","BE1","BG4","HR05","CY0","CZ01","DK01","EE0","FI1B","FR1",
  "DE3","EL3","HU1","IE06","ITI","LV00","LT01","LU00","MT00","NL3",
  "PL9","PT1","RO3","SK01","SI04","ES3","SE1"
)

# Defining variables of study
vars <- outline %>%
  filter(
    thematic_reports == T & special_wrangling == F & description == "GPP" & chapter != "Access to Justice" & section != "Control of violence" 
  ) %>%
  distinct(target_var_1) %>%
  pull(target_var_1)

# Gathering topics
topic_lookup <- outline %>%
  select(target_var_1, topic) %>%
  distinct() %>%
  deframe()

# Defining transformation functions
trfunc1 <- function(value) {
  case_when(
    value <= 2  ~ 1,
    value <= 4  ~ 0,
    value == 98 ~ 0
  )
}
trfunc2 <- function(value) {
  case_when(
    value <= 2  ~ 1,
    value <= 5  ~ 0,
    value == 98 ~ 0
  )
}
trfunc3 <- function(value) {
  case_when(
    value <= 2  ~ 0,
    value <= 4  ~ 1,
    value == 98 ~ 0
  )
}
trfunc4 <- function(value) {
  case_when(
    value == 1  ~ 1,
    value == 2  ~ 0,
    value == 98 ~ 0
  )
}
trfunc5 <- function(value){
  case_when(
    value <= 3 ~ 0,
    value == 4 ~ 1,
    value == 98 ~ 0
  )
}
trfunc6 <- function(value){
  return(value)
}

# Sub-setting data and applying transformation functions
base_variables <- c("country_name_ltn", "nuts_id", "age_groups", "gender", "iq_groups", "urban_string", "edu", "fin")
data4tests <- master_data_gpp %>%
  select(
    all_of(base_variables), 
    all_of(vars)
  ) %>%
  mutate(
    # Applying Transformation function
    across(
      vars, 
      ~ case_when(
        
        # First transformation function
        topic_lookup[cur_column()] %in% c(
          "Trust", 
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
          "Citizen Perceptions"
        ) ~ trfunc1(.x),
        
        # Second transformation function
        topic_lookup[cur_column()] %in% c("Corruption Change") ~ trfunc2(.x),
        
        # Third transformation function
        topic_lookup[cur_column()] %in% c("Corruption Perceptions") ~ trfunc3(.x),
        
        # Fourth transformation function
        topic_lookup[cur_column()] %in% c(
          "Security Violence",
          "Bribe Victimization",
          "Discrimination",
          "Civic Participation A Civic Participation B"
        ) ~ trfunc4(.x),
        
        # Fifth transformation function
        topic_lookup[cur_column()] %in% c("Attitudes towards corruption") ~ trfunc5(.x),
        
        # Sixth transformation function
        topic_lookup[cur_column()] %in% c("Transformed") ~ trfunc6(.x)

      )
    ),
    
    female = case_when(
      gender == "Female" ~ 1,
      gender == "Male"   ~ 0
    ),
    urban = case_when(
      urban_string == "Urban" ~ 1,
      urban_string == "Rural" ~ 0
    ),
    low_income = case_when(
      iq_groups %in% c("Income Quintile 1", "Income Quintile 2") ~ 1,
      iq_groups %in% c("Income Quintile 3", "Income Quintile 4", "Income Quintile 5") ~ 0
    ),
    young = case_when(
      age_groups %in% c("18-24", "25-34") ~ 1,
      age_groups %in% c("35-44", "45-54", "55-64", "+65") ~ 0
    ),
    highschool = case_when(
      edu < 4  ~ 1,
      edu < 98 ~ 0
    ),
    fintight = case_when(
      fin < 3  ~ 1,
      fin < 99 ~ 0
    ),
    area = "EU"
  )

# Generating complete roaster of results
results_eu      <- diffmeans(data4tests, vars, groupings4analysis, "area")
results_country <- diffmeans(data4tests, vars, groupings4analysis, "country_name_ltn", collapse = F)
results_region  <- diffmeans(data4tests, vars, groupings4analysis, "nuts_id", collapse = F)

# Summary and saving data
summary <- lapply(
  c(
    "EU"      = "EU",
    "country" = "country",
    "region"  = "region"
  ),
  function(level){
    
    if(level == "EU"){
      lapply(
        groupings4analysis %>% set_names(groupings4analysis),
        function(macrogrouping){
          results_eu[[macrogrouping]] %>%
            right_join(
              outline %>%
                select(target_var_1, chapter, section),
              by = c("variable" = "target_var_1")
            ) %>%
            filter(
              !is.na(geovar)
            )
        }
      )

    } else {
      
      if (level == "country"){
        data = results_country
      }
      if (level == "region"){
        data = results_region
      }
      
      # Generating summary
      map_dfc(
        groupings4analysis %>% set_names(groupings4analysis),
        function(macrogroup){
          
          new_names <- c("variable" = macrogroup, 
                         "chapter"  = paste0("chapter_", macrogroup),
                         "section"  = paste0("section_", macrogroup),
                         "nsigs"    = paste0("nsigs_", macrogroup),
                         "avgdiff"  = paste0("avgdiff_", macrogroup),
                         "maxdiff"  = paste0("maxdiff_", macrogroup))
          
          map_dfr(
            vars,
            function(variable){
              
              chapter <- outline %>%
                filter(
                  target_var_1 == variable
                ) %>%
                distinct(chapter) %>%
                pull(chapter)
              
              section <- outline %>%
                filter(
                  target_var_1 == variable
                ) %>%
                distinct(section) %>%
                pull(section)
              
              data[[macrogroup]][[variable]] %>% 
                ungroup() %>% 
                summarise(
                  variable = variable,
                  chapter  = chapter,
                  section  = section,
                  nsigs    = sum(stat_sig),
                  avgdiff  = mean(diff, na.rm = T)*100,
                  maxdiff  = max(abs(diff), na.rm = T)*100
                )
            }
          ) %>%
            arrange(desc(nsigs)) %>%
            rename(
              !!!set_names(names(new_names), new_names)
            )
        }
      )
    }
  }
)

topic_summary <- lapply(
  c("country" = "country", "region" = "region"),
  function(level){
    
    map_dfc(
      groupings4analysis %>% set_names(groupings4analysis),
      function(macrogrouping){
        
        new_names <- c("grcol"  = macrogrouping, 
                       "nsigs"  = paste0("nsigs_", macrogrouping))
        
        grcol  <- paste0("section_", macrogrouping)
        valcol <- paste0("nsigs_", macrogrouping)
        
        if (level == "country"){
          total = 27
        } else {
          total = 110
        }
        
        summary[[level]] %>%
          select(
            grcol  = all_of(grcol),
            valcol = all_of(valcol)
          ) %>%
          group_by(grcol) %>%
          mutate(
            valcol = valcol/total
          ) %>%
          summarise(
            nsigs = mean(valcol)
          ) %>%
          arrange(desc(nsigs)) %>%
          rename(
            !!!set_names(names(new_names), new_names)
          )
      }
    )
    
  })

# Visualization - Differential Bars
results_country[["young"]][["discrimination1"]] %>%
  select(geovar, mean_A, mean_B, diff) %>%
  mutate(
    status = if_else(diff < 0, "Negative Diff.", "Positive Diff."),
    across(
      c(mean_A, mean_B),
      ~.x*100
    )
  ) %>%
  # pivot_longer(!geovar) %>%
  ggplot() +
  geom_segment(
    aes(
      x = reorder(geovar, mean_A),
      y = mean_A,
      yend = mean_B,
      colour = status
    ),
    linewidth = 4
  ) + 
  scale_colour_manual(
    "",
    values = c("Negative Diff." = "#374B4A",
               "Positive Diff." = "#69A2B0")
  ) +
  labs(
    title = "Young: Experiences of discrimination"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    # panel.grid.major.y = element_blank()
  )

# Visualization - Scatterplot
results_region[["urban"]][["SEC_walking"]] %>%
  select(geovar, mean_A, mean_B, diff) %>%
  mutate(
    status = if_else(diff < 0, "Negative Diff.", "Positive Diff."),
    across(
      c(mean_A, mean_B),
      ~.x*100
    )
  ) %>%
  # pivot_longer(!geovar) %>%
  ggplot() +
  geom_point(
    aes(
      x = mean_A,
      y = mean_B,
      colour = status
    ),
    size = 3
  ) + 
  scale_colour_manual(
    "",
    values = c("Negative Diff." = "#374B4A",
               "Positive Diff." = "#69A2B0")
  ) +
  # scale_x_continuous(
  #   limits = c(0,100)
  # ) +
  # scale_y_continuous(
  #   limits = c(0,100)
  # ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    # axis.title.x = element_blank(),
    # axis.title.y = element_blank(),
  )
  


