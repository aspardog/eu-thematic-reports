# STEP ONE. DEFINE THE VARIABLES OF INTEREST
voi <- outline %>%
  filter(
    thematic_reports == T & special_wrangling == F & description == "GPP" & chapter != "Access to Justice" & section != "Control of Violence"
  ) %>%
  distinct(target_var_1) %>%
  pull(target_var_1)



# STEP TWO. DEFINE AND IMPLEMENT TRANSFORMATIONS
topics <- outline %>%
  select(target_var_1, topic) %>%
  distinct() %>%
  deframe()

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
base_variables <- c("country_name_ltn", "nuts_id")
data4tests <- master_data_gpp %>%
  select(
    all_of(base_variables), 
    all_of(voi)
  ) %>%
  mutate(
    # Applying Transformation function
    across(
      voi, 
      ~ case_when(
        
        # First transformation function
        topics[cur_column()] %in% c(
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
        topics[cur_column()] %in% c("Corruption Change") ~ trfunc2(.x),
        
        # Third transformation function
        topics[cur_column()] %in% c("Corruption Perceptions") ~ trfunc3(.x),
        
        # Fourth transformation function
        topics[cur_column()] %in% c(
          "Security Violence",
          "Bribe Victimization",
          "Discrimination",
          "Civic Participation A Civic Participation B"
        ) ~ trfunc4(.x),
        
        # Fifth transformation function
        topics[cur_column()] %in% c("Attitudes towards corruption") ~ trfunc5(.x),
        
        # Sixth transformation function
        topics[cur_column()] %in% c("Transformed") ~ trfunc6(.x)
        
      )
    )
  )

    


# STEP FOUR. CALL VARIANCE FUNCTION
variance_summary <- diffvariance(data4tests)


# paste results together

# manually add eu variance for each indicator