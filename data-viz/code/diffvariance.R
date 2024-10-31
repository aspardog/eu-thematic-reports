
diffvariance <- function(df) {
  
  # SUMMARIZE TO NUTS LEVEL AND COUNTRY LEVEL
  data_nuts <- df %>%
    select(country_name_ltn, nuts_id, all_of(voi)) %>%
    group_by(country_name_ltn, nuts_id) %>%
    summarise(
      across(all_of(voi), ~ mean(.x, na.rm = TRUE)
      ))
  
  data_country <- data_nuts %>%
    left_join(region_names, by = c("country_name_ltn", "nuts_id")) %>%
    group_by(nuts_id) %>%
    mutate(
      total_pop_weight = sum(pop_weight, na.rm = TRUE),
      reweighted = pop_weight / total_pop_weight
    ) %>%
    ungroup() %>%
    # Calculate weighted values for each variable in `voi`
    mutate(across(all_of(voi), ~ .x * reweighted, .names = "weighted_{.col}")) %>%
    group_by(country_name_ltn) %>%
    summarise(
      nuts_id = first(nuts_id),
      across(starts_with("weighted_"), mean, na.rm = TRUE, .names = "{.col}"),
      .groups = "keep"
    ) %>%
    # Rename weighted columns to match `voi` names and select only necessary columns
    rename_with(~ sub("weighted_", "", .), starts_with("weighted_")) %>%
    select(country_name_ltn, nuts_id, all_of(voi)) %>%
    mutate(
      nuts_id = substr(nuts_id, 1, 2)
    )
  
  # variance between countries (each country an observation)
  summary <- data_country %>%
    ungroup() %>%
    summarise(across(all_of(voi), var, na.rm = TRUE)) %>%
    pivot_longer(cols = everything(), names_to = "target_variable", values_to = "between_countries_var")
  
  # variance within regions - at the individual level
  variance_within_nuts <- df %>%
    group_by(country_name_ltn, nuts_id) %>%
    summarise(across(all_of(voi), var, na.rm = TRUE), .groups = 'drop') %>%
    ungroup()
  
  avg_nuts_variance <- variance_within_nuts %>%
    summarise(across(all_of(voi), mean, na.rm = TRUE)) %>%
    pivot_longer(cols = everything(), names_to = "target_variable", values_to = "avg_within_region_var")
  
  # min and max nuts region
  max_variance_region <- variance_within_nuts %>%
    summarise(across(all_of(voi), ~ nuts_id[which.max(.x)])) %>%
    pivot_longer(cols = everything(), names_to = "target_variable", values_to = "max_var_region")
  max_variance_region_value <- variance_within_nuts %>%
    pivot_longer(cols = all_of(voi), names_to = "target_variable", values_to = "variance") %>%
    group_by(target_variable) %>%
    summarise(max_var_region_val = max(variance, na.rm = TRUE),
              .groups = 'drop')
  
  min_variance_region <- variance_within_nuts %>%
    summarise(across(all_of(voi), ~ nuts_id[which.min(.x)])) %>%
    pivot_longer(cols = everything(), names_to = "target_variable", values_to = "min_var_region")
  min_variance_region_value <- variance_within_nuts %>%
    pivot_longer(cols = all_of(voi), names_to = "target_variable", values_to = "variance") %>%
    group_by(target_variable) %>%
    summarise(min_var_region_val = min(variance, na.rm = TRUE),
              .groups = 'drop')
  
  # variance between regions in the same country
  variance_between_regions <- data_nuts %>%
    group_by(country_name_ltn) %>%
    summarise(across(all_of(voi), ~var(.x, na.rm = T))) %>%
    ungroup() %>%
    summarise(across(all_of(voi), mean, na.rm = T)) %>%
    pivot_longer(cols = all_of(voi), names_to = "target_variable", values_to = "avg_between_region_variance")
  
  
  # variance within country (each region an observation)
  variance_within_country <- df %>%
    group_by(country_name_ltn) %>%
    summarise(across(all_of(voi), var, na.rm = TRUE), .groups = 'drop') %>%
    ungroup()
  
  avg_within_country_variance <- variance_within_country %>%
    summarise(across(all_of(voi), mean, na.rm = TRUE)) %>%
    pivot_longer(cols = everything(), names_to = "target_variable", values_to = "avg_within_country_var") 
  
  # min and max country variance
  max_variance_country <- variance_within_country %>%
    summarise(across(all_of(voi), ~ country_name_ltn[which.max(.x)])) %>%
    pivot_longer(cols = everything(), names_to = "target_variable", values_to = "max_var_country") 
  max_variance_country_value <- variance_within_country %>%
    pivot_longer(cols = all_of(voi), names_to = "target_variable", values_to = "variance") %>%
    group_by(target_variable) %>%
    summarise(max_var_country_val = max(variance, na.rm = TRUE),
              .groups = 'drop')
  
  min_variance_country <- variance_within_country %>%
    summarise(across(all_of(voi), ~ country_name_ltn[which.min(.x)])) %>%
    pivot_longer(cols = everything(), names_to = "target_variable", values_to = "min_var_country")
  min_variance_country_value <- variance_within_country %>%
    pivot_longer(cols = all_of(voi), names_to = "target_variable", values_to = "variance") %>%
    group_by(target_variable) %>%
    summarise(min_var_country_val = min(variance, na.rm = TRUE),
              .groups = 'drop')
  
  # combine results
  variance_summary <- summary %>%
    left_join(avg_nuts_variance, by = "target_variable") %>%
    left_join(variance_between_regions, by = "target_variable") %>%
    left_join(min_variance_region, by = "target_variable") %>%
    left_join(min_variance_region_value, by = "target_variable") %>%
    left_join(max_variance_region, by = "target_variable") %>%
    left_join(max_variance_region_value, by = "target_variable") %>%
    left_join(avg_within_country_variance, by = "target_variable") %>%
    left_join(max_variance_country, by = "target_variable") %>%
    left_join(max_variance_country_value, by = "target_variable") %>%
    left_join(min_variance_country, by = "target_variable") %>%
    left_join(min_variance_country_value, by = "target_variable")
  
  
  return(variance_summary)
}


