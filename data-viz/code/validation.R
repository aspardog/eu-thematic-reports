# function to impute data with less than 30 observations
impute_values <- function(data_points) {
  
  # Extract the GPP source
  GPP_full <- data_points[["GPP"]]
  
  # Create an empty dataframe to store the imputed data
  impute <- data.frame(target_var = character(),
                       nuts_id = character(),
                       demographic = character(),
                       chart_id = character(),
                       count = numeric(),
                       stringsAsFactors = FALSE)
  
  # Iterate through the rows of GPP_full
  for (i in 1:nrow(GPP_full)) {
    if (!is.na(GPP_full$count[i]) && GPP_full$count[i] < 30) {
      # Append the row to the impute dataframe
      impute <- rbind(impute, data.frame(target_var = GPP_full$target_var[i],
                                         nuts_id = GPP_full$nuts_id[i],
                                         demographic = GPP_full$demographic[i],
                                         chart_id = GPP_full$chart_id[i],
                                         count = GPP_full$count[i],
                                         stringsAsFactors = FALSE))
      # Replace the value2plot with NA in the original dataframe
      GPP_full$value2plot[i] <- NA
    }
  }
  
  # special cases (bribery2, discrimination2, discrimination3)
  special_cases <- list(
    # discrimination2 = data_points[["Special"]][["R1F67"]],
    discrimination3 = data_points[["Special"]][["R1F68"]],
    bribery2        = data_points[["Special"]][["R3F14"]]
  )
  
  for (case_name in names(special_cases)) {
    special_data <- special_cases[[case_name]]
    for (i in 1:nrow(special_data)) {
      if (!is.na(special_data$sample_size[i]) && special_data$sample_size[i] < 30) {
        impute <- rbind(impute, data.frame(target_var = case_name,
                                           nuts_id = special_data$nuts_id[i],
                                           demographic = "Total Sample",
                                           chart_id = special_data$chart_id[i],
                                           count = special_data$sample_size[i],
                                           stringsAsFactors = FALSE))
        # Replace the value2plot with NA in the original dataframe
        special_data$value2plot[i] <- NA
      }
    }
  }
  
  # Replace the modified special cases back into data_points
  # data_points[["Special"]][["R1F67"]] <- special_cases[["discrimination2"]]
  data_points[["Special"]][["R1F68"]] <- special_cases[["discrimination3"]]
  data_points[["Special"]][["R3F14"]] <- special_cases[["bribery2"]]
  data_points[["GPP"]] <- GPP_full
  
  # Write the imputed data to an Excel file
  write.xlsx(impute, file = "imputed_data.xlsx")
  
  # Return the modified data_points
  return(data_points)
}




