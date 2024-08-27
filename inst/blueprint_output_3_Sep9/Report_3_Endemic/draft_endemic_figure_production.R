

output_files <- list.files("./Report_3_Endemic")
output_files <- output_files[which(stringr::str_detect(string = output_files, pattern = "scenario_output"))]
combined_outputs <- data.frame()
for(i in 1: length(output_files)) {

  # Load in the output file:
  temp_obj <- readRDS(file = paste0("./Report_3_Endemic/", output_files[i]))

  # Filter out the parameter lists:
  tempdf <- list()
  for(j in 1:length(temp_obj)) {
    tempdf[[j]] <- temp_obj[[j]][[2]]
  }

  # Create a combined dataframe of the outputs
  tempdf <- bind_rows(tempdf)

  # Bind it to the combined dataframe:
  combined_outputs <- bind_rows(combined_outputs, tempdf)
}

# View each of the unique parameter values:
all(unique(combined_outputs$ID) %in% 1:200)
unique(combined_outputs$archetype)
unique(combined_outputs$coverage_type)
unique(combined_outputs$coverage)
unique(combined_outputs$efficacy)
unique(combined_outputs$disease_status)
unique(combined_outputs$iteration)

# Calculate the timestep on which to switch Far UVC on:
timestep_uvc_on <- round(((20 - 2) * 365) / 0.5)

combined_outputs |>
  pivot_longer(cols = c(S_count, E_count, I_count, R_count), names_to = "state", values_to = "individuals") |>
  filter(timestep >= ((365 * 15 * 2) + 1)) |>
  filter(efficacy == 0.6) |>
  filter(state == "R_count") |>
  ggplot(aes(x = timestep, y = individuals, colour = as.factor(iteration), linetype = archetype)) +
  geom_line() +
  theme_bw() +
  geom_vline(xintercept = timestep_uvc_on, linetype = "dashed") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_grid(coverage_type~coverage)
