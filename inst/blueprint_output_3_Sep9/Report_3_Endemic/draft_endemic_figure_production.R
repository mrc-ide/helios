#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+++++ Visual Checks of Endemic Simulation Outputs +++++#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# Load in the requisite packages:
library(tidyverse)
library(patchwork)

#----- 1) Load the Outputs -------------------------------------------------------------------------

# Set the working directory to within the Blueprint Report 3 directory:
setwd("./inst/blueprint_output_3_Sep9/")

# List the names of the files in the endemic directory and load them into a single, unified dataframe
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

# Create a long-format version of the combined dataframe:
combined_outputs |>
  pivot_longer(cols = c(S_count, E_count, I_count, R_count),
               names_to = "state",
               values_to = "individuals") -> combined_outputs_long

#----- 2) Mean number of infected individuals through time (by iteration) --------------------------

# SARS-CoV-2
combined_outputs_long |>
  filter(timestep >= ((365 * 15 * 2) + 1)) |>
  filter(archetype == "sars_cov_2") |>
  filter(state == "I_count") |>
  summarise(I = mean(individuals), .by = c(archetype, coverage_type, coverage, efficacy, timestep)) |>
  ggplot(aes(x = timestep, y = I, colour = factor(efficacy))) +
  geom_line() +
  theme_bw() +
  labs(col = "Efficacy",
       x = "Time (Days)",
       y = "Mean Number of Infectious Individuals") +
  geom_vline(xintercept = timestep_uvc_on, linetype = "dashed") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_grid(coverage_type~coverage)

# Influenza
combined_outputs_long |>
  filter(timestep >= ((365 * 15 * 2) + 1)) |>
  filter(archetype == "flu") |>
  filter(state == "I_count") |>
  summarise(I = mean(individuals), .by = c(archetype, coverage_type, coverage, efficacy, timestep)) |>
  ggplot(aes(x = timestep, y = I, colour = factor(efficacy))) +
  geom_line() +
  theme_bw() +
  labs(col = "Efficacy",
       x = "Time (Days)",
       y = "Mean Number of Infectious Individuals") +
  geom_vline(xintercept = timestep_uvc_on, linetype = "dashed") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_grid(coverage_type~coverage)


#----- 3) Charlie's Report 2 Figure 1: Infectious Dynamics -----------------------------------------

#----- 3) Charlie's Report 2 Figure 1: Infectious Dynamics -----------------------------------------

# Load an example parameter list:
example_parameter_list <- readRDS("./Report_3_Endemic/endemic_parameter_list_1.rds")

# Get the time and human population parameters for plotting:
dt <- example_parameter_list[[1]]$dt
population <- example_parameter_list[[1]]$human_population
years_to_simulate <- example_parameter_list[[1]]$simulation_time / 365
timestep_baseline_start <- ((years_to_simulate - 5) * 365) / dt
timestep_baseline_end <- ((years_to_simulate - 2) * 365) / dt
timestep_uvc_start <- ((years_to_simulate - 2) * 365 + 1) / dt
timestep_uvc_end <- (years_to_simulate * 365) / dt

