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
output_files <- list.files("./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_outputs/")
output_files <- output_files[which(stringr::str_detect(string = output_files, pattern = "scenario_output"))]

# Quick visualisation of key metrics for endemic#
years_to_simulate <- 24
summary_outputs <- data.frame()
for(i in 1: length(output_files)) {

  # Load in the output file:
  temp_obj <- readRDS(file = paste0("./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_outputs/", output_files[i]))

  # Filter out the parameter lists:
  tempdf <- list()
  for(j in 1:length(temp_obj)) {
    tempdf[[j]] <- temp_obj[[j]][[2]]
  }

  indices_prev_UVC <- round(((years_to_simulate - 4) * 365) / temp_obj[[1]]$parameters$dt) : round(((years_to_simulate - 2) * 365) / temp_obj[[1]]$parameters$dt)
  indices_post_UVC <- round(((years_to_simulate - 2) * 365) / temp_obj[[1]]$parameters$dt):(365 * years_to_simulate / temp_obj[[1]]$parameters$dt)
  years_pre_UVC <- length(indices_prev_UVC) / (365 / temp_obj[[1]]$parameters$dt)
  years_post_UVC <- length(indices_post_UVC) / (365 / temp_obj[[1]]$parameters$dt)

  # Create a combined dataframe of the outputs
  tempdf <- bind_rows(tempdf) %>%
    group_by(ID, archetype, coverage_type, coverage, efficacy, disease_status, iteration) %>%
    summarise(avg_inc_pre_UVC = sum(E_new[indices_prev_UVC]) / years_pre_UVC,
              avg_inc_post_UVC = sum(E_new[indices_post_UVC]) / years_post_UVC,
              avg_prev_pre_UVC = mean(I_count[indices_prev_UVC]),
              avg_prev_post_UVC = mean(I_count[indices_post_UVC])) %>%
    mutate(reduction_incidence = 1 - avg_inc_post_UVC / avg_inc_pre_UVC) %>%
    mutate(reduction_prevalence = 1 - avg_prev_post_UVC / avg_prev_pre_UVC)

  # Bind it to the combined dataframe:
  summary_outputs <- bind_rows(summary_outputs, tempdf)
}

head(summary_outputs)

avg_summary_outputs <- summary_outputs %>%
  group_by(archetype, coverage_type, coverage, efficacy, disease_status) %>%
  summarise(mean_reduction_incidence = mean(reduction_incidence),
            mean_reduction_prevalence = mean(reduction_prevalence),
            lower_reduction_incidence = min(reduction_incidence),
            upper_reduction_incidence = max(reduction_incidence))

# ggplot(subset(avg_summary_outputs, coverage_type == "random" & coverage <= 0.7),
#               aes(x = 100 * coverage, y = 100 * mean_reduction_incidence)) +
#   geom_bar(stat = "identity", position = "dodge", fill = "#324A5F", col = "black") +
#   geom_errorbar(aes(ymin = 100 * lower_reduction_incidence,
#                     ymax = 100 * upper_reduction_incidence),
#                 width = 5) +
#   theme_bw() +
#   facet_grid(archetype ~ efficacy, scales = "free_y",
#              labeller = as_labeller(c(`0.4` = "40% Efficacy",
#                                       `0.6` = "60% Efficacy",
#                                       `0.8` = "80% Efficacy",
#                                       `flu` = "Influenza",
#                                       `sars_cov_2` = "SARS-CoV-2"))) +
#   scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 80, 100)) +
#   labs(fill = "Time\nPeriod", x = "Far UVC Coverage (%)",
#        y = "% Reduction in Annual Infection Incidence")

ggplot(data = subset(avg_summary_outputs, coverage_type == "random" & coverage <= 0.7),
       aes(x = 100 * coverage, y = 100 * mean_reduction_incidence)) +
  geom_bar(data = subset(avg_summary_outputs, coverage_type == "targeted_riskiness" & coverage <= 0.7),
           aes(x = 100 * coverage, y = 100 * mean_reduction_incidence),
           col = "black", fill = "lightgrey", stat = "identity") +
  geom_bar(stat = "identity", position = "dodge", fill = "#324A5F", col = "black") +
  geom_errorbar(aes(ymin = 100 * lower_reduction_incidence,
                    ymax = 100 * upper_reduction_incidence),
                width = 5) +
  theme_bw() +
  facet_grid(archetype ~ efficacy, scales = "free_y",
             labeller = as_labeller(c(`0.4` = "40% Efficacy",
                                      `0.6` = "60% Efficacy",
                                      `0.8` = "80% Efficacy",
                                      `flu` = "Influenza",
                                      `sars_cov_2` = "SARS-CoV-2"))) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 80, 100)) +
  labs(fill = "Time\nPeriod", x = "Far UVC Coverage (%)",
       y = "% Reduction in Annual Infection Incidence")


# Loading and processing the full outputs
combined_outputs <- data.frame()
for(i in 1: length(output_files)) {

  # Load in the output file:
  temp_obj <- readRDS(file = paste0("./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_outputs/", output_files[i]))

  # Filter out the parameter lists:
  tempdf <- list()
  for(j in 1:length(temp_obj)) {
    tempdf[[j]] <- temp_obj[[j]][[2]]
  }

  # Create a combined dataframe of the outputs
  tempdf <- bind_rows(tempdf)

  # Bind it to the combined dataframe:
  combined_outputs <- bind_rows(combined_outputs, tempdf) %>%
    select(timestep, E_new, E_count, I_count, iteration, archetype, coverage_type, coverage, efficacy)
}

combined_outputs_summarised <- combined_outputs %>%
  group_by(archetype, coverage_type, coverage, efficacy, iteration) %>%
  filter(timestep > min(indices_prev_UVC)) %>%
  filter(coverage == 0.5)

ggplot(combined_outputs_summarised, aes(x = timestep,
                             y = I_count,
                             colour = coverage_type,
                             group = interaction(coverage_type, factor(iteration)))) +
  geom_line() +
  facet_grid(archetype ~ efficacy)






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

