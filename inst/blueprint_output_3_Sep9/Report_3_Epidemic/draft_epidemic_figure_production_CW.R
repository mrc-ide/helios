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
output_files <- list.files("./Report_3_Epidemic/Epidemic_Simulation_Batch_1/epidemic_batch_1_outputs/")
output_files <- output_files[which(stringr::str_detect(string = output_files, pattern = "scenario_output"))]

# Quick visualisation of key metrics for endemic#
summary_outputs <- data.frame()
counter <- 1
for(i in 1: length(output_files)) {

  # Load in the output file:
  temp_obj <- readRDS(file = paste0("./Report_3_Epidemic/Epidemic_Simulation_Batch_1/epidemic_batch_1_outputs/", output_files[i]))

  # Filter out the parameter lists:
  tempdf <- list()
  for(j in 1:length(temp_obj)) {
    tempdf[[j]] <- temp_obj[[j]][[2]]
    if (temp_obj[[j]][[1]]$beta_household == 0.207) {
      tempdf[[j]]$archetype <- "influenza"
    } else {
      tempdf[[j]]$archetype <- "sars_cov_2"
    }
    tempdf[[j]]$iteration <- counter
    counter <- counter + 1
  }

  # Create a combined dataframe of the outputs
  tempdf2 <- bind_rows(tempdf) %>%
    mutate(coverage_type = ifelse(is.na(coverage_type), "none", coverage_type)) %>%
    mutate(coverage  = ifelse(is.na(coverage), 0, coverage)) %>%
    mutate(efficacy  = ifelse(is.na(efficacy), 0, efficacy)) %>%
    group_by(iteration, archetype, coverage_type, coverage, efficacy, disease_status) %>%
    summarise(peak = max(I_count),
              peak_timing = first(which(I_count == max(I_count))),
              final_size = max(R_count))

  # Bind it to the combined dataframe:
  summary_outputs <- bind_rows(summary_outputs, tempdf2)
}

avg_summary_outputs <- summary_outputs %>%
  group_by(archetype, coverage_type, coverage, efficacy, disease_status) %>%
  summarise(mean_peak_timing = mean(peak_timing),
            mean_peak = mean(peak),
            mean_final_size = mean(final_size))

ggplot() +
  geom_line(data = subset(avg_summary_outputs, coverage_type == "random" & coverage <= 0.7),
            aes(x = 100 * coverage, y = 100 * mean_final_size), col = "black") +
  geom_line(data = subset(avg_summary_outputs, coverage_type == "targeted_riskiness" & coverage <= 0.7),
            aes(x = 100 * coverage, y = 100 * mean_final_size), col = "grey") +
  facet_grid(archetype ~ efficacy, scales = "free_y") +
  lims(y = c(0, NA))

ggplot() +
  geom_line(data = subset(avg_summary_outputs, coverage_type == "random" & coverage <= 0.7),
            aes(x = 100 * coverage, y = 100 * mean_peak), col = "black") +
  geom_line(data = subset(avg_summary_outputs, coverage_type == "targeted_riskiness" & coverage <= 0.7),
            aes(x = 100 * coverage, y = 100 * mean_peak), col = "grey") +
  facet_grid(archetype ~ efficacy, scales = "free_y") +
  lims(y = c(0, NA))

ggplot() +
  geom_line(data = subset(avg_summary_outputs, coverage_type == "random" & coverage <= 0.7),
            aes(x = 100 * coverage, y = 100 * mean_peak_timing), col = "black") +
  geom_line(data = subset(avg_summary_outputs, coverage_type == "targeted_riskiness" & coverage <= 0.7),
            aes(x = 100 * coverage, y = 100 * mean_peak_timing), col = "grey") +
  facet_grid(archetype ~ efficacy, scales = "free_y") +
  lims(y = c(0, NA))
