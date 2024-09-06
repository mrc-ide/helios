#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+++++ Visual Checks of Endemic Simulation Outputs +++++#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# Load in the requisite packages:
library(tidyverse)
library(patchwork)

# Specify the Blueprint colours:
blueprint_colours <- colorRampPalette(c("#00AFFF", "#03113E"))(4)

##' Simulations were run varying the following variables:
##'
##' 1. archetype (flu, SARS-CoV-2)
##' 2. coverage type ()
##' 3. coverage
##' 4. efficacy
##'
##' 25 iterations were run for each parameterisation

#----- 1) Load the Outputs -------------------------------------------------------------------------

# Set the working directory to within the Blueprint Report 3 directory:
setwd("./inst/blueprint_output_3_Sep9/")

# List the names of the files in the endemic directory and load them into a single, unified dataframe
output_files <- list.files("./Report_3_Epidemic/Epidemic_Simulation_Batch_1/epidemic_batch_1_outputs/")
output_files <- output_files[which(stringr::str_detect(string = output_files, pattern = "scenario_output"))]

# Check the number of files (should be 30)
length(output_files); length(output_files) == 30

#----- 2) Create the dataframe(s) ------------------------------------------------------------------

# Quick visualisation of key metrics for endemic#
summary_outputs <- data.frame()

# Initiate a counter to reimpose an "iteration" column on the dataframes:
counter <- 1

# Loop through the output files, match the archetype using the beta values, and create a combined dataframe:
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
  tempdf2 <- bind_rows(tempdf) |>
    mutate(coverage_type = ifelse(is.na(coverage_type), "none", coverage_type)) |>
    mutate(coverage  = ifelse(is.na(coverage), 0, coverage)) |>
    mutate(efficacy  = ifelse(is.na(efficacy), 0, efficacy)) |>
    group_by(iteration, archetype, coverage_type, coverage, efficacy, disease_status) |>
    summarise(peak = max(I_count),
              peak_timing = first(which(I_count == max(I_count))),
              final_size = max(R_count))

  # Bind it to the combined dataframe:
  summary_outputs <- bind_rows(summary_outputs, tempdf2)
}

# Generate the summary metrics for the epidemic report:
avg_summary_outputs <- summary_outputs |>
  group_by(archetype, coverage_type, coverage, efficacy, disease_status) |>
  summarise(mean_peak_timing = mean(peak_timing),
            mean_peak = mean(peak),
            mean_final_size = mean(final_size))

# Save the dataframes:
saveRDS(object = summary_outputs, file = "./Report_3_Epidemic/Epidemic_Simulation_Batch_1/epidemic_summary_outputs.rds")
saveRDS(object = avg_summary_outputs, file = "./Report_3_Epidemic/Epidemic_Simulation_Batch_1/epidemic_avg_summary_outputs.rds")

##' NOTE: As the completed runs did not append all column identifiers / parameter values, the number
##' of parameter combinations appears different from the parameter table generated when creating the
##' parameter lists. We have introduced variable values (e.g. coverate_type == "none") that were not
##' present in the original parameter sets. Not a problem necessarily, but needs to be born in mind
##' when running the script.

#----- 3) Figure.2.1.a Mean Final Epidemic Size ----------------------------------------------------

# Turn off scientific notation:
options(scipen = 666)

#+++ Figure 2.1.a: Mean final epidemic size +++#
ggplot() +
  geom_line(data = subset(avg_summary_outputs, coverage_type == "random" & coverage <= 0.7),
            aes(x = 100 * coverage, y = 100 * mean_final_size), col = blueprint_colours[1], linewidth = 1) +
  geom_line(data = subset(avg_summary_outputs, coverage_type == "targeted_riskiness" & coverage <= 0.7),
            aes(x = 100 * coverage, y = 100 * mean_final_size), col = blueprint_colours[2], linewidth = 1) +
  theme_bw() +
  facet_grid(archetype ~ efficacy,
             scales = "free_y",
             labeller = as_labeller(c(`influenza` = "Influenza",
                                     `sars_cov_2` = "SARS-CoV-2",
                                     `0.4` = "40% Efficacy",
                                     `0.6` = "60% Efficacy",
                                     `0.8` = "80% Efficacy"))) +
  theme(strip.background = )
  lims(y = c(0, NA))

#----- 4) Figure.2.1.b Mean Peak Size --------------------------------------------------------------

#+++ Figure 2.1.b: +++#
ggplot() +
  geom_line(data = subset(avg_summary_outputs, coverage_type == "random" & coverage <= 0.7),
            aes(x = 100 * coverage, y = 100 * mean_peak), col = blueprint_colours[1]) +
  geom_line(data = subset(avg_summary_outputs, coverage_type == "targeted_riskiness" & coverage <= 0.7),
            aes(x = 100 * coverage, y = 100 * mean_peak), col = blueprint_colours[4]) +
  facet_grid(archetype ~ efficacy, scales = "free_y") +
  theme_bw()
  lims(y = c(0, NA))

#----- 5) Figure.2.1.c Mean Peak Timing ------------------------------------------------------------

#+++ Figure 2.1.c: Mean Peak Timing +++#
ggplot() +
  geom_line(data = subset(avg_summary_outputs, coverage_type == "random" & coverage <= 0.7),
            aes(x = 100 * coverage, y = 100 * mean_peak_timing), col = blueprint_colours[1]) +
  geom_line(data = subset(avg_summary_outputs, coverage_type == "targeted_riskiness" & coverage <= 0.7),
            aes(x = 100 * coverage, y = 100 * mean_peak_timing), col = blueprint_colours[4]) +
  theme_bw() +
  facet_grid(archetype ~ efficacy, scales = "free_y") +
  lims(y = c(0, NA))
