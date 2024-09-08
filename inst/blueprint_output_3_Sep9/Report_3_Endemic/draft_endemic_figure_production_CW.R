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
output_files <- list.files("./Report_3_Endemic/Endemic_Simulation_Batch_3/endemic_batch_3_outputs/")
output_files <- output_files[which(stringr::str_detect(string = output_files, pattern = "scenario_output"))]
temp <- readRDS(file = paste0("./Report_3_Endemic/Endemic_Simulation_Batch_3/endemic_batch_3_outputs/", output_files[1]))

# Extracting relevant indices to calculate far UVC attributable reductions
years_to_simulate <- temp[[1]]$parameters$years_to_simulate
timestep_uvc_on <- temp[[1]]$parameters$timestep_uvc_on
years_uvc_on_for <- temp[[1]]$parameters$years_uvc_on_for

indices_pre_UVC <- (timestep_uvc_on - (365 * 4 / temp_obj[[1]]$parameters$dt)): timestep_uvc_on
indices_post_UVC <- (timestep_uvc_on + 1):(365 * years_to_simulate / temp_obj[[1]]$parameters$dt)
years_pre_UVC <- length(indices_pre_UVC) / (365 / temp[[1]]$parameters$dt)
years_post_UVC <- length(indices_post_UVC) / (365 / temp[[1]]$parameters$dt)

## Summarising output
summary_outputs <- data.frame()
for(i in 1: length(output_files)) {

  # Load in the output file:
  temp_obj <- readRDS(file = paste0("./Report_3_Endemic/Endemic_Simulation_Batch_3/endemic_batch_3_outputs/", output_files[i]))

  # Filter out the parameter lists:
  tempdf <- list()
  for(j in 1:length(temp_obj)) {
    tempdf[[j]] <- temp_obj[[j]][[2]]
  }

  # Create a combined dataframe of the outputs
  tempdf <- bind_rows(tempdf) %>%
    group_by(ID, archetype, coverage_type, coverage, efficacy, disease_status, iteration) %>%
    summarise(avg_inc_pre_UVC = sum(E_new[indices_pre_UVC]) / years_pre_UVC,
              avg_inc_post_UVC = sum(E_new[indices_post_UVC]) / years_post_UVC,
              avg_prev_pre_UVC = mean(I_count[indices_pre_UVC]),
              avg_prev_post_UVC = mean(I_count[indices_post_UVC])) %>%
    mutate(reduction_incidence = 1 - avg_inc_post_UVC / avg_inc_pre_UVC) %>%
    mutate(reduction_prevalence = 1 - avg_prev_post_UVC / avg_prev_pre_UVC)

  # Bind it to the combined dataframe:
  summary_outputs <- bind_rows(summary_outputs, tempdf)
}

head(summary_outputs)
endemic_summary_output <- summary_outputs
saveRDS(endemic_summary_output, file = "inst/blueprint_output_3_Sep9/Report_3_Endemic/Endemic_Simulation_Batch_3/endemic_summary_outputs.rds")

avg_summary_outputs <- summary_outputs %>%
  group_by(archetype, coverage_type, coverage, efficacy, disease_status) %>%
  summarise(mean_reduction_incidence = mean(reduction_incidence),
            mean_reduction_prevalence = mean(reduction_prevalence),
            lower_reduction_incidence = min(reduction_incidence),
            upper_reduction_incidence = max(reduction_incidence)) %>%
  ungroup() %>%
  mutate(new_ID = 1:n())

inc_reduction_plot <- ggplot(data = subset(avg_summary_outputs, coverage_type == "random" & coverage <= 0.7),
       aes(x = 100 * coverage, y = 100 * mean_reduction_incidence)) +
  geom_bar(data = subset(avg_summary_outputs, coverage_type == "targeted_riskiness" & coverage <= 0.7),
           aes(x = 100 * coverage, y = 100 * mean_reduction_incidence),
           col = "black", fill = "lightgrey", stat = "identity") +
  geom_bar(stat = "identity", position = "dodge", fill = "#324A5F", col = "black") +
  geom_errorbar(aes(ymin = 100 * lower_reduction_incidence,
                    ymax = 100 * upper_reduction_incidence),
                width = 5) +
  geom_errorbar(data = subset(avg_summary_outputs, coverage_type == "targeted_riskiness" & coverage <= 0.7),
                aes(ymin = 100 * lower_reduction_incidence,
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

# Plot
inc_reduction_alt_plot <- ggplot(data = subset(avg_summary_outputs, coverage <= 0.7),
       aes(x = 100 * coverage, y = 100 * mean_reduction_incidence, fill = coverage_type)) +
  geom_bar(stat = "identity", position = position_dodge(), col = "black", width = 6) +
  geom_errorbar(aes(ymin = 100 * lower_reduction_incidence,
                    ymax = 100 * upper_reduction_incidence,
                    group = coverage_type),
                width = 5, position = position_dodge(6)) +
  theme_bw() +
  scale_fill_manual(values = c("lightgrey", "#324A5F"),
                    labels = c("Targeted\nRiskiness", "Random")) +  # Colors for each bar
  facet_grid(archetype ~ efficacy, scales = "free_y",
             labeller = as_labeller(c(`0.4` = "40% Efficacy",
                                      `0.6` = "60% Efficacy",
                                      `0.8` = "80% Efficacy",
                                      `flu` = "Influenza",
                                      `sars_cov_2` = "SARS-CoV-2"))) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 80, 100)) +
  labs(fill = "Targeting\nStrategy", x = "Far UVC Coverage (%)",
       y = "% Reduction in Annual Infection Incidence")

avg_summary_outputs_diff <- summary_outputs %>%
  mutate(diff = avg_inc_pre_UVC - avg_inc_post_UVC) %>%
  select(iteration, archetype, coverage_type, coverage, efficacy, disease_status, diff) %>%
  pivot_wider(id_cols = c(iteration, archetype, coverage, efficacy, disease_status),
              names_from = coverage_type,
              values_from = diff) %>%
  group_by(archetype, coverage, efficacy, disease_status) %>%
  mutate(diff_rel = targeted_riskiness / random - 1) %>%
  summarise(mean_diff_rel = median(diff_rel, na.rm = TRUE),
            lower_diff_rel = quantile(diff_rel, 0.25, na.rm = TRUE),
            upper_diff_rel = quantile(diff_rel, 0.75, na.rm = TRUE))

extra_impact_rel <- ggplot(data = subset(avg_summary_outputs_diff, coverage <= 0.9),
       aes(x = 100 * coverage, y = 100 * mean_diff_rel)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#08BDBD", col = "black") +
  geom_errorbar(aes(ymin = 100 * lower_diff_rel, ymax = 100 * upper_diff_rel)) +
  theme_bw() +
  facet_grid(archetype ~ efficacy, scales = "free_y",
             labeller = as_labeller(c(`0.4` = "40% Efficacy",
                                      `0.6` = "60% Efficacy",
                                      `0.8` = "80% Efficacy",
                                      `flu` = "Influenza",
                                      `sars_cov_2` = "SARS-CoV-2"))) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  labs(fill = "Time\nPeriod", x = "Far UVC Coverage (%)",
       y = "Relative Extra Impact (%)")

# Loading and processing exemplar full output

# Load in the output file:
temp_obj <- readRDS(file = paste0("./Report_3_Endemic/Endemic_Simulation_Batch_3/endemic_batch_3_outputs/", output_files[20]))
temp_df <- temp_obj[[65]]
temp_df$parameters$far_uvc_joint_coverage
temp_df$parameters$far_uvc_joint_efficacy

for_plot <- subset(temp_df[[2]], timestep >= 10730) %>%
  mutate(time_period = ifelse(((timestep - 10730) * temp_df$parameters$dt) / 365 > 5, "Pre-UVC", "Post-UVC"))
for_plot_avg <- for_plot %>%
  group_by(time_period) %>%
  summarise(time_period_avg = mean(1000 * E_new / 100000),
            start = min((timestep - 10730) * temp_df$parameters$dt) / 365,
            end = max((timestep - 10730) * temp_df$parameters$dt) / 365)

exemplar_plot <- ggplot() +
  geom_line(data = for_plot,
            aes(x = ((timestep - 10730) * temp_df$parameters$dt) / 365,
                y = 1000 * E_new / 100000,
                col = ifelse(((timestep - 10730) * temp_df$parameters$dt) / 365 > 5, "Pre-UVC", "Post-UVC"))) +
  geom_vline(xintercept = 5, linetype = "dashed") +
  scale_colour_manual(values = c("lightgrey", "orange"),
                      labels = c("Pre-UVC", "Post-UVC"),
                      name = "Time Period") +
  geom_segment(data = for_plot_avg,
               aes(x = start, xend = end,
                   y = time_period_avg, yend = time_period_avg),
               linetype = "dashed", color = "black") +
  theme_bw() +
  labs(x = "Year (far UVC on at Year 5)", y = "Daily Incidence Per 1,000 Population")


# combined_outputs <- data.frame()
# for(i in 1: length(output_files)) {
#
#   # Load in the output file:
#   temp_obj <- readRDS(file = paste0("./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_outputs/", output_files[i]))
#
#   # Filter out the parameter lists:
#   tempdf <- list()
#   for(j in 1:length(temp_obj)) {
#     tempdf[[j]] <- temp_obj[[j]][[2]]
#   }
#
#   # Create a combined dataframe of the outputs
#   tempdf <- bind_rows(tempdf)
#
#   # Bind it to the combined dataframe:
#   combined_outputs <- bind_rows(combined_outputs, tempdf) %>%
#     select(timestep, E_new, E_count, I_count, iteration, archetype, coverage_type, coverage, efficacy)
# }
#
# combined_outputs_summarised <- combined_outputs %>%
#   group_by(archetype, coverage_type, coverage, efficacy, iteration) %>%
#   filter(timestep > min(indices_prev_UVC)) %>%
#   filter(coverage == 0.5)
#
# ggplot(combined_outputs_summarised, aes(x = timestep,
#                              y = I_count,
#                              colour = coverage_type,
#                              group = interaction(coverage_type, factor(iteration)))) +
#   geom_line() +
#   facet_grid(archetype ~ efficacy)
# extra_impact_abs <- ggplot(data = subset(avg_summary_outputs_diff, coverage <= 0.9),
#        aes(x = 100 * coverage, y = 100 * diff_abs)) +
#   geom_bar(stat = "identity", position = "dodge", fill = "#324A5F", col = "black") +
#   theme_bw() +
#   facet_grid(archetype ~ efficacy, scales = "free_y",
#              labeller = as_labeller(c(`0.4` = "40% Efficacy",
#                                       `0.6` = "60% Efficacy",
#                                       `0.8` = "80% Efficacy",
#                                       `flu` = "Influenza",
#                                       `sars_cov_2` = "SARS-CoV-2"))) +
#   scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
#   labs(fill = "Time\nPeriod", x = "Far UVC Coverage (%)",
#        y = "Absolute Extra Impact")

