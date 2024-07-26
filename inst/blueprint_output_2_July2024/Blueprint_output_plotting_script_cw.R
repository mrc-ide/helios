# Load in the requisite packages:
library(helios)
library(ggplot2)
library(tidyverse)

# Loading in the outputs
outputs <- readRDS("inst/blueprint_output_2_July2024/raw_outputs_results1.rds")
simulations_to_run <- readRDS("inst/blueprint_output_2_July2024/simulations_to_run.rds")
parameter_lists <- readRDS("inst/blueprint_output_2_July2024/parameter_lists.rds")

# Processing the data
### NEED SOME WAY OF PORTING IN YEARS TO SIMULATE, TIMING OF UVC ETC FROM THE RUNS IN HERE
dt <- parameter_lists[[1]]$dt
population <- parameter_lists[[1]]$human_population
years_to_simulate <- parameter_lists[[1]]$simulation_time / 365
timestep_baseline_start <- ((years_to_simulate - 4) * 365) / dt
timestep_baseline_end <- ((years_to_simulate - 2) * 365) / dt
timestep_uvc_start <- ((years_to_simulate - 2) * 365 + 1) / dt
timestep_uvc_end <- (years_to_simulate * 365) / dt

## Removing the burnin whilst the model reaches equilibrium
outputs_processed <- outputs
index_start <- which(outputs_processed[[1]]$timestep == timestep_baseline_start)
index_end <- which(outputs_processed[[1]]$timestep == timestep_uvc_end)
for (i in 1:length(outputs)) {
  outputs_processed[[i]] <- outputs_processed[[i]][index_start:index_end, ]
  outputs_processed[[i]]$archetype <- simulations_to_run[i, "archetype"]
  outputs_processed[[i]]$coverage <- simulations_to_run[i, "coverage"]
  outputs_processed[[i]]$efficacy <- simulations_to_run[i, "efficacy"]
  outputs_processed[[i]]$iteration <- simulations_to_run[i, "iteration"]
  outputs_processed[[i]]$new_timestep <- outputs_processed[[i]]$timestep - min(outputs_processed[[i]]$timestep)
}

## Plotting the trajectories of individual stochastic simulations
index <- 2 * 365
overall <- dplyr::bind_rows(outputs_processed) %>%
  mutate(daily_timestep = floor(new_timestep * dt)) %>%
  group_by(iteration, archetype, efficacy, coverage, daily_timestep) %>%
  summarise(S_count = mean(S_count),
            E_count = mean(E_count),
            I_count = mean(I_count),
            R_count = mean(R_count),
            E_new = sum(E_new),
            n_external_infections = sum(n_external_infections))
end <- length(unique(overall$daily_timestep))
ggplot(overall, aes(x = daily_timestep, y = I_count * 1000 / population,
                     group = interaction(archetype, factor(iteration)),
                    col = archetype)) +
  geom_line(alpha = 1) +
  geom_vline(xintercept = c(0, 365, 730, 1095, 1460), linewidth = 0.25, linetype = "dashed") +
  geom_vline(xintercept = index, linewidth = 0.5, linetype = "solid") +
  facet_grid(efficacy ~ coverage,
             labeller = as_labeller(c(`0` = "0% Coverage",
                                      `0.1` = "10% Coverage",
                                      `0.25` = "25% Coverage",
                                      `0.5` = "50% Coverage",
                                      `0.4` = "40% Efficacy",
                                      `0.6` = "60% Efficacy",
                                      `0.8` = "80% Efficacy"))) +
  theme_bw() +
  scale_colour_manual(values = c("#C463B1", "#79CB97"),
                      labels = c("Influenza", "SARS-CoV-2"),
                      breaks = c("flu", "sars_cov_2")) +
  labs(col = "Pathogen\nArchetype",
       x = "Time (Days)",
       y = "Number of Infectious Individuals (Per 1,000 Population)")

## Plotting the range spanned by the stochastic simulations
overall2 <- overall %>%
  group_by(daily_timestep, archetype, efficacy, coverage) %>%
  summarise(S_count = mean(S_count),
            S_lower = min(S_count),
            S_upper = max(S_count),
            E_count = mean(E_count),
            E_lower = min(E_count),
            E_upper = max(E_count),
            I_count = mean(I_count),
            I_lower = min(I_count),
            I_upper = max(I_count),
            R_count = mean(R_count),
            R_lower = min(R_count),
            R_upper = max(R_count),
            E_new = mean(E_new),
            E_new_lower = min(E_new),
            E_new_upper = max(E_new))
ggplot(overall2, aes(x = daily_timestep, col = archetype)) +
  geom_line(aes(y = 1000 * E_new / population), linewidth = 0.1) +
  geom_ribbon(aes(fill = archetype,
                  ymin = 1000 * E_new_lower / population,
                  ymax = 1000 * E_new_upper / population), alpha = 0.15) +
  geom_vline(xintercept = c(0, 365, 730, 1095, 1460), linewidth = 0.25, linetype = "dashed") +
  geom_vline(xintercept = index, linewidth = 0.5, linetype = "solid") +
  facet_grid(efficacy ~ coverage,
             labeller = as_labeller(c(`0` = "0% Coverage",
                                      `0.1` = "10% Coverage",
                                      `0.25` = "25% Coverage",
                                      `0.5` = "50% Coverage",
                                      `0.4` = "40% Efficacy",
                                      `0.6` = "60% Efficacy",
                                      `0.8` = "80% Efficacy"))) +
  theme_bw() +
  scale_colour_manual(values = c("#C463B1", "#79CB97"),
                      labels = c("Influenza", "SARS-CoV-2"),
                      breaks = c("flu", "sars_cov_2")) +
  scale_fill_manual(values = c("#C463B1", "#79CB97"),
                    labels = c("Influenza", "SARS-CoV-2"),
                    breaks = c("flu", "sars_cov_2")) +
  labs(col = "Pathogen\nArchetype", fill = "Pathogen\nArchetype", x = "Time (Days)",
       y = "Daily Infection Incidence (Per 1,000 Population)")

## Calculating and plotting incidence reduction
index <- 2 * 365
end <- length(unique(overall$daily_timestep))
overall3 <- overall %>%
  group_by(daily_timestep, archetype, efficacy, coverage) %>%
  summarise(S_count = mean(S_count), E_new = mean(E_new)) %>%
  ungroup() %>%
  group_by(archetype, efficacy, coverage) %>%
  summarise(incidence_pre_uvc = sum(E_new[1:index]),
            incidence_after_uvc = sum(E_new[(index + 1):end])) %>%
  pivot_longer(cols = c(incidence_pre_uvc, incidence_after_uvc),
               values_to = "incidence", names_to = "period")

overall3$period <- factor(overall3$period, levels = c("incidence_pre_uvc", "incidence_after_uvc"))
ggplot(overall3, aes(x = 100 * efficacy, y = 0.5 * incidence * 1000 / population, fill = period)) +
  geom_bar(stat = "identity", position = "dodge", col = "black") +
  scale_fill_manual(values = c("#E1E1E1", "#324A5F"),
                    labels = c("Incidence\nPre-UVC", "Incidence\nPost-UVC"),
                    breaks = c("incidence_pre_uvc", "incidence_after_uvc")) +
  theme_bw() +
  facet_grid(archetype ~ coverage, scales = "free_y",
             labeller = as_labeller(c(`0` = "0% Coverage",
                                      `0.1` = "10% Coverage",
                                      `0.25` = "25% Coverage",
                                      `0.5` = "50% Coverage",
                                      `flu` = "Influenza",
                                      `sars_cov_2` = "SARS-CoV-2"))) +
  labs(fill = "Time\nPeriod", x = "Far UVC Efficacy (%)",
       y = "Annual Infection Incidence (Per 1,000 Population)")

overall4 <- overall %>%
  group_by(daily_timestep, archetype, efficacy, coverage) %>%
  summarise(S_count = mean(S_count), E_new = mean(E_new)) %>%
  ungroup() %>%
  group_by(archetype, efficacy, coverage) %>%
  summarise(incidence_pre_uvc = sum(E_new[1:index]),
            incidence_after_uvc = sum(E_new[(index + 1):end])) %>%
  mutate(incidence_reduction = incidence_pre_uvc - incidence_after_uvc,
         incidence_percentage_reduction = incidence_reduction / incidence_pre_uvc)
ggplot(overall4, aes(x = 100 * efficacy, y = 100 * incidence_percentage_reduction)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#324A5F", col = "black") +
  theme_bw() +
  facet_grid(archetype ~ coverage, scales = "free_y",
             labeller = as_labeller(c(`0` = "0% Coverage",
                                      `0.1` = "10% Coverage",
                                      `0.25` = "25% Coverage",
                                      `0.5` = "50% Coverage",
                                      `flu` = "Influenza",
                                      `sars_cov_2` = "SARS-CoV-2"))) +
  labs(fill = "Time\nPeriod", x = "Far UVC Efficacy (%)",
       y = "Reduction in Infection Incidence (Per 1,000 Population)")

for (i in 1:40) {

  x <- outputs[[i]]
  index_start <- which(x$timestep == timestep_baseline_start)
  index_end <- which(x$timestep == timestep_uvc_end)
  x <- x[index_start:index_end, ]

  layout(matrix(c(1,1,2,3, 1,1,4,5), nrow = 2, byrow = TRUE))
  par(mar = c(3, 3, 3, 3))
  plot(x$timestep * 0.5, x$S_count, ylim = c(0, 115000), type = "l", col = "#333333", xlab = "Time", ylab = "Number")
  lines(x$timestep * 0.5, x$E_new, ylim = c(0, 100000), type = "l", col = "#A0D848")
  lines(x$timestep * 0.5, x$n_external_infections, ylim = c(0, 100000), type = "l", col = "#F15A24")
  lines(x$timestep * 0.5, x$R_count, ylim = c(0, 100000), type = "l", col = "#29ABE2")
  abline(v = timestep_baseline_start * 0.5)
  abline(v = timestep_baseline_end * 0.5)
  abline(v = timestep_uvc_start * 0.5)
  abline(v = timestep_uvc_end * 0.5)

  plot(x$timestep * 0.5, x$S_count, ylim = c(0, max(x$S_count)), type = "l", col = "#333333", xlab = "Time", ylab = "S Count")
  plot(x$timestep * 0.5, x$E_new, ylim = c(0, max(x$E_new)), type = "l", col = "#A0D848", xlab = "Time", ylab = "E Count")
  plot(x$timestep * 0.5, x$n_external_infections, ylim = c(0, max(x$n_external_infections)), type = "l", col = "#F15A24", xlab = "Time", ylab = "I Count")
  plot(x$timestep * 0.5, x$R_count, ylim = c(0, max(x$R_count)), type = "l", col = "#29ABE2", xlab = "Time", ylab = "R Count")

  browser()

}

