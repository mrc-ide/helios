library(dplyr)
library(ggplot2)
library(tidyr)
library(truncnorm)

devtools::load_all()

base_params <- get_parameters(
  overrides = list(
    human_population = 10000 *25,
    number_initial_S = 9990*25,
    number_initial_E = 10*25,
    number_initial_I = 0,
    number_initial_R = 0,
    simulation_time  = 150,
    seed             = 42
  ),
  archetype = "sars_cov_2"
)

params_2a <- base_params %>%
  set_setting_specific_ach("workplace", mean = 4.8, sd = 1.5) %>%
  set_setting_specific_ach("school",    mean = 4.0, sd = 1.2) %>%
  set_setting_specific_ach("leisure",   mean = 3.0, sd = 1.0) %>%
  set_setting_specific_ach("household", mean = 0.5, sd = 0.2)

variables_output <- create_variables(params_2a)
params_with_riskiness <- variables_output$parameters_list

output_2a <- run_simulation(params_2a)
output_df_2a <- output_2a


p1 <- output_df_2a %>%
  select(timestep, S_count, E_count, I_count, R_count) %>%
  pivot_longer(
    cols      = ends_with("_count"),
    names_to  = "compartment",
    values_to = "count"
  ) %>%
  mutate(
    compartment = factor(
      compartment,
      levels = c("S_count", "E_count", "I_count", "R_count"),
      labels = c("Susceptible", "Exposed", "Infectious", "Recovered")
    )
  ) %>%
  ggplot(aes(x = timestep, y = count, color = compartment)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c(
      "Susceptible" = "steelblue",
      "Exposed"     = "orange",
      "Infectious"  = "red",
      "Recovered"   = "darkgreen"
    )
  ) +
  labs(
    title = "Epidemic | ACH Pipeline | no UVC",
    x     = "Timestep",
    y     = "Number of individuals",
    color = "Compartment"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p1)

riskiness_data <- data.frame(
  setting = c(
    rep("Workplace", length(params_with_riskiness$workplace_specific_riskiness)),
    rep("School", length(params_with_riskiness$school_specific_riskiness)),
    rep("Leisure", length(params_with_riskiness$leisure_specific_riskiness)),
    rep("Household", length(params_with_riskiness$household_specific_riskiness))
  ),
  riskiness = c(
    params_with_riskiness$workplace_specific_riskiness,
    params_with_riskiness$school_specific_riskiness,
    params_with_riskiness$leisure_specific_riskiness,
    params_with_riskiness$household_specific_riskiness
  )
)

p2 <- riskiness_data %>%
  ggplot(aes(x = riskiness, fill = setting)) +
  geom_histogram(bins = 40, alpha = 0.7, position = "identity") +
  facet_wrap(~setting, scales = "free_y", ncol = 2) +
  scale_x_continuous(
    breaks = seq(0.5,3, by = 0.5),
    limits = c(0.5,3)
  ) +
  scale_fill_manual(
    values = c(
      "Workplace" = "steelblue",
      "School"    = "orange",
      "Leisure"   = "purple",
      "Household" = "darkgreen"
    )
  ) +
  labs(
    title = "Riskiness Distributions by Setting (from ACH)",
    x     = "Relative Riskiness",
    y     = "Count",
    fill  = "Setting"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p2)

# Summary statistics for riskiness
cat("\n=== RISKINESS SUMMARY ===\n")
riskiness_summary <- riskiness_data %>%
  group_by(setting) %>%
  summarise(
    mean_riskiness = mean(riskiness),
    median_riskiness = median(riskiness),
    sd_riskiness = sd(riskiness),
    min_riskiness = min(riskiness),
    max_riskiness = max(riskiness)
  )

print(riskiness_summary)

variables_output_2a <- create_variables(params_2a)
cat("\n=== ACH Summary ===\n")
cat("Household ACH: mean=", mean(variables_output_2a$parameters_list$household_specific_ach),
    "sd=", sd(variables_output_2a$parameters_list$household_specific_ach), "\n")
cat("Workplace ACH: mean=", mean(variables_output_2a$parameters_list$workplace_specific_ach),
    "sd=", sd(variables_output_2a$parameters_list$workplace_specific_ach), "\n")
