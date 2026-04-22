library(dplyr)
library(ggplot2)

devtools::load_all()

base_params <- get_parameters(
  overrides = list(
    human_population = 5000 * 2,
    number_initial_S = 4995 * 2,
    number_initial_E = 5 * 2,
    number_initial_I = 0,
    number_initial_R = 0,
    simulation_time  = 150,
    seed             = 42
  ),
  archetype = "sars_cov_2"
)

output_2c <- base_params %>%
  set_setting_specific_ach("workplace", mean = 4.8, sd = 1.5) %>%
  set_setting_specific_ach("school",    mean = 4.0, sd = 1.2) %>%
  set_setting_specific_ach("leisure",   mean = 3.0, sd = 1.0) %>%
  set_setting_specific_ach("household", mean = 0.5, sd = 0.2) %>%
  set_uvc_ach("workplace", coverage = 0.5, coverage_target = "square_footage",
              coverage_type = "targeted_riskiness", timestep = 0,
              relationship_type = "sigmoid", max_efficacy = 0.9,
              sigmoid_k = 1, sigmoid_x0 = 4.8) %>%
  set_uvc_ach("school",    coverage = 0.5, coverage_target = "square_footage",
              coverage_type = "targeted_riskiness", timestep = 0,
              relationship_type = "sigmoid", max_efficacy = 0.9,
              sigmoid_k = 1, sigmoid_x0 = 4) %>%
  set_uvc_ach("leisure",   coverage = 0.5, coverage_target = "square_footage",
              coverage_type = "targeted_riskiness", timestep = 0,
              relationship_type = "sigmoid", max_efficacy = 0.9,
              sigmoid_k = 1, sigmoid_x0 = 3.0) %>%
  set_uvc_ach("household", coverage = 0.5, coverage_target = "square_footage",
              coverage_type = "targeted_riskiness", timestep = 0,
              relationship_type = "sigmoid", max_efficacy = 0.9,
              sigmoid_k = 1, sigmoid_x0 = 0.5) %>%
  run_simulation()

output_df_2c <- output_2c


cat("\n=== KEY METRICS - Run 2C (ACH + UVC sigmoid efficacy) ===\n")
cat("Peak infections:", max(output_df_2c$I_count),
    "at timestep", which.max(output_df_2c$I_count), "\n")
cat("Attack rate:",
    round(max(output_df_2c$R_count) / base_params$human_population * 100, 1), "%\n")
cat("Final susceptible:", tail(output_df_2c$S_count, 1), "\n\n")

output_df_2c %>%
  select(timestep, S_count, E_count, I_count, R_count) %>%
  tidyr::pivot_longer(
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
    title = "Epidemic | ACH | targeted UVC | sigmoid efficacy, 50% coverage)",
    x     = "Timestep",
    y     = "Number of individuals",
    color = "Compartment"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
