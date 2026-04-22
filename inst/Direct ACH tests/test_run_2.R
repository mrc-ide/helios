########

library(dplyr)
library(ggplot2)

# Direct riskiness + UVC 50% efficacy/coverage
devtools::load_all()

# Setup base parameters
base_params <- get_parameters(
  overrides = list(
    human_population = 5000 * 2,
    number_initial_S = 4995 * 2,
    number_initial_E = 5 * 2,
    number_initial_I = 0,
    number_initial_R = 0,
    simulation_time  = 150 ,
    seed             = 42
  ),
  archetype = "sars_cov_2"
)

# Direct riskiness + UVC at 50% efficacy and coverage from timestep 0
output_1b <- base_params %>%
  set_setting_specific_riskiness("workplace", mean = 0, sd = 0.37, min = 0.4472, max = 2.236) %>%
  set_setting_specific_riskiness("school",    mean = 0, sd = 0.37, min = 0.4472, max = 2.236) %>%
  set_setting_specific_riskiness("leisure",   mean = 0, sd = 0.37, min = 0.4472, max = 2.236) %>%
  set_setting_specific_riskiness("household", mean = 0, sd = 0.37, min = 0.4472, max = 2.236) %>%
  set_uvc("workplace", coverage = 0.5, coverage_target = "square_footage",
          coverage_type = "targeted_riskiness", efficacy = 0.5, timestep = 0) %>%
  set_uvc("school",    coverage = 0.5, coverage_target = "square_footage",
          coverage_type = "targeted_riskiness", efficacy = 0.5, timestep = 0) %>%
  set_uvc("leisure",   coverage = 0.5, coverage_target = "square_footage",
          coverage_type = "targeted_riskiness", efficacy = 0.5, timestep = 0) %>%
  set_uvc("household", coverage = 0.5, coverage_target = "square_footage",
          coverage_type = "targeted_riskiness", efficacy = 0.5, timestep = 0) %>%
  run_simulation()

# Extract result data frame
output_df_1b <- as.data.frame(output_1b$result)

cat("\n=== KEY METRICS - Run 1B (Direct riskiness + UVC 50% efficacy/coverage) ===\n")
cat("Peak infections:", max(output_df_1b$I_count),
    "at timestep", which.max(output_df_1b$I_count), "\n")
cat("Attack rate:",
    round(max(output_df_1b$R_count) / base_params$human_population * 100, 1), "%\n")
cat("Final susceptible:", tail(output_df_1b$S_count, 1), "\n\n")

# Visualize epidemic curve
output_df_1b %>%
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
    title = "Epidemic |  Direct Riskiness | targeted UVC (50% efficacy, 50% coverage)",
    x     = "Timestep",
    y     = "Number of individuals",
    color = "Compartment"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

