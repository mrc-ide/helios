library(dplyr)
library(ggplot2)
library(tidyr)
library(truncnorm)

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

params_2b <- base_params %>%
  set_setting_specific_ach("workplace", mean = 4.8, sd = 1.5) %>%
  set_setting_specific_ach("school",    mean = 4.0, sd = 1.2) %>%
  set_setting_specific_ach("leisure",   mean = 3.0, sd = 1.0) %>%
  set_setting_specific_ach("household", mean = 0.5, sd = 0.2) %>%
  set_uvc_ach("workplace", coverage = 0.5, coverage_target = "square_footage",
              coverage_type = "random", timestep = 0,
              relationship_type = "constant", max_efficacy = 0.5,
              sigmoid_k = NULL, sigmoid_x0 = NULL) %>%
  set_uvc_ach("school",    coverage = 0.5, coverage_target = "square_footage",
              coverage_type = "random", timestep = 0,
              relationship_type = "constant", max_efficacy = 0.5,
              sigmoid_k = NULL, sigmoid_x0 = NULL) %>%
  set_uvc_ach("leisure",   coverage = 0.5, coverage_target = "square_footage",
              coverage_type = "random", timestep = 0,
              relationship_type = "constant", max_efficacy = 0.5,
              sigmoid_k = NULL, sigmoid_x0 = NULL) %>%
  set_uvc_ach("household", coverage = 0.5, coverage_target = "square_footage",
              coverage_type = "random", timestep = 0,
              relationship_type = "constant", max_efficacy = 0.5,
              sigmoid_k = NULL, sigmoid_x0 = NULL)

# Create variables to get ACH and riskiness distributions
variables_output <- create_variables(params_2b)
params_with_details <- variables_output$parameters_list

# Run simulation
output_2b <- run_simulation(params_2b)
output_df_2b <- output_2b

# ========================================================================
# EXTRACT KEY METRICS
# ========================================================================

# Peak infections
peak_infections <- max(output_df_2b$I_count)
timestep_at_peak <- which.max(output_df_2b$I_count)

# Attack rate
attack_rate <- round(max(output_df_2b$R_count) / base_params$human_population * 100, 1)

# Final susceptible count
final_susceptible <- tail(output_df_2b$S_count, 1)

# Additional metrics
final_recovered <- tail(output_df_2b$R_count, 1)

# Print epidemic metrics
cat("\n=== EPIDEMIC METRICS ===\n")
cat("Peak infections:", peak_infections, "\n")
cat("Timestep at peak:", timestep_at_peak, "\n")
cat("Attack rate:", attack_rate, "%\n")
cat("Final susceptible:", final_susceptible, "\n")
cat("Final recovered:", final_recovered, "\n\n")

# ========================================================================
# PLOT 1: SEIR CURVE
# ========================================================================

p1 <- output_df_2b %>%
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
    title = "Epidemic | ACH Pipeline | random UVC (Constant 50% efficacy, 50% coverage)",
    subtitle = paste0("Attack rate: ", attack_rate, "% | Peak infections: ", peak_infections, " at timestep ", timestep_at_peak),
    x     = "Timestep",
    y     = "Number of individuals",
    color = "Compartment"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p1)

# ========================================================================
# PLOT 2: ACH DISTRIBUTIONS
# ========================================================================

ach_data <- data.frame(
  setting = c(
    rep("Workplace", length(params_with_details$workplace_specific_ach)),
    rep("School", length(params_with_details$school_specific_ach)),
    rep("Leisure", length(params_with_details$leisure_specific_ach)),
    rep("Household", length(params_with_details$household_specific_ach))
  ),
  ach = c(
    params_with_details$workplace_specific_ach,
    params_with_details$school_specific_ach,
    params_with_details$leisure_specific_ach,
    params_with_details$household_specific_ach
  )
)

p2 <- ach_data %>%
  ggplot(aes(x = ach, fill = setting)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  facet_wrap(~setting, scales = "free", ncol = 2) +
  scale_fill_manual(
    values = c(
      "Workplace" = "steelblue",
      "School"    = "orange",
      "Leisure"   = "purple",
      "Household" = "darkgreen"
    )
  ) +
  labs(
    title = "ACH Distributions by Setting",
    x     = "ACH (Air Changes per Hour)",
    y     = "Count",
    fill  = "Setting"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p2)

# ========================================================================
# PLOT 3: RISKINESS DISTRIBUTIONS
# ========================================================================

riskiness_data <- data.frame(
  setting = c(
    rep("Workplace", length(params_with_details$workplace_specific_riskiness)),
    rep("School", length(params_with_details$school_specific_riskiness)),
    rep("Leisure", length(params_with_details$leisure_specific_riskiness)),
    rep("Household", length(params_with_details$household_specific_riskiness))
  ),
  riskiness = c(
    params_with_details$workplace_specific_riskiness,
    params_with_details$school_specific_riskiness,
    params_with_details$leisure_specific_riskiness,
    params_with_details$household_specific_riskiness
  )
)

p3 <- riskiness_data %>%
  ggplot(aes(x = riskiness, fill = setting)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  facet_wrap(~setting, scales = "free_y", ncol = 2) +
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
    subtitle = "UVC targeted at high-riskiness locations (top 50% by square footage)",
    x     = "Relative Riskiness",
    y     = "Count",
    fill  = "Setting"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p3)

# ========================================================================
# SUMMARY STATISTICS
# ========================================================================

cat("\n=== ACH SUMMARY ===\n")
ach_summary <- ach_data %>%
  group_by(setting) %>%
  summarise(
    n_locations = n(),
    mean_ach = mean(ach),
    median_ach = median(ach),
    sd_ach = sd(ach),
    min_ach = min(ach),
    max_ach = max(ach)
  )
print(ach_summary)

cat("\n=== RISKINESS SUMMARY ===\n")
riskiness_summary <- riskiness_data %>%
  group_by(setting) %>%
  summarise(
    n_locations = n(),
    mean_riskiness = mean(riskiness),
    median_riskiness = median(riskiness),
    sd_riskiness = sd(riskiness),
    min_riskiness = min(riskiness),
    max_riskiness = max(riskiness)
  )
print(riskiness_summary)

cat("\n=== ACH → RISKINESS CONVERSION ===\n")
cat("Household: median ACH =", median(params_with_details$household_specific_ach),
    "→ median riskiness =", round(median(params_with_details$household_specific_riskiness), 3), "\n")
cat("Workplace: median ACH =", median(params_with_details$workplace_specific_ach),
    "→ median riskiness =", round(median(params_with_details$workplace_specific_riskiness), 3), "\n")
cat("School: median ACH =", median(params_with_details$school_specific_ach),
    "→ median riskiness =", round(median(params_with_details$school_specific_riskiness), 3), "\n")
cat("Leisure: median ACH =", median(params_with_details$leisure_specific_ach),
    "→ median riskiness =", round(median(params_with_details$leisure_specific_riskiness), 3), "\n")

cat("\n=== UVC TARGETING ===\n")
cat("Coverage type: targeted_riskiness (targets high-riskiness locations)\n")
cat("Efficacy type: constant (50% efficacy regardless of ACH)\n")
cat("Coverage: 50% of square footage in each setting\n")
