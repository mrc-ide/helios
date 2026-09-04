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

params_test <- base_params %>%
  set_setting_specific_ach("workplace", mean = 4.8, sd = 1.5) %>%
  set_setting_specific_ach("school",    mean = 4.0, sd = 1.2) %>%
  set_setting_specific_ach("leisure",   mean = 3.0, sd = 1.0) %>%
  set_setting_specific_ach("household", mean = 0.5, sd = 0.2) %>%
  set_intervention_ach("workplace", coverage = 0.5, coverage_target = "square_footage",
                       coverage_type = "random", timestep = 0,
                       delta_ach = 3, delta_uv = 0) %>%
  set_intervention_ach("school",    coverage = 0.5, coverage_target = "square_footage",
                       coverage_type = "random", timestep = 0,
                       delta_ach = 3, delta_uv = 0) %>%
  set_intervention_ach("leisure",   coverage = 0.5, coverage_target = "square_footage",
                       coverage_type = "random", timestep = 0,
                       delta_ach = 0, delta_uv = 2) %>%
  set_intervention_ach("household", coverage = 0.5, coverage_target = "square_footage",
                       coverage_type = "random", timestep = 0,
                       delta_ach = 1, delta_uv = 0)

# Create variables to get ACH, riskiness, and efficacy distributions
variables_output <- create_variables(params_test)
params_with_details <- variables_output$parameters_list

# Run simulation
output_df <- run_simulation(params_test)


for (s in c("workplace", "school", "leisure", "household")) {
  eff <- params_with_details[[paste0(s, "_specific_efficacy")]]
  cat(s, ": mean =", round(mean(eff), 3),
      "| min =", round(min(eff), 3),
      "| max =", round(max(eff), 3), "\n")
}

peak_infections <- max(output_df$I_count)
timestep_at_peak <- which.max(output_df$I_count)
attack_rate <- round(max(output_df$R_count) / base_params$human_population * 100, 1)

cat("\n=== EPIDEMIC METRICS ===\n")
cat("Peak infections:", peak_infections, "\n")
cat("Timestep at peak:", timestep_at_peak, "\n")
cat("Attack rate:", attack_rate, "%\n")


p1 <- output_df %>%
  select(timestep, S_count, E_count, I_count, R_count) %>%
  pivot_longer(cols = ends_with("_count"), names_to = "compartment", values_to = "count") %>%
  mutate(compartment = factor(compartment,
                              levels = c("S_count", "E_count", "I_count", "R_count"),
                              labels = c("Susceptible", "Exposed", "Infectious", "Recovered"))) %>%
  ggplot(aes(x = timestep, y = count, color = compartment)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("Susceptible" = "steelblue", "Exposed" = "orange",
                                "Infectious" = "red", "Recovered" = "darkgreen")) +
  labs(title = "SEIR Curve | Wells-Riley ACH-based efficacy",
       subtitle = paste0("Attack rate: ", attack_rate, "% | Peak: ", peak_infections,
                         " at timestep ", timestep_at_peak),
       x = "Timestep", y = "Number of individuals", color = "Compartment") +
  theme_minimal() + theme(legend.position = "bottom")

print(p1)


ach_data <- data.frame(
  setting = c(
    rep("Workplace", length(params_with_details$workplace_specific_ach)),
    rep("School",    length(params_with_details$school_specific_ach)),
    rep("Leisure",   length(params_with_details$leisure_specific_ach)),
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
  geom_histogram(bins = 30, alpha = 0.7) +
  facet_wrap(~setting, scales = "free", ncol = 2) +
  scale_fill_manual(values = c("Workplace" = "steelblue", "School" = "orange",
                               "Leisure" = "purple", "Household" = "darkgreen")) +
  labs(title = "ACH Distributions by Setting", x = "ACH", y = "Count") +
  theme_minimal() + theme(legend.position = "bottom")

print(p2)



efficacy_data <- data.frame(
  setting = c(
    rep("Workplace", length(params_with_details$workplace_specific_efficacy)),
    rep("School",    length(params_with_details$school_specific_efficacy)),
    rep("Leisure",   length(params_with_details$leisure_specific_efficacy)),
    rep("Household", length(params_with_details$household_specific_efficacy))
  ),
  efficacy = c(
    params_with_details$workplace_specific_efficacy,
    params_with_details$school_specific_efficacy,
    params_with_details$leisure_specific_efficacy,
    params_with_details$household_specific_efficacy
  )
)

p3 <- efficacy_data %>%
  ggplot(aes(x = efficacy, fill = setting)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  facet_wrap(~setting, scales = "free", ncol = 2) +
  scale_fill_manual(values = c("Workplace" = "steelblue", "School" = "orange",
                               "Leisure" = "purple", "Household" = "darkgreen")) +
  labs(title = "Efficacy Distributions by Setting (Wells-Riley derived)",
       subtitle = "Efficacy = relative reduction in P(infection) from intervention",
       x = "Efficacy", y = "Count") +
  theme_minimal() + theme(legend.position = "bottom")

print(p3)


combined_data <- data.frame(
  setting  = ach_data$setting,
  ach      = ach_data$ach,
  efficacy = efficacy_data$efficacy
)

p4 <- combined_data %>%
  ggplot(aes(x = ach, y = efficacy, color = setting)) +
  geom_point(alpha = 0.3, size = 0.8) +
  facet_wrap(~setting, scales = "free_x", ncol = 2) +
  scale_color_manual(values = c("Workplace" = "steelblue", "School" = "orange",
                                "Leisure" = "purple", "Household" = "darkgreen")) +
  labs(title = "ACH vs Efficacy by Setting",
       subtitle = "Higher baseline ACH → lower marginal efficacy from intervention",
       x = "Baseline ACH", y = "Efficacy") +
  theme_minimal() + theme(legend.position = "bottom")

print(p4)


riskiness_data <- data.frame(
  setting = c(
    rep("Workplace", length(params_with_details$workplace_specific_riskiness)),
    rep("School",    length(params_with_details$school_specific_riskiness)),
    rep("Leisure",   length(params_with_details$leisure_specific_riskiness)),
    rep("Household", length(params_with_details$household_specific_riskiness))
  ),
  riskiness = c(
    params_with_details$workplace_specific_riskiness,
    params_with_details$school_specific_riskiness,
    params_with_details$leisure_specific_riskiness,
    params_with_details$household_specific_riskiness
  )
)

p5 <- riskiness_data %>%
  ggplot(aes(x = riskiness, fill = setting)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  facet_wrap(~setting, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c("Workplace" = "steelblue", "School" = "orange",
                               "Leisure" = "purple", "Household" = "darkgreen")) +
  labs(title = "Riskiness Distributions by Setting", x = "Relative Riskiness", y = "Count") +
  theme_minimal() + theme(legend.position = "bottom")

print(p5)
