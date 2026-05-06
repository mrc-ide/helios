library(dplyr)
library(ggplot2)
library(tidyr)
library(truncnorm)

devtools::load_all()

base_params <- get_parameters(
  overrides = list(
    human_population = 10000,
    number_initial_S = 9990,
    number_initial_E = 10,
    number_initial_I = 0,
    number_initial_R = 0,
    simulation_time  = 150,
    seed             = 42
  ),
  archetype = "sars_cov_2"
)

# ── Baseline: ACH only, no intervention ──────────────────────────────────────

params_baseline <- base_params %>%
  set_setting_specific_ach("workplace", mean = 4.8, sd = 1.5) %>%
  set_setting_specific_ach("school",    mean = 4.0, sd = 1.2) %>%
  set_setting_specific_ach("leisure",   mean = 3.0, sd = 1.0) %>%
  set_setting_specific_ach("household", mean = 0.5, sd = 0.2)

variables_baseline    <- create_variables(params_baseline)
params_with_details   <- variables_baseline$parameters_list
output_baseline       <- run_simulation(params_baseline)

# ── Sanity checks ─────────────────────────────────────────────────────────────

cat("\n=== ACH SUMMARY ===\n")
for (s in c("workplace", "school", "leisure", "household")) {
  ach <- params_with_details[[paste0(s, "_specific_ach")]]
  cat(s, ": mean =", round(mean(ach), 2), "| sd =", round(sd(ach), 2), "\n")
}

cat("\n=== EPIDEMIC METRICS ===\n")
cat("Peak infections:", max(output_baseline$I_count), "\n")
cat("Attack rate:", round(max(output_baseline$R_count) / base_params$human_population * 100, 1), "%\n")

# ── Plot 1: SEIR curve ────────────────────────────────────────────────────────

p1 <- output_baseline %>%
  select(timestep, S_count, E_count, I_count, R_count) %>%
  pivot_longer(cols = ends_with("_count"), names_to = "compartment", values_to = "count") %>%
  mutate(compartment = factor(compartment,
                              levels = c("S_count", "E_count", "I_count", "R_count"),
                              labels = c("Susceptible", "Exposed", "Infectious", "Recovered"))) %>%
  ggplot(aes(x = timestep, y = count, color = compartment)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("Susceptible" = "steelblue", "Exposed" = "orange",
                                "Infectious" = "red", "Recovered" = "darkgreen")) +
  labs(title = "SEIR curve — baseline, no intervention",
       x = "Timestep", y = "Number of individuals", color = "Compartment") +
  theme_minimal() + theme(legend.position = "bottom")

print(p1)

# ── Plot 2: ACH distributions ─────────────────────────────────────────────────

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
  geom_histogram(bins = 40, alpha = 0.7) +
  facet_wrap(~setting, scales = "free", ncol = 2) +
  scale_fill_manual(values = c("Workplace" = "steelblue", "School" = "orange",
                               "Leisure" = "purple", "Household" = "darkgreen")) +
  labs(title = "ACH distributions by setting", x = "ACH (hr⁻¹)", y = "Count", fill = "Setting") +
  theme_minimal() + theme(legend.position = "bottom")

print(p2)

# ── Plot 3: Riskiness distributions ──────────────────────────────────────────

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

p3 <- riskiness_data %>%
  ggplot(aes(x = riskiness, fill = setting)) +
  geom_histogram(bins = 40, alpha = 0.7) +
  facet_wrap(~setting, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c("Workplace" = "steelblue", "School" = "orange",
                               "Leisure" = "purple", "Household" = "darkgreen")) +
  labs(title = "Riskiness distributions by setting",
       x = "Relative riskiness", y = "Count", fill = "Setting") +
  theme_minimal() + theme(legend.position = "bottom")

print(p3)
