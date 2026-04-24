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
                       delta_ach = 0, delta_uv = 2) %>%  # UV-C example
  set_intervention_ach("household", coverage = 0.5, coverage_target = "square_footage",
                       coverage_type = "random", timestep = 0,
                       delta_ach = 1, delta_uv = 0)

# Create variables to get ACH, riskiness, and efficacy distributions
variables_output <- create_variables(params_test)
params_with_details <- variables_output$parameters_list


for (s in c("workplace", "school", "leisure", "household")) {
  eff <- params_with_details[[paste0(s, "_specific_efficacy")]]
  cat(s, ": mean =", round(mean(eff), 3),
      "| min =", round(min(eff), 3),
      "| max =", round(max(eff), 3), "\n")
}


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

p1 <- ach_data %>%
  ggplot(aes(x = ach, fill = setting)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  facet_wrap(~setting, scales = "free", ncol = 2) +
  scale_fill_manual(values = c("Workplace" = "steelblue", "School" = "orange",
                               "Leisure" = "purple", "Household" = "darkgreen")) +
  labs(title = "ACH Distributions by Setting", x = "ACH", y = "Count") +
  theme_minimal() + theme(legend.position = "bottom")

print(p1)



combined_data <- data.frame(
  setting = efficacy_data$setting,
  ach = ach_data$ach,
  efficacy = efficacy_data$efficacy
)

p3 <- combined_data %>%
  ggplot(aes(x = ach, y = efficacy, color = setting)) +
  geom_point(alpha = 0.3, size = 0.8) +
  facet_wrap(~setting, scales = "free_x", ncol = 2) +
  scale_color_manual(values = c("Workplace" = "steelblue", "School" = "orange",
                                "Leisure" = "purple", "Household" = "darkgreen")) +
  labs(title = "ACH vs Efficacy by Setting",
       subtitle = "Higher baseline ACH → lower marginal efficacy from intervention",
       x = "Baseline ACH", y = "Efficacy") +
  theme_minimal() + theme(legend.position = "bottom")

print(p3)


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

p4 <- riskiness_data %>%
  ggplot(aes(x = riskiness, fill = setting)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  facet_wrap(~setting, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c("Workplace" = "steelblue", "School" = "orange",
                               "Leisure" = "purple", "Household" = "darkgreen")) +
  labs(title = "Riskiness Distributions by Setting", x = "Relative Riskiness", y = "Count") +
  theme_minimal() + theme(legend.position = "bottom")

print(p4)
