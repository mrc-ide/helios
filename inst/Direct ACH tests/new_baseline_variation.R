# Branch baseline-ACH-scaled intervention
# Tests affected_by_baseline_ach = TRUE
# Run from /Users/geethaj/helios (ach_efficacy_update branch)

library(dplyr)
library(ggplot2)
library(tidyr)

devtools::load_all("/Users/geethaj/helios")

pop_size <- 10000

base_params <- get_parameters(
  overrides = list(
    human_population = pop_size,
    number_initial_S = pop_size - 10,
    number_initial_E = 10,
    number_initial_I = 0,
    number_initial_R = 0,
    simulation_time  = 150,
    seed             = 42
  ),
  archetype = "sars_cov_2"
)

params <- base_params %>%
  set_setting_specific_ach("workplace", mean = 4.8, sd = 1.5) %>%
  set_setting_specific_ach("school",    mean = 4.0, sd = 1.2) %>%
  set_setting_specific_ach("leisure",   mean = 3.0, sd = 1.0) %>%
  set_setting_specific_ach("household", mean = 0.5, sd = 0.2)

# Proportional ventilation upgrade: delta = 50% of each location's baseline
# ACH. affected_by_baseline_ach = TRUE means baseline_ach_function is called
# per location with that location's ACH as its first arg. The `boost_fraction`
# param is passed through baseline_ach_params.
boost_intv <- make_intervention(
  name                     = "ventilation_boost_50pct",
  affected_by_baseline_ach = TRUE,
  baseline_ach_function    = function(ach, boost_fraction) ach * boost_fraction,
  baseline_ach_params      = list(boost_fraction = 0.5)
)

params <- params %>%
  set_intervention_ach(
    setting         = "joint",
    coverage        = 0.5,
    coverage_target = "individuals",
    coverage_type   = "random",
    timestep        = 0,
    boost_intv
  )

output <- run_simulation(params)

peak_I      <- max(output$I_count)
peak_t      <- which.max(output$I_count)
final_R     <- tail(output$R_count, 1)
attack_rate <- round(final_R / pop_size * 100, 1)

cat("\n=== BRANCH BASELINE-SCALED INTERVENTION (delta=0.5*baseline, 50% coverage) ===\n")
cat("Peak infections:   ", peak_I, "\n")
cat("Peak timestep:     ", peak_t, "\n")
cat("Attack rate:       ", attack_rate, "%\n\n")

out_dir <- "/Users/geethaj/helios_comparison/output/branch"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

write.csv(output, file.path(out_dir, "baseline_scaled.csv"), row.names = FALSE)

p <- output %>%
  select(timestep, S_count, E_count, I_count, R_count) %>%
  pivot_longer(cols = ends_with("_count"), names_to = "compartment", values_to = "count") %>%
  mutate(compartment = factor(
    compartment,
    levels = c("S_count", "E_count", "I_count", "R_count"),
    labels = c("Susceptible", "Exposed", "Infectious", "Recovered")
  )) %>%
  ggplot(aes(x = timestep, y = count, color = compartment)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c(
    Susceptible = "steelblue", Exposed = "orange",
    Infectious = "red", Recovered = "darkgreen"
  )) +
  labs(
    title    = "Branch baseline-scaled intervention | 10k pop, ACH on",
    subtitle = paste0(
      "delta = 0.5 * baseline ACH, 50% coverage | Attack rate: ",
      attack_rate, "% | Peak: ", peak_I, " @ t=", peak_t
    ),
    x = "Timestep", y = "Number of individuals", color = "Compartment"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p)
ggsave(file.path(out_dir, "baseline_scaled_seir.png"), plot = p, width = 8, height = 5, dpi = 150)

cat("Saved to:", out_dir, "\n")
