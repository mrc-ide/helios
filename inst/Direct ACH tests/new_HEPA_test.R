# Branch HEPA intervention with unit-to-unit variation
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

# HEPA filter intervention. Typical portable HEPA adds ~3 eACH to a room
# (CADR ~500 m^3/hr in a ~200 m^3 space). Unit-to-unit variation captures
# filter age, fan speed setting, and placement effects (sd ~ 0.5 eACH).
# pmax(0, ...) inside calculate_efficacy_from_ach prevents negative deltas
# when noise pushes a draw below zero.
hepa_intv <- make_intervention(
  name                     = "hepa_with_variation",
  affected_by_baseline_ach = FALSE,
  baseline_ach_function    = function() 3,
  baseline_ach_params      = list(),
  variation                = TRUE,
  variation_function       = rnorm,
  variation_params         = list(mean = 0, sd = 0.5)
)

params <- params %>%
  set_intervention_ach(
    setting         = "joint",
    coverage        = 0.5,
    coverage_target = "individuals",
    coverage_type   = "random",
    timestep        = 0,
    hepa_intv
  )

output <- run_simulation(params)

peak_I      <- max(output$I_count)
peak_t      <- which.max(output$I_count)
final_R     <- tail(output$R_count, 1)
attack_rate <- round(final_R / pop_size * 100, 1)

cat("\n=== BRANCH HEPA + VARIATION (delta=3 eACH, sd=0.5, 50% coverage) ===\n")
cat("Peak infections:   ", peak_I, "\n")
cat("Peak timestep:     ", peak_t, "\n")
cat("Attack rate:       ", attack_rate, "%\n\n")

out_dir <- "/Users/geethaj/helios_comparison/output/branch"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

write.csv(output, file.path(out_dir, "hepa_variation.csv"), row.names = FALSE)

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
    title    = "Branch HEPA + variation | 10k pop, ACH on",
    subtitle = paste0(
      "delta=3 eACH (sd=0.5), 50% coverage | Attack rate: ",
      attack_rate, "% | Peak: ", peak_I, " @ t=", peak_t
    ),
    x = "Timestep", y = "Number of individuals", color = "Compartment"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p)
ggsave(file.path(out_dir, "hepa_variation_seir.png"), plot = p, width = 8, height = 5, dpi = 150)

cat("Saved to:", out_dir, "\n")
