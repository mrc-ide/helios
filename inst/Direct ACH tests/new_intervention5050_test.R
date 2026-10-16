# Branch joint intervention — 50% efficacy, 50% coverage, 10k population
# Run from /Users/geethaj/helios (ach_efficacy_update branch)

library(dplyr)
library(ggplot2)
library(tidyr)

devtools::load_all("/Users/geethaj/helios")

# ---- Scenario setup ---------------------------------------------------------
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

# Setting-specific ACH on for all four settings (your usual values).
params <- base_params %>%
  set_setting_specific_ach("workplace", mean = 4.8, sd = 1.5) %>%
  set_setting_specific_ach("school",    mean = 4.0, sd = 1.2) %>%
  set_setting_specific_ach("leisure",   mean = 3.0, sd = 1.0) %>%
  set_setting_specific_ach("household", mean = 0.5, sd = 0.2)

# ---- Calibrate intervention delta -------------------------------------------
# Pick a constant delta that gives ~50% efficacy at average joint-setting
# conditions. Joint covers workplace/school/leisure (V=27/10/8, mean ACH=
# 4.8/4.0/3.0). Use the mid-range V and ACH as a calibration point.
target_efficacy <- 0.5
delta_calibrated <- efficacy_to_delta(
  target_efficacy = target_efficacy,
  baseline_ach    = 4.0,   # rough middle across the three settings
  V               = 15     # rough middle across V=27, 10, 8
)
cat("Calibrated intervention delta (eACH):", round(delta_calibrated, 2), "\n")

# Constant-delta intervention. delta_function takes no args and returns
# the same delta everywhere; delta_depends_on_baseline_ach = FALSE.
joint_intv <- make_intervention(
  name                     = "joint_uvc",
  delta_depends_on_baseline_ach = FALSE,
  delta_function    = function() delta_calibrated,
  delta_params      = list()
)

# ---- Configure intervention -------------------------------------------------
params <- params %>%
  set_intervention_ach(
    setting         = "joint",
    coverage        = 0.5,
    coverage_target = "individuals",
    coverage_type   = "random",
    timestep        = 0,
    joint_intv
  )

# ---- Run --------------------------------------------------------------------
output <- run_simulation(params)

# ---- Metrics ----------------------------------------------------------------
peak_I      <- max(output$I_count)
peak_t      <- which.max(output$I_count)
final_R     <- tail(output$R_count, 1)
attack_rate <- round(final_R / pop_size * 100, 1)

cat("\n=== BRANCH JOINT INTERVENTION (50% eff target, 50% coverage) ===\n")
cat("Population:        ", pop_size, "\n")
cat("Peak infections:   ", peak_I, "\n")
cat("Peak timestep:     ", peak_t, "\n")
cat("Final recovered:   ", final_R, "\n")
cat("Attack rate:       ", attack_rate, "%\n\n")

# ---- Save outputs -----------------------------------------------------------
out_dir <- "/Users/geethaj/helios_comparison/output/branch"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

write.csv(
  output,
  file.path(out_dir, "joint_intervention.csv"),
  row.names = FALSE
)

# ---- Plot SEIR --------------------------------------------------------------
p <- output %>%
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
  scale_color_manual(values = c(
    Susceptible = "steelblue",
    Exposed     = "orange",
    Infectious  = "red",
    Recovered   = "darkgreen"
  )) +
  labs(
    title    = "Branch joint intervention | 10k pop, ACH on",
    subtitle = paste0(
      "Target eff: ", target_efficacy * 100,
      "% | Coverage: 50% | Attack rate: ", attack_rate,
      "% | Peak: ", peak_I, " @ t=", peak_t
    ),
    x = "Timestep", y = "Number of individuals", color = "Compartment"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p)

ggsave(
  file.path(out_dir, "joint_intervention_seir.png"),
  plot = p, width = 8, height = 5, dpi = 150
)

cat("Outputs saved to:", out_dir, "\n")
