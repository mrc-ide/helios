# Branch baseline — no intervention, 10k population

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

# Setting-specific ACH on for all four settings (your usual values)
params <- base_params %>%
  set_setting_specific_ach("workplace", mean = 4.8, sd = 1.5) %>%
  set_setting_specific_ach("school",    mean = 4.0, sd = 1.2) %>%
  set_setting_specific_ach("leisure",   mean = 3.0, sd = 1.0) %>%
  set_setting_specific_ach("household", mean = 0.5, sd = 0.2)

# No intervention — leave all intervention_*_active flags FALSE.

# ---- Run --------------------------------------------------------------------
output <- run_simulation(params)

# ---- Metrics ----------------------------------------------------------------
peak_I       <- max(output$I_count)
peak_t       <- which.max(output$I_count)
final_R      <- tail(output$R_count, 1)
attack_rate  <- round(final_R / pop_size * 100, 1)

cat("\n=== BRANCH BASELINE (no intervention) ===\n")
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
  file.path(out_dir, "baseline.csv"),
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
    title    = "Branch baseline (no intervention) | 10k pop, ACH on",
    subtitle = paste0(
      "Attack rate: ", attack_rate,
      "% | Peak: ", peak_I, " @ t=", peak_t
    ),
    x = "Timestep", y = "Number of individuals", color = "Compartment"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p)

ggsave(
  file.path(out_dir, "baseline_seir.png"),
  plot = p, width = 8, height = 5, dpi = 150
)

cat("Outputs saved to:", out_dir, "\n")
