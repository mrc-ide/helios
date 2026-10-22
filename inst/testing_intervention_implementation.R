# Stress test: delta_depends_on_baseline_ach = TRUE
# Walks through 4 realistic intervention types + a constant-delta baseline.
# Run from /Users/geethaj/helios (ach_efficacy_update branch)

library(dplyr)
library(ggplot2)
library(tidyr)

devtools::load_all("/Users/geethaj/helios")

pop_size <- 20000

# ---- Shared baseline params -------------------------------------------------
make_params <- function(seed = 42) {
  get_parameters(
    overrides = list(
      human_population = pop_size,
      number_initial_S = pop_size - 10,
      number_initial_E = 10,
      number_initial_I = 0,
      number_initial_R = 0,
      simulation_time  = 150,
      seed             = seed
    ),
    archetype = "flu"
  ) %>%
    set_setting_specific_ach("workplace", mean = 4.8, sd = 1.5) %>%
    set_setting_specific_ach("school",    mean = 4.0, sd = 1.2) %>%
    set_setting_specific_ach("leisure",   mean = 3.0, sd = 1.0) %>%
    set_setting_specific_ach("household", mean = 0.5, sd = 0.2)
}

# ---- 5 scenarios to stress-test --------------------------------------------
scenarios <- list(
  list(
    name = "constant_delta=3",
    intv = make_intervention(
      name = "constant",
      delta_depends_on_baseline_ach = FALSE,
      delta_function = function() 3,
      delta_params   = list(),
      coverage       = 0.5
    )
  ),
  list(
    name = "proportional_upgrade_50pct",
    intv = make_intervention(
      name = "proportional",
      delta_depends_on_baseline_ach = TRUE,
      delta_function = function(ach, factor) ach * factor,
      delta_params   = list(factor = 0.5),
      coverage       = 0.5
    )
  ),
  list(
    name = "target_standard_ach4",
    intv = make_intervention(
      name = "target",
      delta_depends_on_baseline_ach = TRUE,
      delta_function = function(ach, target) pmax(0, target - ach),
      delta_params   = list(target = 4),
      coverage       = 0.5
    )
  ),
  list(
    name = "diminishing_returns",
    intv = make_intervention(
      name = "diminishing",
      delta_depends_on_baseline_ach = TRUE,
      delta_function = function(ach, max_boost, scale) max_boost * exp(-ach / scale),
      delta_params   = list(max_boost = 5, scale = 4),
      coverage       = 0.5
    )
  ),
  list(
    name = "capped_proportional",
    intv = make_intervention(
      name = "capped",
      delta_depends_on_baseline_ach = TRUE,
      delta_function = function(ach, factor, cap) pmin(ach * factor, cap),
      delta_params   = list(factor = 0.5, cap = 3),
      coverage       = 0.5
    )
  )
)

# ---- For each scenario: diagnostic + epidemic outcome ----------------------
diagnostics <- list()
seir_curves <- list()

for (s in scenarios) {
  cat("\n========== Scenario:", s$name, "==========\n")

  params <- make_params() %>%
    set_intervention_ach(
      setting         = "joint",
      coverage_target = "individuals",
      coverage_type   = "random",
      timestep        = 0,
      s$intv
    )

  # 1. Inspect per-location delta + efficacy distributions
  v <- create_variables(params)$parameters_list
  for (setting in c("workplace", "school", "leisure")) {
    ach <- v[[paste0(setting, "_specific_ach")]]
    eff <- v[[paste0(setting, "_specific_efficacy")]]
    cov <- v[[paste0("intervention_", setting, "_covered")]]
    cat(sprintf(
      "  %-9s | ACH range: [%.2f, %.2f] | efficacy on covered: mean=%.3f, range=[%.3f, %.3f]\n",
      setting,
      min(ach), max(ach),
      mean(eff[cov == 1]),
      min(eff[cov == 1]), max(eff[cov == 1])
    ))
    diagnostics[[length(diagnostics) + 1]] <- data.frame(
      scenario = s$name, setting = setting,
      ach = ach, efficacy = eff, covered = cov
    )
  }

  # 2. Run the simulation
  out <- run_simulation(params)
  final_pct <- tail(out$R_count, 1) / pop_size * 100
  peak_pct  <- max(out$I_count) / pop_size * 100
  cat(sprintf("  Epidemic outcome: final=%.1f%%, peak=%.1f%%\n",
              final_pct, peak_pct))
  seir_curves[[s$name]] <- out %>% mutate(scenario = s$name)
}

diagnostics_df <- bind_rows(diagnostics)
seir_df        <- bind_rows(seir_curves) %>%
  mutate(scenario = factor(scenario,
                           levels = sapply(scenarios, `[[`, "name")))

# ---- Save -------------------------------------------------------------------
out_dir <- "/Users/geethaj/helios_comparison/output/branch"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Plot 1: efficacy vs ACH across scenarios (covered locations only) ----
p1 <- ggplot(diagnostics_df %>% filter(covered == 1),
             aes(x = ach, y = efficacy, color = scenario)) +
  geom_point(alpha = 0.4, size = 0.8) +
  facet_wrap(~ setting, scales = "free_x") +
  labs(
    title    = "Realized efficacy vs baseline ACH (covered locations only)",
    subtitle = "Shows how each delta_function shape translates to efficacy",
    x = "Baseline ACH (1/hour)",
    y = "Per-location efficacy"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p1)
ggsave(file.path(out_dir, "stress_efficacy_vs_ach.png"), plot = p1,
       width = 10, height = 5, dpi = 150)

# ---- Plot 2: SEIR curves for each scenario --------------------------------
p2 <- ggplot(seir_df, aes(x = timestep)) +
  geom_line(aes(y = I_count, color = scenario), linewidth = 1) +
  labs(
    title    = "Infectious-curve comparison across intervention shapes",
    subtitle = "All scenarios use 50% random joint coverage; same seed",
    x = "Timestep", y = "Infectious count", color = "Scenario"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p2)
ggsave(file.path(out_dir, "stress_seir_comparison.png"), plot = p2,
       width = 9, height = 5, dpi = 150)

cat("\nDone. Outputs in", out_dir, "\n")
