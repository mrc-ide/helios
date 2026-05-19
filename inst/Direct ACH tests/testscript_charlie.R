
# helios ACH/efficacy pipeline — playground
# Quick reference:
#
# (1) Base parameters
#     params <- get_parameters(overrides = list(...), archetype = "sars_cov_2")
#
# (2) (Optional) Setting-specific ACH for any subset of settings.
#     If omitted for a setting, that setting gets uniform riskiness = 1.
#     params <- params %>%
#       set_setting_specific_ach("workplace", mean = 4.8, sd = 1.5) %>%
#       set_setting_specific_ach("school",    mean = 4.0, sd = 1.2) %>%
#       set_setting_specific_ach("leisure",   mean = 3.0, sd = 1.0) %>%
#       set_setting_specific_ach("household", mean = 0.5, sd = 0.2)
#
# (3) (Optional) Define an intervention. Three orthogonal knobs:
#     - constant delta:            affected_by_baseline_ach = FALSE,
#                                  baseline_ach_function = function() <delta>
#     - delta varies with ACH:     affected_by_baseline_ach = TRUE,
#                                  baseline_ach_function = function(ach, ...) <expr>
#     - unit-to-unit noise:        variation = TRUE,
#                                  variation_function = rnorm,
#                                  variation_params = list(mean=0, sd=...)
#     Coverage (fraction of total setting size to cover) lives on the
#     intervention object:
#     intv <- make_intervention(
#       name                     = "...",
#       affected_by_baseline_ach = FALSE,
#       baseline_ach_function    = function() 4,
#       baseline_ach_params      = list(),
#       coverage                 = 0.5
#     )
#
# (4) (Optional) Install the intervention.
#     params <- set_intervention_ach(
#       params,
#       setting         = "joint",
#       coverage_target = "square_footage"
#       coverage_type   = "random",
#       timestep        = 0,
#       intv
#     )
#
# Run:
#     output <- run_simulation(params)
# =============================================================================

library(dplyr)
library(ggplot2)
library(tidyr)

devtools::load_all("/Users/geethaj/helios")

# ---- Shared helpers ---------------------------------------------------------
# A small, fast scenario template. ~1 minute per run at 10k pop.
make_base_params <- function(seed = 42) {
  get_parameters(
    overrides = list(
      human_population = 10000,
      number_initial_S = 9990,
      number_initial_E = 10,
      number_initial_I = 0,
      number_initial_R = 0,
      simulation_time  = 150,
      seed             = seed
    ),
    archetype = "sars_cov_2"
  )
}

# Standard setting-specific ACH config used in most sections below.
add_setting_specific_ach <- function(params) {
  params %>%
    set_setting_specific_ach("workplace", mean = 4.8, sd = 1.5) %>%
    set_setting_specific_ach("school",    mean = 4.0, sd = 1.2) %>%
    set_setting_specific_ach("leisure",   mean = 3.0, sd = 1.0) %>%
    set_setting_specific_ach("household", mean = 0.5, sd = 0.2)
}

# Plot SEIR curves with attack rate and peak metrics.
plot_seir <- function(output, title) {
  pop <- sum(output[1, c("S_count", "E_count", "I_count", "R_count")])
  peak_I <- max(output$I_count)
  peak_t <- which.max(output$I_count)
  attack <- round(tail(output$R_count, 1) / pop * 100, 1)

  p <- output %>%
    select(timestep, S_count, E_count, I_count, R_count) %>%
    pivot_longer(cols = ends_with("_count"),
                 names_to = "compartment", values_to = "count") %>%
    mutate(compartment = factor(compartment,
                                levels = c("S_count", "E_count", "I_count", "R_count"),
                                labels = c("Susceptible", "Exposed", "Infectious", "Recovered"))) %>%
    ggplot(aes(x = timestep, y = count, color = compartment)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = c(
      Susceptible = "steelblue", Exposed = "orange",
      Infectious  = "red",       Recovered = "darkgreen")) +
    labs(title = title,
         subtitle = paste0("Attack rate: ", attack, "% | Peak: ",
                           peak_I, " @ t=", peak_t),
         x = "Timestep", y = "Individuals", color = "Compartment") +
    theme_minimal() + theme(legend.position = "bottom")
  print(p)
  invisible(p)
}


# =============================================================================
# Section 1 — Bare baseline (no ACH, no intervention)
# =============================================================================
# The simplest possible run. Verifies the package loads and runs end-to-end.

cat("\n========== 1: bare baseline ==========\n")
params_1 <- make_base_params()
out_1    <- run_simulation(params_1)
plot_seir(out_1, "1: Bare baseline")

# TRY: change `seed = 42` in make_base_params() and re-run.
# TRY: change `archetype = "sars_cov_2"` to `"flu"` or `"measles"`.


# =============================================================================
# Section 2 — Setting-specific ACH on, still no intervention
# =============================================================================
# Each location now has its own ACH drawn from a truncated normal. Riskiness
# is derived from that ACH via Wells-Riley + median normalization (riskiness
# distribution centered near 1.0). No intervention applied.

cat("\n========== 2: setting-specific ACH on ==========\n")
params_2 <- make_base_params() %>% add_setting_specific_ach()
out_2    <- run_simulation(params_2)
plot_seir(out_2, "2: ACH on, no intervention")

# Inspect the drawn distributions:
v_2 <- create_variables(params_2)$parameters_list
cat("Workplace ACH summary:\n");       print(summary(v_2$workplace_specific_ach))
cat("Workplace riskiness summary:\n"); print(summary(v_2$workplace_specific_riskiness))

# TRY: change mean=4.8 to mean=0.5 for workplace -> very stuffy workplaces.
# TRY: omit one setting's set_setting_specific_ach() call -> that setting
#      falls back to uniform riskiness=1 (matches main's "off" behavior).


# =============================================================================
# Section 3 — Constant-delta intervention, joint coverage
# =============================================================================
# Same delta applied to every covered location. The 50% efficacy is a
# *calibration target*, not a guarantee — actual per-location efficacy
# varies with each location's baseline ACH.

cat("\n========== 3: constant-delta intervention ==========\n")
target_eff_3 <- 0.5
delta_3      <- efficacy_to_delta(
  target_efficacy = target_eff_3, baseline_ach = 4.0, V = 15
)
cat(sprintf("Calibrated delta for %.0f%% target efficacy: %.2f eACH\n",
            100 * target_eff_3, delta_3))

intv_3 <- make_intervention(
  name                     = "constant_uvc",
  affected_by_baseline_ach = FALSE,
  baseline_ach_function    = local({d <- delta_3; function() d}),
  baseline_ach_params      = list(),
  coverage                 = 0.5
)
params_3 <- make_base_params() %>% add_setting_specific_ach() %>%
  set_intervention_ach(
    setting         = "joint",
    coverage_target = "square_footage",
    coverage_type   = "random",
    timestep        = 0,
    intv_3
  )
out_3 <- run_simulation(params_3)
plot_seir(out_3, sprintf("3: Constant delta=%.1f, joint, 50%% cov, random",
                         delta_3))

# Inspect realized per-location efficacy:
v_3 <- create_variables(params_3)$parameters_list
for (s in c("workplace", "school", "leisure")) {
  eff <- v_3[[paste0(s, "_specific_efficacy")]]
  cov <- v_3[[paste0("intervention_", s, "_covered")]]
  cat(sprintf("%-9s mean efficacy over covered locations: %.3f\n",
              s, mean(eff[cov == 1])))
}



# =============================================================================
# Section 4 — Baseline-ACH-dependent intervention (delta scales with ACH)
# =============================================================================
# Models "improve ventilation by X% of baseline": delta = boost * ach.
# Tests the affected_by_baseline_ach = TRUE code path. Locations with higher
# baseline ACH get a bigger absolute delta added.

cat("\n========== 4: baseline-ACH-dependent intervention ==========\n")
intv_4 <- make_intervention(
  name                     = "ventilation_upgrade",
  affected_by_baseline_ach = TRUE,
  baseline_ach_function    = function(ach, boost) ach * boost,
  baseline_ach_params      = list(boost = 0.5),
  coverage                 = 0.5
)
params_4 <- make_base_params() %>% add_setting_specific_ach() %>%
  set_intervention_ach(
    setting         = "joint",
    coverage_target = "square_footage",
    coverage_type   = "random",
    timestep        = 0,
    intv_4
  )
out_4 <- run_simulation(params_4)
plot_seir(out_4, "4: delta = 0.5 * baseline ACH, joint, 50% cov")



# =============================================================================
# Section 5 — Intervention with unit-to-unit variation (HEPA-like)
# =============================================================================
# Constant base delta + normal noise per location. Models HEPA filters whose
# real-world effectiveness varies with filter age, fan setting, placement.

cat("\n========== 5: intervention with variation ==========\n")
intv_5 <- make_intervention(
  name                     = "hepa_with_variation",
  affected_by_baseline_ach = FALSE,
  baseline_ach_function    = function() 3,
  baseline_ach_params      = list(),
  variation                = TRUE,
  variation_function       = rnorm,
  variation_params         = list(mean = 0, sd = 0.5),
  coverage                 = 0.5
)
params_5 <- make_base_params() %>% add_setting_specific_ach() %>%
  set_intervention_ach(
    setting         = "joint",
    coverage_target = "square_footage",
    coverage_type   = "random",
    timestep        = 0,
    intv_5
  )
out_5 <- run_simulation(params_5)
plot_seir(out_5, "5: HEPA delta=3, noise sd=0.5, joint, 50% cov")



# =============================================================================
# Section 6 — Per-setting interventions instead of joint
# =============================================================================
# Different interventions in different settings. Joint mode is a single
# pooled budget; per-setting mode lets you mix and match independently.

cat("\n========== 6: per-setting interventions ==========\n")
intv_wp <- make_intervention(
  name = "uvc_workplace",
  affected_by_baseline_ach = FALSE,
  baseline_ach_function = function() 4,
  baseline_ach_params = list(), coverage = 0.6
)
intv_sc <- make_intervention(
  name = "uvc_school",
  affected_by_baseline_ach = FALSE,
  baseline_ach_function = function() 2,
  baseline_ach_params = list(), coverage = 0.8
)
params_6 <- make_base_params() %>% add_setting_specific_ach() %>%
  set_intervention_ach(
    setting = "workplace", coverage_target = "square_footage",
    coverage_type = "random", timestep = 0, intv_wp
  ) %>%
  set_intervention_ach(
    setting = "school", coverage_target = "square_footage",
    coverage_type = "targeted_riskiness", timestep = 0, intv_sc
  )
out_6 <- run_simulation(params_6)
plot_seir(out_6, "6: Per-setting (workplace 60% rand + school 80% targ)")



# =============================================================================
# Section 7 — Side-by-side: random vs targeted coverage
# =============================================================================
# Visualize the targeted coverage advantage. Same intervention, same
# coverage; only difference is how the covered locations are chosen.

cat("\n========== 7: random vs targeted ==========\n")
make_intv <- function() {
  make_intervention(
    name = "comparison",
    affected_by_baseline_ach = FALSE,
    baseline_ach_function = local({d <- delta_3; function() d}),
    baseline_ach_params = list(),
    coverage = 0.5
  )
}
params_7r <- make_base_params() %>% add_setting_specific_ach() %>%
  set_intervention_ach(setting="joint", coverage_target="square_footage",
                       coverage_type="random", timestep=0, make_intv())
params_7t <- make_base_params() %>% add_setting_specific_ach() %>%
  set_intervention_ach(setting="joint", coverage_target="square_footage",
                       coverage_type="targeted_riskiness", timestep=0, make_intv())
out_7r <- run_simulation(params_7r)
out_7t <- run_simulation(params_7t)

cat(sprintf("Random   final epidemic size: %.1f%%\n",
            tail(out_7r$R_count, 1) / 10000 * 100))
cat(sprintf("Targeted final epidemic size: %.1f%%\n",
            tail(out_7t$R_count, 1) / 10000 * 100))

