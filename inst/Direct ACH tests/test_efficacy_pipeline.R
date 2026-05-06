library(dplyr)
library(ggplot2)
library(tidyr)
library(truncnorm)

devtools::load_all()

#Unit Tests

wr_params <- list(
  wells_riley_emission_rate          = 27,      # pi, FFU/hour
  wells_riley_decay_rate             = 0.64,    # kD, hr^-1
  wells_riley_infection_prob_per_ffu = 1.37e-2, # r
  wells_riley_respiratory_rate_factor = 0.45,   # RRtv
  wells_riley_time_in_room           = 4,       # t, hours
  volume_per_person_workplace        = 27       # V, m^3 per person
)

# A range of ACH values covering low-to-high ventilation
ach_values <- c(1, 2, 4, 6, 8, 10)

# Helper: factory that produces a zero-argument function returning a fixed delta.
# Using a factory avoids R closure-capture gotchas when building functions in loops.
make_const_fn <- function(d) function() d

# ── Test 21: Empty intervention list -> efficacy = 0 everywhere ───────────────
cat("Test 21: Empty intervention list -> efficacy = 0\n")

params_21 <- c(wr_params, list(intervention_workplace_list = list()))
eff_21    <- calculate_efficacy_from_ach(ach_values, params_21, "workplace")

stopifnot("efficacy should be 0 when no interventions" = all(abs(eff_21) < 1e-10))
cat("  PASS\n\n")


# ── Test 22: affected_by_baseline_ach = FALSE -> uniform delta ────────────────
# All locations receive the same delta regardless of their ACH value.
# Because delta is the same but baseline ACH differs, efficacy still varies
# across locations — specifically, higher baseline ACH yields LOWER efficacy
# (diminishing returns). We verify the monotone-decreasing pattern.

cat("Test 22: affected_by_baseline_ach = FALSE -> uniform delta, efficacy\n",
    "         decreases as baseline ACH increases\n")

intv_22 <- make_intervention(
  name                   = "constant_ach",
  affected_by_baseline_ach = FALSE,
  baseline_ach_function  = make_const_fn(5),
  baseline_ach_params    = list()
)
params_22 <- c(wr_params, list(intervention_workplace_list = list(intv_22)))
eff_22    <- calculate_efficacy_from_ach(ach_values, params_22, "workplace")

stopifnot("output length matches input" = length(eff_22) == length(ach_values))
stopifnot("efficacy strictly decreases as baseline ACH increases" = all(diff(eff_22) < 0))
cat("  PASS\n\n")


# ── Test 23: affected_by_baseline_ach = TRUE -> delta varies by location ──────
# Delta is a function of the location's baseline ACH. Output should still be
# a vector of length n with all values in [0, 1).

cat("Test 23: affected_by_baseline_ach = TRUE -> delta varies, output length OK\n")

intv_23 <- make_intervention(
  name                   = "ach_dependent",
  affected_by_baseline_ach = TRUE,
  baseline_ach_function  = function(ach) ach * 0.5  # delta = half of baseline ACH
)
params_23 <- c(wr_params, list(intervention_workplace_list = list(intv_23)))
eff_23    <- calculate_efficacy_from_ach(ach_values, params_23, "workplace")

stopifnot("output length matches input" = length(eff_23) == length(ach_values))
stopifnot("all efficacy values >= 0"    = all(eff_23 >= 0))
stopifnot("all efficacy values < 1"     = all(eff_23 < 1))
cat("  PASS\n\n")


# ── Test 24: Two interventions -> total delta is additive ────────────────────
# Running intervention A (delta=3) and B (delta=4) separately and together
# should give the same efficacy as a single intervention with delta=7.
# This verifies the loop in calculate_efficacy_from_ach sums correctly.

cat("Test 24: Two additive interventions -> same result as single delta = sum\n")

intv_a        <- make_intervention("A", baseline_ach_function = make_const_fn(3))
intv_b        <- make_intervention("B", baseline_ach_function = make_const_fn(4))
intv_combined <- make_intervention("AB", baseline_ach_function = make_const_fn(7))

params_both <- c(wr_params, list(intervention_workplace_list = list(intv_a, intv_b)))
params_comb <- c(wr_params, list(intervention_workplace_list = list(intv_combined)))

eff_both <- calculate_efficacy_from_ach(ach_values, params_both, "workplace")
eff_comb <- calculate_efficacy_from_ach(ach_values, params_comb, "workplace")

stopifnot("two interventions identical to single combined delta" =
            all(abs(eff_both - eff_comb) < 1e-10))
cat("  PASS\n\n")


# ── Test 25: All efficacy values are in [0, 1) ───────────────────────────────
# Using UV-222 parameters as a realistic intervention.

cat("Test 25: All efficacy values in [0, 1)\n")

uv222_delta <- uv_to_delta(f = 1, E_avg = 1, k = 4.22)   # ~15.2 eACH
intv_25 <- make_intervention("uv222", baseline_ach_function = make_const_fn(uv222_delta))
params_25 <- c(wr_params, list(intervention_workplace_list = list(intv_25)))
eff_25    <- calculate_efficacy_from_ach(ach_values, params_25, "workplace")

stopifnot("all efficacy values >= 0" = all(eff_25 >= 0))
stopifnot("all efficacy values < 1"  = all(eff_25 < 1))
cat("  PASS\n\n")


# ── Test 26: delta = 0 -> efficacy = 0 ───────────────────────────────────────
# If the intervention adds nothing, p_pre == p_post, so efficacy must be zero.

cat("Test 26: delta = 0 -> efficacy = 0\n")

intv_26 <- make_intervention("zero", baseline_ach_function = make_const_fn(0))
params_26 <- c(wr_params, list(intervention_workplace_list = list(intv_26)))
eff_26    <- calculate_efficacy_from_ach(ach_values, params_26, "workplace")

stopifnot("efficacy is exactly 0 when delta = 0" = all(abs(eff_26) < 1e-10))
cat("  PASS\n\n")


# ── Test 27: Very large delta -> efficacy approaches but never reaches 1 ──────
# The Wells-Riley equation is exponential: p_post -> 0 but never hits 0,
# so efficacy -> 1 but never hits 1.

cat("Test 27: Very large delta -> efficacy close to 1 but strictly < 1\n")

intv_27 <- make_intervention("huge", baseline_ach_function = make_const_fn(1e6))
params_27 <- c(wr_params, list(intervention_workplace_list = list(intv_27)))
eff_27    <- calculate_efficacy_from_ach(ach_values, params_27, "workplace")

stopifnot("efficacy is strictly < 1 even for huge delta" = all(eff_27 < 1))
stopifnot("efficacy is close to 1 for huge delta"        = all(eff_27 > 0.999))
cat("  PASS\n\n")


# ── Test 28: Higher baseline ACH -> lower efficacy (diminishing returns) ──────
# This is the key policy-relevant finding: the more ventilated a space already
# is, the less additional benefit a fixed intervention provides.

cat("Test 28: Higher baseline ACH -> lower efficacy for same intervention\n")

ach_range <- c(0.5, 1, 3, 6, 10, 20)
intv_28   <- make_intervention("fixed", baseline_ach_function = make_const_fn(5))
params_28 <- c(wr_params, list(intervention_workplace_list = list(intv_28)))
eff_28    <- calculate_efficacy_from_ach(ach_range, params_28, "workplace")

stopifnot("efficacy strictly decreases as baseline ACH increases" = all(diff(eff_28) < 0))
cat("  PASS\n\n")


# ── Test 29: Larger delta -> higher efficacy at fixed baseline ACH ────────────
# The larger the intervention, the more it reduces infection probability.

cat("Test 29: Larger delta -> higher efficacy at same baseline ACH\n")

deltas <- c(1, 3, 5, 10, 20)
eff_29 <- sapply(deltas, function(d) {
  p <- c(wr_params, list(intervention_workplace_list = list(
    make_intervention("x", baseline_ach_function = make_const_fn(d))
  )))
  calculate_efficacy_from_ach(4, p, "workplace")  # fixed ACH = 4
})

stopifnot("efficacy strictly increases with delta" = all(diff(eff_29) > 0))
cat("  PASS\n\n")


cat("=== Tests 21-29 COMPLETE ===\n\n\n")

cat("======================================================\n")
cat("  INTEGRATION TESTS: single intervention (37-42)\n")
cat("======================================================\n\n")

# ── Shared baseline parameters ────────────────────────────────────────────────
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

params_baseline <- base_params %>%
  set_setting_specific_ach("workplace", mean = 4.8, sd = 1.5) %>%
  set_setting_specific_ach("school",    mean = 4.0, sd = 1.2) %>%
  set_setting_specific_ach("leisure",   mean = 3.0, sd = 1.0) %>%
  set_setting_specific_ach("household", mean = 0.5, sd = 0.2)


# ── Test 37: set_intervention_ach -> flags set correctly ─────────────────────
# Verify that set_intervention_ach correctly marks the setting as active and
# stores the intervention list in the right slot.

cat("Test 37: set_intervention_ach -> active flag and list stored correctly\n")

uv222_intv <- make_intervention(
  name                   = "uv222_workplace",
  affected_by_baseline_ach = FALSE,
  baseline_ach_function  = make_const_fn(uv_to_delta(f = 1, E_avg = 1, k = 4.22))
)

params_intv <- params_baseline %>%
  set_intervention_ach(
    setting         = "workplace",
    coverage_target = "individuals",
    coverage_type   = "random",
    timestep        = 0,
    uv222_intv
  )

stopifnot("active flag is TRUE"         = isTRUE(params_intv$intervention_workplace_active))
stopifnot("intervention list is stored" = is.list(params_intv$intervention_workplace_list))
stopifnot("one intervention in list"    = length(params_intv$intervention_workplace_list) == 1)
stopifnot("intervention name correct"   =
            params_intv$intervention_workplace_list[[1]]$name == "uv222_workplace")
cat("  PASS\n\n")


# ── Test 38: calculate_efficacy_from_ach on real ACH distribution ────────────
# After create_variables() draws per-location ACH values, passing them to
# calculate_efficacy_from_ach should return a vector of the same length.

cat("Test 38: calculate_efficacy_from_ach returns correct-length vector\n")

vars_intv        <- create_variables(params_intv)
params_populated <- vars_intv$parameters_list
workplace_ach    <- params_populated$workplace_specific_ach

eff_38 <- calculate_efficacy_from_ach(workplace_ach, params_intv, "workplace")

stopifnot("output length matches number of workplaces" =
            length(eff_38) == length(workplace_ach))
stopifnot("all values >= 0"  = all(eff_38 >= 0))
stopifnot("all values < 1"   = all(eff_38 < 1))
cat("  PASS\n\n")


# ── Test 39: UV-222 efficacy is plausible at typical workplace ventilation ────
# UV-222 adds ~15.2 eACH. At typical workplace ACH ~4.8, the Wells-Riley model
# predicts roughly 60-80% efficacy. We use a wide window (30-95%) to be
# robust to parameter variation while still catching implausible results.

cat("Test 39: UV-222 efficacy is plausible at typical workplace ACH\n")

uv222_delta_val <- uv_to_delta(f = 1, E_avg = 1, k = 4.22)
cat("  UV-222 eACH delta:", round(uv222_delta_val, 2), "\n")
cat("  Median workplace ACH:", round(median(workplace_ach), 2), "\n")
cat("  Median efficacy:", round(median(eff_38) * 100, 1), "%\n")
cat("  Range: [", round(min(eff_38) * 100, 1), "% ,",
    round(max(eff_38) * 100, 1), "%]\n")

stopifnot("median efficacy > 30%"  = median(eff_38) > 0.30)
stopifnot("median efficacy < 95%"  = median(eff_38) < 0.95)
stopifnot("higher ACH -> lower efficacy in workplace distribution" =
            cor(workplace_ach, eff_38) < 0)  # negative correlation expected
cat("  PASS\n\n")


# ── Test 40: run_simulation runs without error with intervention ──────────────

cat("Test 40: run_simulation completes without error with one intervention\n")

output_intv <- run_simulation(params_intv)

stopifnot("simulation output is a data frame"    = is.data.frame(output_intv))
stopifnot("simulation has expected columns"      =
            all(c("S_count", "E_count", "I_count", "R_count") %in% names(output_intv)))
stopifnot("simulation ran for correct timesteps" =
            nrow(output_intv) == base_params$simulation_time / base_params$dt)
cat("  PASS\n\n")


# ── Test 41: Intervention reduces cases relative to baseline ─────────────────
# UV-222 in the workplace should reduce cumulative infections. We compare
# the final Recovered count (= total ever-infected) between runs.

cat("Test 41: UV-222 workplace intervention reduces cumulative infections\n")

output_baseline <- run_simulation(params_baseline)

final_R_baseline   <- tail(output_baseline$R_count, 1)
final_R_intv       <- tail(output_intv$R_count,     1)
attack_rate_base   <- round(final_R_baseline / base_params$human_population * 100, 1)
attack_rate_intv   <- round(final_R_intv     / base_params$human_population * 100, 1)

cat("  Attack rate — baseline:    ", attack_rate_base, "%\n")
cat("  Attack rate — intervention:", attack_rate_intv, "%\n")
cat("  Reduction:", attack_rate_base - attack_rate_intv, "pp\n")

stopifnot("intervention reduces cumulative infections" = final_R_intv < final_R_baseline)
cat("  PASS\n\n")


# ── Test 42: Comparison plot — baseline vs intervention SEIR ─────────────────
# Visual confirmation that the two epidemic curves separate in the expected
# direction. No stopifnot here — inspect the plot.

cat("Test 42: Plotting baseline vs intervention SEIR curves\n")

seir_compare <- bind_rows(
  output_baseline %>%
    mutate(scenario = "Baseline (no intervention)"),
  output_intv %>%
    mutate(scenario = "UV-222 in workplace")
) %>%
  select(timestep, scenario, I_count, R_count) %>%
  pivot_longer(cols = c(I_count, R_count),
               names_to = "compartment", values_to = "count") %>%
  mutate(
    compartment = recode(compartment,
                         "I_count" = "Infectious",
                         "R_count" = "Recovered (cumulative)"),
    scenario = factor(scenario,
                      levels = c("Baseline (no intervention)", "UV-222 in workplace"))
  )

p_compare <- ggplot(seir_compare, aes(x = timestep, y = count,
                                       color = scenario, linetype = scenario)) +
  geom_line(linewidth = 1) +
  facet_wrap(~compartment, scales = "free_y") +
  scale_color_manual(values = c("Baseline (no intervention)" = "steelblue",
                                "UV-222 in workplace"        = "firebrick3")) +
  scale_linetype_manual(values = c("Baseline (no intervention)" = "solid",
                                   "UV-222 in workplace"        = "dashed")) +
  labs(
    title    = "Baseline vs UV-222 intervention (workplace only)",
    subtitle = paste0("UV-222 eACH delta = ", round(uv222_delta_val, 1),
                      " | Attack rate: baseline ", attack_rate_base,
                      "% vs intervention ", attack_rate_intv, "%"),
    x        = "Timestep",
    y        = "Number of individuals",
    color    = "Scenario",
    linetype = "Scenario"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_compare)
cat("  Plot printed — check that intervention curve is below baseline.\n\n")


cat("=== Tests 37-42 COMPLETE ===\n\n")
cat("All tests passed.\n")
