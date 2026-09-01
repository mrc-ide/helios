## ---------------------------------------------------------------------------
## Walkthrough of the ACH / intervention pipeline introduced in PR #142
##
## Steps through every function the PR adds, checking behaviour at each stage,
## and finishes with three SARS-CoV-2 epidemic runs: no intervention, a 75%
## efficacy intervention at 75% coverage, and a fixed-output device calibrated
## to the same nominal efficacy.
##
## Run with:  Rscript inst/ach_pipeline_walkthrough.R
## Takes ~2 minutes at POP = 10000.
## ---------------------------------------------------------------------------

suppressMessages(library(helios))

POP      <- 10000
SIM_TIME <- 150
SEED     <- 1

# generate_setting_specific_ach() and convert_ach_to_riskiness() are not
# exported (no @export tag), so reach them through the namespace.
generate_setting_specific_ach <- helios:::generate_setting_specific_ach
convert_ach_to_riskiness      <- helios:::convert_ach_to_riskiness

# --- check helper ------------------------------------------------------------
.n_pass <- 0L
.n_fail <- 0L
check <- function(label, expr) {
  ok <- isTRUE(tryCatch(expr, error = function(e) FALSE))
  if (ok) .n_pass <<- .n_pass + 1L else .n_fail <<- .n_fail + 1L
  cat(sprintf("  [%s] %s\n", if (ok) "PASS" else "FAIL", label))
  invisible(ok)
}
section <- function(title) cat(sprintf("\n== %s %s\n", title, strrep("=", max(0, 68 - nchar(title)))))

base_params <- function(seed = SEED, sim_time = SIM_TIME) {
  get_parameters(
    overrides = list(
      human_population = POP,
      number_initial_S = POP - 10,
      number_initial_E = 10,
      number_initial_I = 0,
      number_initial_R = 0,
      simulation_time  = sim_time,
      seed             = seed
    ),
    archetype = "sars_cov_2"
  )
}

# ACH assumptions used throughout. Note these are NOT defaults in
# get_parameters() -- all eight slots are NULL and must be set explicitly.
ACH <- list(
  workplace = c(mean = 4.8, sd = 1.5),
  school    = c(mean = 4.0, sd = 1.2),
  leisure   = c(mean = 3.0, sd = 1.0),
  household = c(mean = 0.5, sd = 0.2)
)
with_heterogeneous_ach <- function(p) {
  for (s in names(ACH)) p <- set_setting_specific_ach(p, s, ACH[[s]]["mean"], ACH[[s]]["sd"])
  p
}
with_uniform_ach <- function(p, ach = 4) {
  for (s in names(ACH)) p <- set_default_ach(p, s, ach)
  p
}


## ===========================================================================
section("1. set_default_ach(): uniform baseline ventilation")
## ===========================================================================

p <- set_default_ach(base_params(), "workplace", ach = 4)
check("sets default_ach_workplace", identical(p$default_ach_workplace, 4))
check("leaves the setting-specific switch off", isFALSE(p$setting_specific_ach_workplace))
check("rejects an unknown setting",
      inherits(try(set_default_ach(p, "hospital", 4), silent = TRUE), "try-error"))
check("rejects a negative ACH",
      inherits(try(set_default_ach(p, "workplace", -1), silent = TRUE), "try-error"))
check("rejects a vector ACH",
      inherits(try(set_default_ach(p, "workplace", c(1, 2)), silent = TRUE), "try-error"))


## ===========================================================================
section("2. generate_setting_specific_ach(): per-location baseline ACH")
## ===========================================================================

# No silent default: an unconfigured setting is a hard error. This is the
# behaviour change that breaks every pre-PR script and vignette.
check("errors when a setting has no ACH configured",
      inherits(try(generate_setting_specific_ach(base_params(), "workplace", 50),
                   silent = TRUE), "try-error"))

uniform_ach <- generate_setting_specific_ach(with_uniform_ach(base_params(), 4), "workplace", 50)
check("uniform path returns one value per location", length(uniform_ach) == 50)
check("uniform path returns the requested ACH everywhere", all(uniform_ach == 4))

p_het <- with_heterogeneous_ach(base_params())
het_ach <- generate_setting_specific_ach(p_het, "workplace", 5000)
check("heterogeneous path returns one value per location", length(het_ach) == 5000)
check("heterogeneous path is non-degenerate", sd(het_ach) > 0)
check("heterogeneous path is strictly positive", all(het_ach > 0))
check("realised mean is close to the requested mean", abs(mean(het_ach) - 4.8) < 0.1)
cat(sprintf("       workplace ACH draw: mean %.2f  sd %.2f  range %.2f-%.2f\n",
            mean(het_ach), sd(het_ach), min(het_ach), max(het_ach)))
# NB rtruncnorm's mean/sd are the PARENT normal's parameters. Fine here
# (sd/mean = 0.31); the realised mean drifts once sd/mean gets large.


## ===========================================================================
section("3. convert_ach_to_riskiness(): ACH -> relative FOI multiplier")
## ===========================================================================

risk <- convert_ach_to_riskiness(het_ach, p_het, "workplace")
check("one riskiness value per location", length(risk) == length(het_ach))
check("all riskiness values positive", all(risk > 0))
check("anchored so the median location has riskiness 1", abs(median(risk) - 1) < 1e-6)
check("monotonically decreasing in ACH", cor(het_ach, risk, method = "spearman") < -0.99)
check("uniform ACH gives riskiness identically 1",
      all(convert_ach_to_riskiness(uniform_ach, p_het, "workplace") == 1))

cat(sprintf("       riskiness: mean %.3f  median %.3f  q99 %.2f  max %.2f\n",
            mean(risk), median(risk), quantile(risk, 0.99), max(risk)))
cat(sprintf("       NOTE mean riskiness is %.3f, not 1 -- see 'known issues' at the end.\n",
            mean(risk)))


## ===========================================================================
section("4. Wells-Riley helper functions")
## ===========================================================================

# uv_to_delta: eACH from far-UVC photophysics. The 3.6 constant assumes
# E_avg in microW/cm^2 and k in cm^2/mJ (the roxygen says mW/cm^2).
d_uvc <- uv_to_delta(f = 0.5, E_avg = 3, k = 0.6)
check("uv_to_delta returns a plausible far-UVC eACH (1-10 /h)", d_uvc > 1 && d_uvc < 10)
check("uv_to_delta scales linearly in fluence",
      abs(uv_to_delta(0.5, 6, 0.6) - 2 * d_uvc) < 1e-9)
cat(sprintf("       f=0.5, E=3 uW/cm2, k=0.6 cm2/mJ -> %.2f eACH\n", d_uvc))

check("ach_to_efficacy is 0 when delta is 0", ach_to_efficacy(4, delta = 0, V = 27) == 0)
check("ach_to_efficacy increases with delta",
      ach_to_efficacy(4, 10, V = 27) > ach_to_efficacy(4, 2, V = 27))
check("ach_to_efficacy decreases with baseline ACH",
      ach_to_efficacy(10, 5, V = 27) < ach_to_efficacy(2, 5, V = 27))

d75 <- efficacy_to_delta(target_efficacy = 0.75, baseline_ach = 4, V = 15)
check("efficacy_to_delta inverts ach_to_efficacy",
      abs(ach_to_efficacy(4, d75, V = 15) - 0.75) < 1e-9)
cat(sprintf("       delta for 75%% efficacy at ACH 4: %.2f eACH\n", d75))

# The W-R constants nearly cancel: efficacy is essentially delta / (ACH+kD+delta).
v_sens <- sapply(c(8, 15, 27), function(v) efficacy_to_delta(0.75, 4, V = v))
check("delta is insensitive to volume per person (<1% across V = 8-27)",
      diff(range(v_sens)) / mean(v_sens) < 0.01)


## ===========================================================================
section("5. make_intervention() + calculate_efficacy_from_ach()")
## ===========================================================================

ach_test <- c(1, 2, 4, 8, 16)
p_test   <- with_uniform_ach(base_params(), 4)

# (a) constant delta, full coverage
intv_const <- make_intervention(
  name           = "constant_delta",
  delta_function = function(delta) delta,
  delta_params   = list(delta = 5),
  coverage       = 1
)
p_a <- set_intervention_ach(p_test, "workplace", "individuals", "random", 0, intv_const)
p_a$intervention_workplace_covered <- rep(1, length(ach_test))
eff_a <- calculate_efficacy_from_ach(ach_test, p_a, "workplace")
check("constant delta: one efficacy per location", length(eff_a) == length(ach_test))
check("constant delta: all efficacies in [0, 1)", all(eff_a >= 0 & eff_a < 1))
check("constant delta: efficacy falls as baseline ACH rises", all(diff(eff_a) < 0))
cat(sprintf("       ACH %s -> efficacy %s\n",
            paste(ach_test, collapse = " "),
            paste(sprintf("%.2f", eff_a), collapse = " ")))

# (b) uncovered locations must get exactly zero
p_b <- p_a
p_b$intervention_workplace_covered <- c(1, 0, 1, 0, 1)
eff_b <- calculate_efficacy_from_ach(ach_test, p_b, "workplace")
check("uncovered locations get exactly zero efficacy", all(eff_b[c(2, 4)] == 0))
check("covered locations are unchanged by the coverage mask",
      all(abs(eff_b[c(1, 3, 5)] - eff_a[c(1, 3, 5)]) < 1e-12))

# (c) no intervention installed -> all zeros
p_c <- p_test
check("no intervention gives an all-zero efficacy vector",
      all(calculate_efficacy_from_ach(ach_test, p_c, "workplace") == 0))

# (d) delta that depends on the location's own baseline ACH
intv_acheff <- make_intervention(
  name                          = "target_75_everywhere",
  delta_depends_on_baseline_ach = TRUE,
  delta_function = function(ach, target, V) efficacy_to_delta(target, ach, V),
  delta_params   = list(target = 0.75, V = 15),
  coverage       = 1
)
p_d <- set_intervention_ach(p_test, "workplace", "individuals", "random", 0, intv_acheff)
p_d$intervention_workplace_covered <- rep(1, length(ach_test))
eff_d <- calculate_efficacy_from_ach(ach_test, p_d, "workplace")
check("ACH-dependent delta hits the 75% target at every ACH",
      all(abs(eff_d - 0.75) < 0.01))
cat(sprintf("       ACH-dependent delta -> efficacy %s\n",
            paste(sprintf("%.3f", eff_d), collapse = " ")))

# (e) unit-to-unit variation
set.seed(99)
intv_var <- make_intervention(
  name               = "with_variation",
  delta_function     = function() 3,
  variation          = TRUE,
  variation_function = rnorm,
  variation_params   = list(mean = 0, sd = 4),
  coverage           = 1
)
p_e <- set_intervention_ach(p_test, "workplace", "individuals", "random", 0, intv_var)
p_e$intervention_workplace_covered <- rep(1, 2000)
eff_e <- calculate_efficacy_from_ach(rep(4, 2000), p_e, "workplace")
check("variation produces location-to-location spread", sd(eff_e) > 0)
check("variation never produces a negative efficacy", all(eff_e >= 0))
# delta_i is floored at zero, so mean delta drifts above the intended value
# whenever sd is an appreciable fraction of delta.
realised_delta <- eff_e * 4.64 / (1 - eff_e)
cat(sprintf("       intended mean delta 3.00, realised %.2f (%+.0f%%); %.0f%% of locations floored at zero\n",
            mean(realised_delta), 100 * (mean(realised_delta) / 3 - 1),
            100 * mean(realised_delta < 1e-9)))


## ===========================================================================
section("6. set_intervention_ach() + coverage allocation")
## ===========================================================================

check("rejects more than one setting at a time",
      inherits(try(set_intervention_ach(p_test, c("workplace", "school"), "individuals",
                                        "random", 0, intv_const), silent = TRUE), "try-error"))
check("rejects an unknown setting",
      inherits(try(set_intervention_ach(p_test, "hospital", "individuals", "random", 0,
                                        intv_const), silent = TRUE), "try-error"))
check("rejects an unknown coverage_target",
      inherits(try(set_intervention_ach(p_test, "workplace", "buildings", "random", 0,
                                        intv_const), silent = TRUE), "try-error"))
check("rejects an unknown coverage_type",
      inherits(try(set_intervention_ach(p_test, "workplace", "individuals", "weak", 0,
                                        intv_const), silent = TRUE), "try-error"))

# Per-setting, random coverage
intv_50 <- make_intervention("cov50", delta_function = function() 7, coverage = 0.5)
p_ws <- set_intervention_ach(with_heterogeneous_ach(base_params()), "workplace",
                             "individuals", "random", 0, intv_50)
v_ws <- suppressMessages(create_variables(p_ws))
sz   <- get_setting_size(v_ws$variables_list, "workplace")
cov  <- v_ws$parameters_list$intervention_workplace_covered
realised <- sum(sz[cov == 1]) / sum(sz)
check("random coverage vector has one entry per location", length(cov) == length(sz))
check("random coverage vector is 0/1", all(cov %in% c(0, 1)))
check("realised coverage meets the 50% target (to within one location)",
      realised >= 0.5 && realised <= 0.5 + max(sz) / sum(sz))
cat(sprintf("       workplace random: %d/%d locations, %.1f%% of individuals\n",
            sum(cov), length(cov), 100 * realised))

# Per-setting, riskiness-targeted coverage
p_wt <- set_intervention_ach(with_heterogeneous_ach(base_params()), "workplace",
                             "individuals", "targeted_riskiness", 0, intv_50)
v_wt <- suppressMessages(create_variables(p_wt))
cov_t  <- v_wt$parameters_list$intervention_workplace_covered
ach_t  <- v_wt$parameters_list$workplace_specific_ach
check("targeted coverage selects the lower-ACH (riskier) locations",
      mean(ach_t[cov_t == 1]) < mean(ach_t[cov_t == 0]))
cat(sprintf("       workplace targeted: mean ACH covered %.2f vs uncovered %.2f\n",
            mean(ach_t[cov_t == 1]), mean(ach_t[cov_t == 0])))

# Joint deployment across workplace + school + leisure
p_j <- set_intervention_ach(with_heterogeneous_ach(base_params()), "joint",
                            "individuals", "random", 0, intv_50)
v_j  <- suppressMessages(create_variables(p_j))
pl_j <- v_j$parameters_list
tot_sz <- 0; tot_cov <- 0
for (s in c("workplace", "school", "leisure")) {
  szs <- if (s == "leisure") pl_j$setting_sizes$leisure else get_setting_size(v_j$variables_list, s)
  cvs <- pl_j[[paste0("intervention_", s, "_covered")]]
  tot_sz <- tot_sz + sum(szs); tot_cov <- tot_cov + sum(szs[cvs == 1])
}
check("joint coverage pools to the 50% target across the three settings",
      abs(tot_cov / tot_sz - 0.5) < 0.02)
check("joint deployment leaves households untouched",
      is.null(pl_j$household_specific_efficacy))
check("joint deployment rejects a simultaneous per-setting intervention", {
  p_clash <- set_intervention_ach(p_j, "household", "individuals", "random", 0, intv_50)
  inherits(try(suppressMessages(create_variables(p_clash)), silent = TRUE), "try-error")
})
cat(sprintf("       joint pooled coverage: %.1f%% of individuals\n", 100 * tot_cov / tot_sz))


## ===========================================================================
section("7. create_variables(): end-to-end wiring")
## ===========================================================================

pl <- v_j$parameters_list
n_workplaces <- max(as.numeric(v_j$variables_list$workplace$get_categories()))
n_schools    <- max(as.numeric(v_j$variables_list$school$get_categories()))
n_households <- max(as.numeric(v_j$variables_list$household$get_categories()))
n_leisure    <- length(pl$setting_sizes$leisure)

check("workplace ACH/riskiness/efficacy vectors match the workplace count",
      length(pl$workplace_specific_ach) == n_workplaces &&
      length(pl$workplace_specific_riskiness) == n_workplaces &&
      length(pl$workplace_specific_efficacy) == n_workplaces)
check("school vectors match the school count",
      length(pl$school_specific_ach) == n_schools &&
      length(pl$school_specific_efficacy) == n_schools)
check("household vectors match the household count",
      length(pl$household_specific_ach) == n_households &&
      length(pl$household_specific_riskiness) == n_households)

# Leisure IDs can have gaps (some sampled venues get no visitors), so check
# the efficacy vector lines up with the categories the FOI loop iterates over.
leis_cats <- v_j$variables_list$specific_leisure$get_categories()
leis_cats <- leis_cats[leis_cats != "0"]
check("leisure vectors match the number of visited leisure locations",
      length(leis_cats) == n_leisure && length(pl$leisure_specific_efficacy) == n_leisure)
check("leisure categories are in ascending order (FOI loop indexes positionally)",
      identical(as.numeric(leis_cats), sort(as.numeric(leis_cats))))
cat(sprintf("       %d workplaces, %d schools, %d leisure venues, %d households\n",
            n_workplaces, n_schools, n_leisure, n_households))

for (s in c("workplace", "school", "leisure")) {
  eff <- pl[[paste0(s, "_specific_efficacy")]]
  cvs <- pl[[paste0("intervention_", s, "_covered")]]
  check(sprintf("%s: efficacy is zero exactly where coverage is zero", s),
        all(eff[cvs == 0] == 0) && all(eff[cvs == 1] > 0))
  check(sprintf("%s: efficacy stays in [0, 1)", s), all(eff >= 0 & eff < 1))
}


## ===========================================================================
section("8. Epidemic runs: SARS-CoV-2, no intervention vs 75% / 75%")
## ===========================================================================

TARGET_EFFICACY <- 0.75
TARGET_COVERAGE <- 0.75

summarise_run <- function(out, label) {
  data.frame(
    arm       = label,
    peak_pct  = 100 * max(out$I_count) / POP,
    peak_day  = out$timestep[which.max(out$I_count)] * 0.5,
    final_pct = 100 * tail(out$R_count, 1) / POP
  )
}

# Arm 1: no intervention
cat("  running baseline ...\n")
out_base <- run_simulation(with_heterogeneous_ach(base_params()))

# Arm 2: delta set per location so realised efficacy is 75% everywhere
intv_exact <- make_intervention(
  name                          = "uvc_75pc_exact",
  delta_depends_on_baseline_ach = TRUE,
  delta_function = function(ach, target, V) efficacy_to_delta(target, ach, V),
  delta_params   = list(target = TARGET_EFFICACY, V = 15),
  coverage       = TARGET_COVERAGE
)
p_exact <- set_intervention_ach(with_heterogeneous_ach(base_params()), "joint",
                                "individuals", "random", 0, intv_exact)
v_exact <- suppressMessages(create_variables(p_exact))
eff_all <- unlist(lapply(c("workplace", "school", "leisure"), function(s) {
  e <- v_exact$parameters_list[[paste0(s, "_specific_efficacy")]]
  c <- v_exact$parameters_list[[paste0("intervention_", s, "_covered")]]
  e[c == 1]
}))
check("realised efficacy is 75% at every covered location",
      all(abs(eff_all - TARGET_EFFICACY) < 0.01))
cat("  running 75% efficacy at 75% coverage ...\n")
out_exact <- run_simulation(p_exact)

# Arm 3: a fixed-output device delivering a constant eACH, calibrated to give
# 75% efficacy at ACH 4. Realised efficacy then varies with local ventilation.
delta_fixed <- efficacy_to_delta(TARGET_EFFICACY, baseline_ach = 4, V = 15)
intv_fixed <- make_intervention(
  name           = "uvc_fixed_output",
  delta_function = function(delta) delta,
  delta_params   = list(delta = delta_fixed),
  coverage       = TARGET_COVERAGE
)
p_fixed <- set_intervention_ach(with_heterogeneous_ach(base_params()), "joint",
                                "individuals", "random", 0, intv_fixed)
v_fixed <- suppressMessages(create_variables(p_fixed))
eff_fx <- unlist(lapply(c("workplace", "school", "leisure"), function(s) {
  e <- v_fixed$parameters_list[[paste0(s, "_specific_efficacy")]]
  c <- v_fixed$parameters_list[[paste0("intervention_", s, "_covered")]]
  e[c == 1]
}))
cat(sprintf("  fixed device: %.2f eACH -> realised efficacy %.2f-%.2f (median %.2f)\n",
            delta_fixed, min(eff_fx), max(eff_fx), median(eff_fx)))
cat("  running fixed-output device at 75% coverage ...\n")
out_fixed <- run_simulation(p_fixed)

results <- rbind(
  summarise_run(out_base,  "no intervention"),
  summarise_run(out_exact, sprintf("%.0f%% efficacy, %.0f%% coverage",
                                   100 * TARGET_EFFICACY, 100 * TARGET_COVERAGE)),
  summarise_run(out_fixed, sprintf("fixed %.1f eACH, %.0f%% coverage",
                                   delta_fixed, 100 * TARGET_COVERAGE))
)
cat("\n")
print(results, row.names = FALSE, digits = 4)

check("the intervention reduces the peak", results$peak_pct[2] < results$peak_pct[1])
check("the intervention reduces the final size", results$final_pct[2] < results$final_pct[1])
cat(sprintf("\n  peak  %.1f%% -> %.1f%%  (%.0f%% reduction)\n",
            results$peak_pct[1], results$peak_pct[2],
            100 * (1 - results$peak_pct[2] / results$peak_pct[1])))
cat(sprintf("  final %.1f%% -> %.1f%%  (%.0f%% reduction)\n",
            results$final_pct[1], results$final_pct[2],
            100 * (1 - results$final_pct[2] / results$final_pct[1])))


## ===========================================================================
section("9. Known issues (reported, not asserted)")
## ===========================================================================

# Riskiness is anchored at the median ACH, so its mean sits above 1 and turning
# heterogeneous ACH on raises the mean FOI multiplier -- i.e. R0 -- relative to
# a uniform-ACH run at the same mean.
pl_base <- suppressMessages(create_variables(with_heterogeneous_ach(base_params())))$parameters_list
for (s in names(ACH)) {
  r <- pl_base[[paste0(s, "_specific_riskiness")]]
  cat(sprintf("  %-10s mean riskiness %.3f (median %.3f) -> mean FOI %+.1f%% vs uniform ACH\n",
              s, mean(r), median(r), 100 * (mean(r) - 1)))
}

# create_variables() is not re-runnable on its own output in joint mode: the
# joint path sets the per-setting _active flags, which the guard then rejects.
roundtrip <- tryCatch({ suppressMessages(create_variables(v_j$parameters_list)); "succeeds" },
                      error = function(e) paste("errors:", conditionMessage(e)))
cat(sprintf("\n  create_variables() re-run on its own joint-mode output: %s\n", roundtrip))


## ===========================================================================
cat(sprintf("\n%s\n%d passed, %d failed\n", strrep("=", 72), .n_pass, .n_fail))
if (.n_fail > 0) quit(status = 1)
