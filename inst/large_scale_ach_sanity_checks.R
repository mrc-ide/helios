# Large-scale (50k population, 8 replicates per scenario) sanity checks for
# the ACH/intervention rewrite on this branch. Not a testthat suite -- these
# are stochastic, population-level checks that the *direction* and rough
# *shape* of model outputs match intuition, not exact-value regression
# tests. Takes ~10-15 minutes wall time using a PSOCK cluster.
#
# Uses parallel::makeCluster()/parLapply() (PSOCK, separate processes)
# rather than mclapply(): forking a multithreaded process is a known
# crash/error source on macOS (especially from RStudio's GUI process), and
# mclapply() silently swallows per-worker errors as try-error objects. PSOCK
# workers are separate R sessions, so a worker error surfaces immediately.
#
# Uses the "flu" archetype (R0 ~ 1.5) rather than the raw/uncalibrated
# defaults, which otherwise produce unrealistically high attack rates.
#
# Checks covered:
#   A. Higher baseline ACH -> lower attack rate (monotonic, no intervention).
#      Uses set_setting_specific_ach() (heterogeneous truncated-normal),
#      not set_default_ach(): with a *uniform* ACH, riskiness is normalised
#      to the setting's own median and always collapses to 1, so the
#      absolute ACH level would have no effect at all.
#   B. Higher intervention coverage -> lower attack rate (monotonic)
#   C. Bigger intervention delta (eACH) -> lower attack rate (monotonic,
#      diminishing returns expected at high delta)
#   D. targeted_riskiness coverage outperforms (or matches) random coverage
#      at the same coverage level
#   E. A "joint" intervention (workplace+school+leisure pooled) reduces
#      those settings' FOI but leaves household FOI essentially untouched
#      (household is excluded from joint deployment by design)

# Run this from the package root (or open the helios .Rproj first) so that
# devtools::load_all(pkg_path) below can find the package.
pkg_path <- normalizePath(".")
devtools::load_all(pkg_path)

# Overridable via env vars for smoke-testing at small scale before committing
# to the full 50k/8-rep run, e.g.:
#   ACH_SANITY_N_REPS=2 ACH_SANITY_POP=500 ACH_SANITY_SIM_TIME=10 Rscript inst/large_scale_ach_sanity_checks.R
N_REPS    <- as.integer(Sys.getenv("ACH_SANITY_N_REPS", "8"))
POP       <- as.integer(Sys.getenv("ACH_SANITY_POP", "50000"))
SIM_TIME  <- as.integer(Sys.getenv("ACH_SANITY_SIM_TIME", "150"))
ARCHETYPE <- "flu"
N_WORKERS <- max(1, min(10, parallel::detectCores() - 1))

cl <- parallel::makeCluster(N_WORKERS)
parallel::clusterCall(cl, function(p) { devtools::load_all(p); NULL }, pkg_path)

# ---------------------------------------------------------------------------
# Scenario building blocks
# ---------------------------------------------------------------------------

base_params <- function(seed, extra_overrides = list()) {
  get_parameters(
    archetype = ARCHETYPE,
    overrides = c(
      list(
        human_population = POP,
        simulation_time  = SIM_TIME,
        number_initial_S = POP - 5,
        number_initial_E = 5,
        seed             = seed
      ),
      extra_overrides
    )
  )
}

# Uniform baseline ACH for every setting -- a deliberate "flat" baseline for
# the intervention checks (B/C/D/E), where what matters is the delta, not
# the absolute ACH level.
with_uniform_ach <- function(parameters_list, ach = 4) {
  for (setting in c("household", "workplace", "school", "leisure")) {
    parameters_list <- set_default_ach(parameters_list, setting, ach)
  }
  parameters_list
}

# Heterogeneous baseline ACH (truncated-normal, fixed sd) for every setting
# -- used by family A, which sweeps mean_ach and needs real cross-location
# spread for the riskiness normalisation to reflect the swept value.
with_setting_specific_ach <- function(parameters_list, mean_ach, sd_ach = 2) {
  for (setting in c("household", "workplace", "school", "leisure")) {
    parameters_list <- set_setting_specific_ach(parameters_list, setting, mean = mean_ach, sd = sd_ach)
  }
  parameters_list
}

# Installs a constant-delta intervention on `setting` (or no-ops at coverage
# 0). Used by B/C/D (setting = "workplace") and E (setting = "joint").
add_intervention <- function(parameters_list, setting, coverage, delta, coverage_type = "random") {
  if (coverage == 0) {
    return(parameters_list)
  }
  intervention <- make_intervention(
    name = "constant_delta",
    delta_function = function(d) d,
    delta_params = list(d = delta),
    coverage = coverage
  )
  set_intervention_ach(
    parameters_list = parameters_list,
    setting = setting,
    coverage_target = "individuals",
    coverage_type = coverage_type,
    timestep = 1,
    intervention = intervention
  )
}

# A: no intervention, sweep heterogeneous baseline ACH
build_A <- function(mean_ach, seed) {
  with_setting_specific_ach(base_params(seed), mean_ach = mean_ach)
}

# B/C/D: workplace intervention, sweep coverage and/or delta
build_BCD <- function(coverage, delta, seed, coverage_type = "random") {
  add_intervention(with_uniform_ach(base_params(seed)), "workplace", coverage, delta, coverage_type)
}

# E: joint intervention (workplace+school+leisure), diagnostics on so we can
# inspect per-setting FOI
build_E <- function(coverage, delta, seed) {
  parameters_list <- with_uniform_ach(base_params(seed, list(render_diagnostics = TRUE)))
  add_intervention(parameters_list, "joint", coverage, delta)
}

# ---------------------------------------------------------------------------
# Runner
# ---------------------------------------------------------------------------

# timestep column is just an integer step index (1..simulation_time/dt), not
# a day count -- multiply by dt to convert to days.
summarise_run <- function(out, population, dt) {
  list(
    attack_rate = 1 - tail(out$S_count, 1) / population,
    peak_I      = max(out$I_count),
    peak_day    = out$timestep[which.max(out$I_count)] * dt,
    mean_FOI_household = if ("FOI_household" %in% names(out)) mean(out$FOI_household) else NA,
    mean_FOI_workplace = if ("FOI_workplace" %in% names(out)) mean(out$FOI_workplace) else NA,
    mean_FOI_school     = if ("FOI_school" %in% names(out)) mean(out$FOI_school) else NA,
    mean_FOI_leisure    = if ("FOI_leisure" %in% names(out)) mean(out$FOI_leisure) else NA
  )
}

run_scenario <- function(label, build_fn, ..., n_reps = N_REPS, cluster = cl) {
  t0 <- Sys.time()
  cat(sprintf("  -> %s (%d reps, %d workers)... ", label, n_reps, length(cluster)))
  results <- parallel::parLapply(cluster, seq_len(n_reps), function(i, label, build_fn, dots) {
    seed <- 10000 * (abs(sum(utf8ToInt(label))) %% 97 + 1) + i
    parameters_list <- do.call(build_fn, c(dots, list(seed = seed)))
    out <- run_simulation(parameters_list)
    s <- summarise_run(out, parameters_list$human_population, parameters_list$dt)
    data.frame(scenario = label, rep = i, seed = seed, s)
  }, label = label, build_fn = build_fn, dots = list(...))
  cat(sprintf("done (%.0fs)\n", as.numeric(Sys.time() - t0, units = "secs")))
  do.call(rbind, results)
}

# PSOCK workers are separate R processes with only the package loaded (via
# clusterCall() above); they need every helper/constant used inside
# build_fn closures exported explicitly.
parallel::clusterExport(cl, varlist = c(
  "base_params", "with_uniform_ach", "with_setting_specific_ach", "add_intervention",
  "build_A", "build_BCD", "build_E", "summarise_run",
  "POP", "SIM_TIME", "ARCHETYPE"
))

# ---------------------------------------------------------------------------
# A. Baseline ACH sweep (no intervention)
# ---------------------------------------------------------------------------

cat("Running scenario family A: baseline ACH sweep...\n")
results_A <- do.call(rbind, lapply(c(1, 4, 10, 20), function(mean_ach) {
  run_scenario(paste0("A_ach_", mean_ach), build_A, mean_ach = mean_ach)
}))

# ---------------------------------------------------------------------------
# B. Coverage sweep (fixed delta = 5, random coverage, workplace)
# ---------------------------------------------------------------------------

cat("Running scenario family B: coverage sweep...\n")
results_B <- do.call(rbind, lapply(c(0, 0.25, 0.5, 0.75, 1.0), function(cov) {
  run_scenario(paste0("B_coverage_", cov), build_BCD, coverage = cov, delta = 5)
}))

# ---------------------------------------------------------------------------
# C. Delta sweep (fixed coverage = 1, random coverage, workplace)
# ---------------------------------------------------------------------------

cat("Running scenario family C: delta sweep...\n")
results_C <- do.call(rbind, lapply(c(0, 2, 10, 20), function(d) {
  run_scenario(paste0("C_delta_", d), build_BCD, coverage = 1, delta = d)
}))
# delta = 5 @ coverage = 1 already ran as part of family B -- reuse it.
results_C_delta5 <- subset(results_B, scenario == "B_coverage_1")
results_C_delta5$scenario <- "C_delta_5"
results_C <- rbind(results_C, results_C_delta5)

# ---------------------------------------------------------------------------
# D. targeted_riskiness vs random coverage (same coverage = 0.5, delta = 5)
# ---------------------------------------------------------------------------

cat("Running scenario family D: targeted vs random coverage...\n")
results_D_targeted <- run_scenario(
  "D_targeted", build_BCD,
  coverage = 0.5, delta = 5, coverage_type = "targeted_riskiness"
)
# random coverage @ 0.5 already ran as part of family B -- reuse it.
results_D_random <- subset(results_B, scenario == "B_coverage_0.5")
results_D_random$scenario <- "D_random"
results_D <- rbind(results_D_targeted, results_D_random)

# ---------------------------------------------------------------------------
# E. Joint intervention vs no intervention (household exclusion check)
# ---------------------------------------------------------------------------

cat("Running scenario family E: joint intervention / household exclusion...\n")
results_E <- rbind(
  run_scenario("E_none",  build_E, coverage = 0,   delta = 5),
  run_scenario("E_joint", build_E, coverage = 0.5, delta = 5)
)

parallel::stopCluster(cl)

# ---------------------------------------------------------------------------
# Save + summarise
# ---------------------------------------------------------------------------

all_results <- rbind(results_A, results_B, results_C, results_D, results_E)
out_suffix <- Sys.getenv("ACH_SANITY_OUT_SUFFIX", "")
saveRDS(all_results, paste0("/tmp/ach_sanity_check_results", out_suffix, ".rds"))
write.csv(all_results, paste0("/tmp/ach_sanity_check_results", out_suffix, ".csv"), row.names = FALSE)

summary_table <- aggregate(
  cbind(attack_rate, peak_I) ~ scenario,
  data = all_results,
  FUN = function(x) c(mean = mean(x), sd = sd(x))
)
print(summary_table)

cat("\nDone. Results saved to /tmp/ach_sanity_check_results.{rds,csv}\n")

# ---------------------------------------------------------------------------
# Visualization
# ---------------------------------------------------------------------------
# One panel per scenario family. Each plots attack_rate per replicate
# (jittered points) plus the mean +/- 1 SD across replicates, ordered by the
# swept parameter -- so a monotonic line is exactly what "intuitively makes
# sense" should look like for families A/B/C, and family D/E are direct
# two-group comparisons instead of a sweep.

library(ggplot2)

# Pull the swept numeric value back out of the scenario label (e.g.
# "B_coverage_0.5" -> 0.5) so points plot in the right order on the x-axis.
all_results$x_value <- as.numeric(sub("^[A-E]_[a-z]+_", "", all_results$scenario))

plot_sweep <- function(data, family_prefix, x_lab, title) {
  d <- data[grepl(paste0("^", family_prefix, "_"), data$scenario), ]
  summary_d <- aggregate(attack_rate ~ x_value, data = d, FUN = mean)
  ggplot(d, aes(x = x_value, y = attack_rate)) +
    geom_jitter(width = 0.02 * diff(range(d$x_value, na.rm = TRUE)), alpha = 0.4) +
    geom_line(data = summary_d, linewidth = 1, color = "steelblue") +
    geom_point(data = summary_d, size = 3, color = "steelblue") +
    labs(x = x_lab, y = "Attack rate (fraction ever infected)", title = title) +
    theme_minimal()
}

p_A <- plot_sweep(all_results, "A", "Baseline ACH (mean of truncated-normal, all settings)",
                   "A: higher baseline ACH should lower attack rate")
p_B <- plot_sweep(all_results, "B", "Workplace intervention coverage",
                   "B: higher coverage should lower attack rate")
p_C <- plot_sweep(all_results, "C", "Workplace intervention delta (eACH)",
                   "C: bigger delta should lower attack rate (diminishing returns)")

print(p_A)
print(p_B)
print(p_C)

# D: targeted_riskiness vs random coverage, same coverage level -- targeted
# should match or beat random (lower or equal attack rate).
p_D <- ggplot(
  subset(all_results, scenario %in% c("D_random", "D_targeted")),
  aes(x = scenario, y = attack_rate)
) +
  geom_boxplot(outlier.shape = NA, width = 0.5) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(x = NULL, y = "Attack rate", title = "D: targeted_riskiness should match or beat random") +
  theme_minimal()
print(p_D)

# E: joint intervention vs no intervention, broken out by setting-specific
# mean FOI. Workplace/school/leisure FOI should drop under the joint
# intervention; household FOI should be ~unchanged (household is excluded
# from joint deployment by design).
foi_long <- reshape(
  subset(all_results, scenario %in% c("E_none", "E_joint"),
         select = c(scenario, rep, mean_FOI_household, mean_FOI_workplace,
                    mean_FOI_school, mean_FOI_leisure)),
  varying = c("mean_FOI_household", "mean_FOI_workplace", "mean_FOI_school", "mean_FOI_leisure"),
  v.names = "mean_FOI",
  timevar = "setting",
  times = c("household", "workplace", "school", "leisure"),
  direction = "long"
)

p_E <- ggplot(foi_long, aes(x = setting, y = mean_FOI, fill = scenario)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(0.7), width = 0.6) +
  labs(
    x = NULL, y = "Mean FOI over simulation",
    title = "E: joint intervention should lower workplace/school/leisure FOI, leave household ~flat"
  ) +
  theme_minimal()
print(p_E)
