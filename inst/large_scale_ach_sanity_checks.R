# Large-scale (50k population, 8 replicates per scenario) sanity checks for
# the ACH/intervention rewrite on this branch. Not a testthat suite -- these
# are stochastic, population-level checks that the *direction* and rough
# *shape* of model outputs match intuition, not exact-value regression
# tests. Takes ~10-15 minutes wall time using a PSOCK cluster.
#
# Uses parallel::makeCluster()/parLapply() (PSOCK, separate processes)
# rather than mclapply() (fork-based): forking a multithreaded process is a
# known crash/error source on macOS, especially from inside RStudio's GUI
# process, and mclapply() silently swallows per-worker errors as
# try-error objects (which then blow up rbind() with an opaque "numbers of
# columns ... do not match" error). PSOCK workers are genuinely separate R
# sessions, so they're robust cross-platform/cross-IDE, and a worker error
# surfaces immediately as a real error instead of being swallowed.
#
# Checks covered:
#   A. Higher baseline ACH -> lower attack rate (monotonic, no intervention)
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
N_REPS     <- as.integer(Sys.getenv("ACH_SANITY_N_REPS", "8"))
POP        <- as.integer(Sys.getenv("ACH_SANITY_POP", "50000"))
SIM_TIME   <- as.integer(Sys.getenv("ACH_SANITY_SIM_TIME", "150"))
N_WORKERS  <- max(1, min(10, parallel::detectCores() - 1))

cl <- parallel::makeCluster(N_WORKERS)
parallel::clusterCall(cl, function(p) { devtools::load_all(p); NULL }, pkg_path)

with_default_ach <- function(parameters_list, ach = 4) {
  for (setting in c("household", "workplace", "school", "leisure")) {
    parameters_list <- set_default_ach(parameters_list, setting, ach)
  }
  parameters_list
}

# ---------------------------------------------------------------------------
# Scenario builders
# ---------------------------------------------------------------------------

base_overrides <- function(seed) {
  list(
    human_population  = POP,
    simulation_time   = SIM_TIME,
    number_initial_S  = POP - 5,
    number_initial_E  = 5,
    seed              = seed
  )
}

# A: no intervention, sweep uniform baseline ACH
build_ach_scenario <- function(ach, seed) {
  with_default_ach(get_parameters(overrides = base_overrides(seed)), ach = ach)
}

# B/C: workplace intervention, sweep coverage and/or delta
build_intervention_scenario <- function(
  coverage, delta, coverage_type = "random", seed, ach = 4
) {
  parameters_list <- with_default_ach(
    get_parameters(overrides = base_overrides(seed)), ach = ach
  )
  if (coverage == 0) {
    return(parameters_list) # no intervention installed at all
  }
  intervention <- make_intervention(
    name = "constant_delta",
    delta_function = function(d) d,
    delta_params = list(d = delta),
    coverage = coverage
  )
  set_intervention_ach(
    parameters_list = parameters_list,
    setting = "workplace",
    coverage_target = "individuals",
    coverage_type = coverage_type,
    timestep = 1,
    intervention = intervention
  )
}

# E: joint intervention (workplace+school+leisure), diagnostics on so we can
# inspect per-setting FOI
build_joint_scenario <- function(coverage, delta, seed, ach = 4) {
  parameters_list <- with_default_ach(
    get_parameters(overrides = c(base_overrides(seed), list(render_diagnostics = TRUE))),
    ach = ach
  )
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
    setting = "joint",
    coverage_target = "individuals",
    coverage_type = "random",
    timestep = 1,
    intervention = intervention
  )
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

# Each PSOCK worker is a separate R process that only has the package
# loaded (via clusterCall() above) -- it does NOT have access to this
# script's global objects unless explicitly exported. build_fn closures
# (with_default_ach, base_overrides, POP, SIM_TIME in their lexical scope)
# resolve those free variables against the *worker's* global env when
# unserialized there, so all of this must be exported before run_scenario()
# is called below.
parallel::clusterExport(cl, varlist = c(
  "with_default_ach", "base_overrides",
  "build_ach_scenario", "build_intervention_scenario", "build_joint_scenario",
  "summarise_run", "POP", "SIM_TIME"
))

# ---------------------------------------------------------------------------
# A. Baseline ACH sweep (no intervention)
# ---------------------------------------------------------------------------

cat("Running scenario family A: baseline ACH sweep...\n")
ach_levels <- c(1, 4, 10, 20)
results_A <- do.call(rbind, lapply(ach_levels, function(ach) {
  run_scenario(paste0("A_ach_", ach), build_ach_scenario, ach = ach)
}))

# ---------------------------------------------------------------------------
# B. Coverage sweep (fixed delta = 5, random coverage, workplace)
# ---------------------------------------------------------------------------

cat("Running scenario family B: coverage sweep...\n")
coverage_levels <- c(0, 0.25, 0.5, 0.75, 1.0)
results_B <- do.call(rbind, lapply(coverage_levels, function(cov) {
  run_scenario(
    paste0("B_coverage_", cov), build_intervention_scenario,
    coverage = cov, delta = 5, coverage_type = "random"
  )
}))

# ---------------------------------------------------------------------------
# C. Delta sweep (fixed coverage = 1, random coverage, workplace)
# ---------------------------------------------------------------------------

cat("Running scenario family C: delta sweep...\n")
delta_levels <- c(0, 2, 10, 20) # delta = 5 @ coverage = 1 reused from family B
results_C <- do.call(rbind, lapply(delta_levels, function(d) {
  run_scenario(
    paste0("C_delta_", d), build_intervention_scenario,
    coverage = 1, delta = d, coverage_type = "random"
  )
}))
results_C_delta5 <- subset(results_B, scenario == "B_coverage_1")
results_C_delta5$scenario <- "C_delta_5"
results_C <- rbind(results_C, results_C_delta5)

# ---------------------------------------------------------------------------
# D. targeted_riskiness vs random coverage (same coverage = 0.5, delta = 5)
# ---------------------------------------------------------------------------

cat("Running scenario family D: targeted vs random coverage...\n")
results_D_targeted <- run_scenario(
  "D_targeted", build_intervention_scenario,
  coverage = 0.5, delta = 5, coverage_type = "targeted_riskiness"
)
results_D_random <- subset(results_B, scenario == "B_coverage_0.5")
results_D_random$scenario <- "D_random"
results_D <- rbind(results_D_targeted, results_D_random)

# ---------------------------------------------------------------------------
# E. Joint intervention vs no intervention (household exclusion check)
# ---------------------------------------------------------------------------

cat("Running scenario family E: joint intervention / household exclusion...\n")
results_E_none  <- run_scenario("E_none",  build_joint_scenario, coverage = 0,   delta = 5)
results_E_joint <- run_scenario("E_joint", build_joint_scenario, coverage = 0.5, delta = 5)
results_E <- rbind(results_E_none, results_E_joint)

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

p_A <- plot_sweep(all_results, "A", "Baseline ACH (uniform, all settings)",
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
