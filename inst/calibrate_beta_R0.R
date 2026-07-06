# calibrate_beta_R0.R
#
# PURPOSE
# -------
# Builds an empirical beta <-> R0 mapping for Helios by:
#   1. Running Helios at a grid of flat beta values (seasonality OFF)
#   2. Measuring the attack rate (AR) at the end of each run
#   3. Back-solving R0 from AR via the final-size equation: AR = 1 - exp(-R0 * AR)
#   4. Fitting a piecewise-linear interpolators in both directions (beta->R0, R0->beta)
#
# This mapping is used to:
#   a) Choose a baseline beta corresponding to a target mean R0
#   b) Convert an Rt(t) time series into a seasonal multiplier vector for Helios:
#        multiplier[t] = Rt[t] / mean(Rt)
#
# OUTPUT
# ------
# beta_R0_lookup.rds  -- data.frame with columns: beta, AR_mean, AR_sd, R0
# beta_from_R0.rds    -- approxfun object: R0 -> beta
# R0_from_beta.rds    -- approxfun object: beta -> R0

library(helios)
library(parallel)

# ==============================================================================
# 0. Configuration — edit these before running
# ==============================================================================

# Beta grid: range should bracket the R0 values you expect.
# Widen or densify if your target R0 falls outside this range.
beta_grid <- seq(0.01, 0.25, length.out = 25)

# Setting-specific beta ratios derived from the flu archetype.
flu             <- get_parameters(archetype = "flu")
household_ratio <- flu$beta_household / flu$beta_community
workplace_ratio <- flu$beta_workplace / flu$beta_community
school_ratio    <- flu$beta_school    / flu$beta_community
leisure_ratio   <- flu$beta_leisure   / flu$beta_community

# Replicates per beta value — more = smoother AR estimates.
n_reps <- 20

# Simulation length (days) — long enough for the epidemic to burn out fully.
sim_time <- 365

# Population size — change this freely; compartments scale automatically.
population <- 30000

# Initial conditions (flu proportions); R is the remainder to ensure sum = population.
initial_S <- round(0.67  * population)
initial_E <- round(0.006 * population)
initial_I <- round(0.012 * population)
initial_R <- population - initial_S - initial_E - initial_I

# Output directory for saved .rds files.
out_dir <- "inst"

set.seed(42)

# Cap cores at 10 to avoid exhausting R's 128-connection limit on high-core machines.
n_cores <- min(max(1L, detectCores() - 1L), 10L)
message(sprintf("Using %d cores", n_cores))

# ==============================================================================
# 1. Grid search (parallelised over beta x replicate combinations)
# ==============================================================================

jobs <- expand.grid(beta_idx = seq_along(beta_grid), rep = seq_len(n_reps))

run_one <- function(job_row) {
  b <- beta_grid[job_row$beta_idx]
  params <- get_parameters(overrides = list(
    human_population = population,
    number_initial_S = initial_S,
    number_initial_E = initial_E,
    number_initial_I = initial_I,
    number_initial_R = initial_R,
    simulation_time  = sim_time,
    seasonality_on   = FALSE,
    beta_community   = b,
    beta_household   = household_ratio * b,
    beta_workplace   = workplace_ratio * b,
    beta_school      = school_ratio    * b,
    beta_leisure     = leisure_ratio   * b
  ))
  sim <- run_simulation(parameters_list = params)$result
  tail(sim$R_count, 1) / population
}

raw <- mclapply(
  seq_len(nrow(jobs)),
  function(i) run_one(jobs[i, ]),
  mc.cores = n_cores
)

results <- do.call(rbind, lapply(seq_along(beta_grid), function(i) {
  ar_vals <- unlist(raw[jobs$beta_idx == i])
  data.frame(beta = beta_grid[i], AR_mean = mean(ar_vals), AR_sd = sd(ar_vals))
}))

message(sprintf("Grid search complete (%d beta values x %d reps)", length(beta_grid), n_reps))

# ==============================================================================
# 2. Back-solve R0 from AR via the final-size equation
# ==============================================================================

backsolve_R0 <- function(AR) {
  if (AR < 0.01) return(NA_real_)
  uniroot(
    f        = function(R0) 1 - exp(-R0 * AR) - AR,
    interval = c(1e-6, 100),
    tol      = 1e-8
  )$root
}

results$R0 <- vapply(results$AR_mean, backsolve_R0, numeric(1))
lookup <- results[!is.na(results$R0) & results$R0 >= 1, ]

# ==============================================================================
# 3. Build interpolation functions
# ==============================================================================

R0_from_beta <- approxfun(x = lookup$beta, y = lookup$R0,  rule = 2)
beta_from_R0 <- approxfun(x = lookup$R0,  y = lookup$beta, rule = 2)

# ==============================================================================
# 4. Sanity plots
# ==============================================================================

op <- par(mfrow = c(1, 2))

plot(
  lookup$beta, lookup$R0,
  type = "b", pch = 19, col = "steelblue",
  xlab = "beta_community", ylab = "R0 (back-solved)",
  main = "beta -> R0"
)
abline(h = 1, lty = 2, col = "grey60")

plot(
  lookup$beta, lookup$AR_mean,
  type = "b", pch = 19, col = "coral", ylim = c(0, 1),
  xlab = "beta_community", ylab = "Attack rate (mean over replicates)",
  main = "beta -> Attack rate"
)
arrows(
  lookup$beta, lookup$AR_mean - lookup$AR_sd,
  lookup$beta, lookup$AR_mean + lookup$AR_sd,
  length = 0.03, angle = 90, code = 3, col = "coral"
)

par(op)

# ==============================================================================
# 5. Save outputs
# ==============================================================================

saveRDS(lookup,       file.path(out_dir, "beta_R0_lookup.rds"))
saveRDS(R0_from_beta, file.path(out_dir, "R0_from_beta.rds"))
saveRDS(beta_from_R0, file.path(out_dir, "beta_from_R0.rds"))

message(sprintf("\nSaved to %s/: beta_R0_lookup.rds, R0_from_beta.rds, beta_from_R0.rds", out_dir))

# ==============================================================================
# 6. Example: using the lookup in a paper analysis script
# ==============================================================================
#
# beta_from_R0  <- readRDS("inst/beta_from_R0.rds")
# Rt            <- readRDS("path/to/IAV_Rt_processed.rds")  # daily Rt vector
#
# baseline_beta <- beta_from_R0(mean(Rt))
# multiplier    <- Rt / mean(Rt)                 # centred on 1.0
#
# params <- get_parameters(overrides = list(
#   simulation_time        = length(Rt),
#   seasonality_on         = TRUE,
#   seasonality_multiplier = multiplier,
#   beta_community         = baseline_beta,
#   beta_household         = household_ratio * baseline_beta,
#   beta_workplace         = workplace_ratio * baseline_beta,
#   beta_school            = school_ratio    * baseline_beta,
#   beta_leisure           = leisure_ratio   * baseline_beta
# ))
# sim <- run_simulation(parameters_list = params)
