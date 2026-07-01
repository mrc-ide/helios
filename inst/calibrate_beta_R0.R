# calibrate_beta_R0.R
#
# PURPOSE
# -------
# Builds an empirical beta <-> R0 mapping for Helios by:
#   1. Running Helios at a grid of flat beta values (seasonality OFF)
#   2. Measuring the attack rate (AR) at the end of each run
#   3. Back-solving R0 from AR via the final-size equation: AR = 1 - exp(-R0 * AR)
#   4. Fitting a piecewise-linear interpolator in both directions (beta->R0, R0->beta)
#
# This mapping is then used to:
#   a) Choose a baseline beta corresponding to a target mean R0
#   b) Convert an Rt(t) time series into a seasonal multiplier vector for Helios:
#        multiplier[t] = Rt[t] / mean(Rt)
#
# OUTPUT
# ------
# beta_R0_lookup.rds  -- data.frame with columns: beta, AR, R0
# beta_from_R0.rds    -- approxfun object: R0 -> beta
# R0_from_beta.rds    -- approxfun object: beta -> R0

library(helios)
library(parallel)

# ==============================================================================
# 0. Configuration
# ==============================================================================

# Beta grid: range should bracket the R0 values you expect.
# Widen or densify if your target R0 falls outside [min_beta, max_beta].
beta_grid <- seq(0.01, 0.25, length.out = 25)

# Betas for other settings are scaled relative to beta_community.
# Adjust these ratios to match your target pathogen / archetype.
household_ratio  <- 3   # beta_household = household_ratio  * beta_community
workplace_ratio  <- 1   # beta_workplace = workplace_ratio  * beta_community
school_ratio     <- 1   # beta_school    = school_ratio     * beta_community
leisure_ratio    <- 1   # beta_leisure   = leisure_ratio    * beta_community

# Number of stochastic replicates per beta value.
# More replicates = smoother AR estimates, especially near the epidemic threshold.
n_reps <- 20

# Simulation settings: long enough for the epidemic to burn out fully.
sim_time   <- 365
population <- 10000

set.seed(42)

n_cores <- max(1L, detectCores() - 1L)
message(sprintf("Using %d cores", n_cores))

# ==============================================================================
# 1. Grid search (parallelised over beta x replicate combinations)
# ==============================================================================

# Expand the full grid of (beta, rep) pairs so each worker gets one unit of work
jobs <- expand.grid(beta_idx = seq_along(beta_grid), rep = seq_len(n_reps))

run_one <- function(job_row) {
  b <- beta_grid[job_row$beta_idx]
  params <- get_parameters(overrides = list(
    human_population = population,
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

# Aggregate: one row per beta value
results <- do.call(rbind, lapply(seq_along(beta_grid), function(i) {
  ar_vals <- unlist(raw[jobs$beta_idx == i])
  data.frame(
    beta    = beta_grid[i],
    AR_mean = mean(ar_vals),
    AR_sd   = sd(ar_vals)
  )
}))

message(sprintf("Grid search complete (%d beta values x %d reps)", length(beta_grid), n_reps))

# ==============================================================================
# 2. Back-solve R0 from AR via the final-size equation
# ==============================================================================
# AR = 1 - exp(-R0 * AR)  =>  solve for R0 numerically given AR

backsolve_R0 <- function(AR) {
  # AR = 0 means no epidemic (R0 <= 1 or stochastic fade-out).
  # Return NA rather than a spurious root.
  if (AR < 0.01) return(NA_real_)
  uniroot(
    f        = function(R0) 1 - exp(-R0 * AR) - AR,
    interval = c(1e-6, 100),
    tol      = 1e-8
  )$root
}

results$R0 <- vapply(results$AR_mean, backsolve_R0, numeric(1))

# Drop rows where epidemic didn't take off (R0 is NA or < 1)
lookup <- results[!is.na(results$R0) & results$R0 >= 1, ]

# ==============================================================================
# 3. Build interpolation functions
# ==============================================================================

R0_from_beta <- approxfun(
  x      = lookup$beta,
  y      = lookup$R0,
  rule   = 2,  # extrapolate with boundary values rather than returning NA
  method = "linear"
)

beta_from_R0 <- approxfun(
  x      = lookup$R0,
  y      = lookup$beta,
  rule   = 2,
  method = "linear"
)

# ==============================================================================
# 4. Quick sanity plot
# ==============================================================================

op <- par(mfrow = c(1, 2))

plot(
  lookup$beta, lookup$R0,
  type = "b", pch = 19, col = "steelblue",
  xlab = "beta_community", ylab = "R0 (back-solved)",
  main = "beta -> R0 mapping"
)
abline(h = 1, lty = 2, col = "grey60")

plot(
  lookup$beta, lookup$AR_mean,
  type = "b", pch = 19, col = "coral",
  ylim = c(0, 1),
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

out_dir <- "inst"  # adjust if running from a different working directory

saveRDS(lookup,        file.path(out_dir, "beta_R0_lookup.rds"))
saveRDS(R0_from_beta,  file.path(out_dir, "R0_from_beta.rds"))
saveRDS(beta_from_R0,  file.path(out_dir, "beta_from_R0.rds"))

message("\nSaved: beta_R0_lookup.rds, R0_from_beta.rds, beta_from_R0.rds")

# ==============================================================================
# 6. Example: how a paper analysis script would use this
# ==============================================================================
#
# # In your separate paper script (outside helios repo), load the lookup and
# # your Rt time series, then construct the multiplier vector:
#
# beta_from_R0 <- readRDS("inst/beta_from_R0.rds")
# Rt            <- readRDS("path/to/IAV_Rt_processed.rds")  # daily Rt vector
#
# mean_Rt        <- mean(Rt)
# baseline_beta  <- beta_from_R0(mean_Rt)        # beta that gives mean(Rt)
# multiplier     <- Rt / mean_Rt                  # centred on 1.0
#
# params <- get_parameters(overrides = list(
#   simulation_time       = length(Rt),
#   seasonality_on        = TRUE,
#   seasonality_multiplier = multiplier,
#   beta_community        = baseline_beta,
#   beta_household        = 3 * baseline_beta,
#   # ... etc.
# ))
# sim <- run_simulation(parameters_list = params)
