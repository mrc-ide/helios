# Script 1 of the beta/R0 calibration pipeline: runs Helios across a grid of
# beta_community values (no interventions, seasonality off) and measures the
# resulting final attack rate at each one. This produces a beta <-> R0 lookup
# table used downstream by Script 2 (map_beta_finalsize.R), which maps a
# target R0 or Rt(t) series to the corresponding beta_community value(s) via
# interpolation (R0 > 1) or linear extrapolation (R0 <= 1) against this
# table
# R0 is back-solved from the final attack rate at every swept beta value,
# including subcritical (R0 < 1) values, via the closed-form final-size
# relation (get_R0_from_attack_rate).

#' Run a beta_community sweep and back-solve R0 via the final-size method
#'
#' @param transmission_fraction Named numeric vector with names `household`,
#'   `workplace`, `leisure`, `community`, summing to 1. Used to derive the
#'   setting-specific beta ratios relative to beta_community. `beta_school`
#'   is always set equal to `beta_workplace`.
#' @param pathogen_params_list A parameters list from [get_parameters()]
#'   (e.g. `get_parameters(archetype = "flu")` or a fully custom
#'   `get_parameters(overrides = list(duration_exposed = ..., ...))`).
#'   Supplies natural history (`duration_exposed`, `duration_infectious`,
#'   hospitalization/death probabilities, etc.) and the defaults for
#'   `population`, `n_reps`, and `sim_time` below. Initial condition
#'   proportions are derived from `pathogen_params_list$number_initial_S/E/I/R`
#'   relative to `pathogen_params_list$human_population`, then rescaled to
#'   whichever `population` is actually used.
#' @param beta_community_range Numeric vector `c(min, max)` for the beta
#'   sweep. Default `c(0.01, 0.25)`.
#' @param n_beta Number of beta values to sweep, linearly spaced across
#'   `beta_community_range`. Default 25.
#' @param population Population size for all runs. If `NULL`, falls back to
#'   `pathogen_params_list$human_population`.
#' @param n_reps Number of stochastic replicates per beta value. If `NULL`,
#'   falls back to 20.
#' @param sim_time Simulation length in days. If `NULL`, falls back to
#'   `pathogen_params_list$simulation_time`.
#' @param n_cores Number of cores for parallel execution. If `NULL`, falls
#'   back to `min(detectCores() - 1, 10)`.
#' @param seed Random seed. Default 42.
#' @param out_file Optional file path to save the result via `saveRDS()`. If
#'   `NULL` (default), the result is only returned, not saved.
#'
#' @return A list with:
#'   - `sweep_table`: data.frame with columns `beta`, `AR_mean`, `AR_sd`,
#'     `R0_fs_mean`, `R0_fs_sd` (fs = final-size). One row per swept beta
#'     value, including subcritical (R0 < 1) values.
#'   - `args`: a named list recording every resolved argument used, for
#'     full reproducibility of the sweep.
#'   - `call_time`: POSIXct timestamp of when the sweep was run.
#'
#' @export
run_beta_sweep <- function(
  transmission_fraction,
  pathogen_params_list,
  beta_community_range = c(0.01, 0.25),
  n_beta = 25L,
  population = NULL,
  n_reps = NULL,
  sim_time = NULL,
  n_cores = NULL,
  seed = 42L,
  out_file = NULL
) {

  #Validate transmission fraction
  required_settings <- c("household", "workplace", "leisure", "community")
  if (!setequal(names(transmission_fraction), required_settings)) {
    stop(sprintf(
      "transmission_fraction must have exactly these names: %s",
      paste(required_settings, collapse = ", ")
    ))
  }
  if (!isTRUE(all.equal(sum(transmission_fraction), 1))) {
    stop("transmission_fraction must sum to 1")
  }

  #Defaults
  # If the user did not supply a value (it is NULL), fall back to a default
  if (is.null(population)) {
    population <- pathogen_params_list$human_population
  }
  if (is.null(n_reps)) {
    n_reps <- 20L
  }
  if (is.null(sim_time)) {
    sim_time <- pathogen_params_list$simulation_time
  }
  if (is.null(n_cores)) {
    n_cores <- min(max(1L, parallel::detectCores() - 1L), 10L)
  }

  #=== Derive setting-specific beta ratios ===#
  # beta_school is always forced equal to beta_workplace
  household_ratio <- transmission_fraction["household"] / transmission_fraction["community"]
  workplace_ratio <- transmission_fraction["workplace"] / transmission_fraction["community"]
  leisure_ratio   <- transmission_fraction["leisure"]   / transmission_fraction["community"]
  school_ratio    <- workplace_ratio

  #=== Rescale initial conditions to the resolved population ===#
  prop_S <- pathogen_params_list$number_initial_S / pathogen_params_list$human_population
  prop_E <- pathogen_params_list$number_initial_E / pathogen_params_list$human_population
  prop_I <- pathogen_params_list$number_initial_I / pathogen_params_list$human_population

  initial_S <- round(prop_S * population)
  initial_E <- round(prop_E * population)
  initial_I <- round(prop_I * population)
  initial_R <- population - initial_S - initial_E - initial_I

  #=== Build beta grid and run the sweep ===#
  beta_grid <- seq(beta_community_range[1], beta_community_range[2], length.out = n_beta)

  jobs <- expand.grid(beta_idx = seq_along(beta_grid), rep = seq_len(n_reps))

  message(sprintf("Using %d cores", n_cores))

  # Per-replicate seed depends only on rep, never on the swept beta value, so
  # replicate i uses the same base randomness at every beta (paired
  # comparison). Sweeping beta with seeds tied to beta_idx instead would make
  # each point along the sweep an independent random draw, which can produce
  # non-monotonic-looking sweep curves near the epidemic threshold that are
  # actually just sampling noise rather than a real model effect
  raw_attack_rates <- parallel::mclapply(
    seq_len(nrow(jobs)),
    function(i) run_one_sweep_replicate(
      beta                  = beta_grid[jobs$beta_idx[i]],
      seed                  = seed + jobs$rep[i],
      population            = population,
      initial_S             = initial_S,
      initial_E             = initial_E,
      initial_I             = initial_I,
      initial_R             = initial_R,
      sim_time              = sim_time,
      household_ratio       = household_ratio,
      workplace_ratio       = workplace_ratio,
      school_ratio          = school_ratio,
      leisure_ratio         = leisure_ratio,
      pathogen_params_list  = pathogen_params_list
    ),
    mc.cores = n_cores
  )

  #=== Back-solve R0 per replicate and aggregate by beta value ===#
  # R0 is back-solved per replicate (not from the mean AR), so R0_fs_mean/sd
  # reflect the actual spread of R0 across replicates
  sweep_rows <- vector(mode = "list", length = n_beta)

  for (i in seq_along(beta_grid)) {
    ar_vals <- unlist(raw_attack_rates[jobs$beta_idx == i])

    R0_vals <- vector(mode = "numeric", length = length(ar_vals))
    for (j in seq_along(ar_vals)) {
      R0_vals[j] <- get_R0_from_attack_rate(ar_vals[j])
    }

    sweep_rows[[i]] <- data.frame(
      beta       = beta_grid[i],
      AR_mean    = mean(ar_vals),
      AR_sd      = sd(ar_vals),
      R0_fs_mean = mean(R0_vals, na.rm = TRUE),
      R0_fs_sd   = sd(R0_vals, na.rm = TRUE)
    )
  }

  sweep_table <- do.call(rbind, sweep_rows)

  message(sprintf("Sweep complete (%d beta values x %d reps)", n_beta, n_reps))


  #=== Assemble and save result ===#
  result <- list(
    sweep_table = sweep_table,
    args = list(
      transmission_fraction = transmission_fraction,
      pathogen_params_list  = pathogen_params_list,
      beta_community_range  = beta_community_range,
      n_beta                = n_beta,
      population            = population,
      n_reps                = n_reps,
      sim_time              = sim_time,
      n_cores               = n_cores,
      seed                  = seed
    ),
    call_time = Sys.time()
  )

  if (!is.null(out_file)) {
    saveRDS(result, out_file)
    message(sprintf("Saved sweep result to %s", out_file))
  }

  result
}

#' Run a single simulation replicate at a fixed beta_community value
#'
#' Helper for [run_beta_sweep()]. Builds the parameter list for one
#' replicate and returns its final attack rate.
#'
#' @param beta beta_community value for this replicate
#' @param seed Seed for this replicate, passed through to
#'   `parameters_list$seed`
#' @param population Population size
#' @param initial_S,initial_E,initial_I,initial_R Initial compartment counts
#' @param sim_time Simulation length in days
#' @param household_ratio,workplace_ratio,school_ratio,leisure_ratio
#'   Setting-specific beta ratios relative to beta_community
#' @param pathogen_params_list Base parameters list supplying natural
#'   history and other non-swept parameters
#'
#' @return Numeric scalar: final attack rate (fraction of population
#'   ultimately infected)
run_one_sweep_replicate <- function(
  beta,
  seed,
  population,
  initial_S,
  initial_E,
  initial_I,
  initial_R,
  sim_time,
  household_ratio,
  workplace_ratio,
  school_ratio,
  leisure_ratio,
  pathogen_params_list
) {

  params_list <- get_parameters(overrides = modifyList(pathogen_params_list, list(
    human_population = population,
    number_initial_S = initial_S,
    number_initial_E = initial_E,
    number_initial_I = initial_I,
    number_initial_R = initial_R,
    simulation_time  = sim_time,
    seasonality_on   = FALSE,
    seed             = seed,
    beta_community   = beta,
    beta_household   = household_ratio * beta,
    beta_workplace   = workplace_ratio * beta,
    beta_school      = school_ratio    * beta,
    beta_leisure     = leisure_ratio   * beta
  )))

  sim_result  <- run_simulation(parameters_list = params_list)$result
  attack_rate <- tail(sim_result$R_count, 1) / population

  attack_rate
}

#' Back-solve R0 from a final attack rate via the final-size relation
#'
#' Helper for [run_beta_sweep()]. Solves `AR = 1 - exp(-R0*AR)` explicitly
#' for R0 (R0 appears once after taking logs, so no numerical root-finder
#' is needed here).
#'
#' @param attack_rate Numeric scalar, must be in (0, 1). `attack_rate = 0`
#'   gives 0/0, `attack_rate >= 1` gives log of <= 0, both undefined.
#'
#' @return Numeric scalar R0, or `NA` if `attack_rate` is outside (0, 1)
get_R0_from_attack_rate <- function(attack_rate) {

  if (attack_rate <= 0 | attack_rate >= 1) {
    R0 <- NA_real_
  } else {
    R0 <- -log(1 - attack_rate) / attack_rate
  }

  R0
}

#' Plot sanity-check figures for a beta sweep result
#'
#' Produces a two-panel base R plot: beta vs. back-solved R0 (with a
#' reference line at R0 = 1), and beta vs. attack rate with error bars
#' across replicates. Useful for a visual check that a sweep behaved
#' sensibly (monotonic R0, reasonable attack-rate spread) before using it
#' in [get_beta_from_R0_finalsize()].
#'
#' @param sweep_result The list returned by [run_beta_sweep()]
#'
#' @return Invisibly returns `NULL`. Called for its plotting side effect.
generate_beta_sweep_plot <- function(sweep_result) {

  sweep_table <- sweep_result$sweep_table

  original_par <- par(mfrow = c(1, 2))

  plot(
    sweep_table$beta, sweep_table$R0_fs_mean,
    type = "b", pch = 19, col = "steelblue",
    xlab = "beta_community", ylab = "R0 (back-solved)",
    main = "beta -> R0 mapping"
  )
  abline(h = 1, lty = 2, col = "grey60")

  plot(
    sweep_table$beta, sweep_table$AR_mean,
    type = "b", pch = 19, col = "coral", ylim = c(0, 1),
    xlab = "beta_community", ylab = "Attack rate (mean over replicates)",
    main = "beta -> Attack rate"
  )
  arrows(
    sweep_table$beta, sweep_table$AR_mean - sweep_table$AR_sd,
    sweep_table$beta, sweep_table$AR_mean + sweep_table$AR_sd,
    length = 0.03, angle = 90, code = 3, col = "coral"
  )

  par(original_par)

  invisible(NULL)
}
