# time_varying_transmission_calibration.R
#
# A three-step pipeline for calibrating beta values against a target R0 or
# Rt(t) time series, and constructing the setting-specific beta vectors needed
# for time-varying transmission runs in Helios.
#
# Workflow (top to bottom):
#
#   Step 1 — run_beta_sweep()
#     Runs Helios across a grid of beta_community values (no interventions,
#     time-varying transmission off) and records the final attack rate for
#     each value. For beta values that produce a self-sustaining epidemic
#     (R0 > 1), R0 is back-solved from the attack rate using the closed-form
#     final-size relation: R0 = -log(1 - AR) / AR. For beta values that do
#     not sustain an epidemic (R0 < 1), R0 is left as NA for those rows.
#     Output is a sweep_table (one row per beta value, with mean/sd of attack
#     rate and, where R0 >= 1, back-solved R0) plus a full record of every
#     argument used, for reproducibility.
#
#   Step 2 — get_beta_from_R0_finalsize()
#     Maps a target R0 scalar or Rt(t) vector to the corresponding
#     beta_community value(s). For targets > 1, interpolates directly against
#     the sweep table. For targets <= 1, fits a linear model to the swept
#     points closest to R0 = 1 and extrapolates. Each Rt(t) value is looked up
#     independently against this static beta-R0 relationship -- this does not
#     account for time-varying susceptible depletion interacting with a changing
#     Rt, and assumes a fully susceptible population.
#
#   Step 3 — generate_time_varying_beta_vectors()
#     Expands the beta_community vector from Step 2 into the five
#     setting-specific beta vectors Helios needs when
#     time_varying_transmission_on = TRUE. Ready to pass directly into
#     get_parameters(overrides = ...).

#' Run a beta_community sweep and back-solve R0 via the final-size method
#'
#' @param transmission_fraction Named numeric vector with names `household`,
#'   `workplace`, `leisure`, `community`, summing to 1. Used to derive the
#'   setting-specific beta ratios relative to beta_community. `beta_school`
#'   is always set equal to `beta_workplace`.
#' @param model_params A parameters list from [get_parameters()]
#'   (e.g. `get_parameters(archetype = "flu")` or a fully custom
#'   `get_parameters(overrides = list(duration_exposed = ..., ...))`).
#'   Supplies natural history (`duration_exposed`, `duration_infectious`,
#'   hospitalization/death probabilities, etc.) and the defaults for
#'   `population`, `n_reps`, and `sim_time` below. Initial condition
#'   proportions are derived from `model_params$number_initial_S/E/I/R`
#'   relative to `model_params$human_population`, then rescaled to
#'   whichever `population` is actually used.
#' @param beta_community_range Numeric vector `c(min, max)` for the beta
#'   sweep. Default `c(0.01, 0.25)`.
#' @param n_beta Number of beta values to sweep, linearly spaced across
#'   `beta_community_range`. Default 25.
#' @param population Population size for all runs. If `NULL`, falls back to
#'   `model_params$human_population`.
#' @param n_reps Number of stochastic replicates per beta value. If `NULL`,
#'   falls back to 20.
#' @param sim_time Simulation length in days. If `NULL`, falls back to
#'   `model_params$simulation_time`.
#' @param n_cores Number of cores for parallel execution. If `NULL`, falls
#'   back to `min(detectCores() - 1, 10)`.
#' @param seed Random seed. Default 42.
#' @param out_file Optional file path to save the result via `saveRDS()`. If
#'   `NULL` (default), the result is only returned, not saved.
#'
#' @return A list with:
#'   - `sweep_table`: data.frame with columns `beta_community`, `AR_mean`,
#'     `AR_sd`, `R0_fs_mean`, `R0_fs_sd` (fs = final-size). One row per swept
#'     beta value; `R0_fs_mean` and `R0_fs_sd` are `NA` for subcritical beta
#'     values (those that do not produce a self-sustaining epidemic).
#'   - `args`: a named list recording every resolved argument used, for
#'     full reproducibility of the sweep.
#'   - `call_time`: POSIXct timestamp of when the sweep was run.
#'
#' @export
run_beta_sweep <- function(
  transmission_fraction,
  model_params,
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
    population <- model_params$human_population
  }
  if (is.null(n_reps)) {
    n_reps <- 20L
  }
  if (is.null(sim_time)) {
    sim_time <- model_params$simulation_time
  }
  if (is.null(n_cores)) {
    n_cores <- min(max(1L, parallel::detectCores() - 1L), 10L)
  }

  #=== Warn if seeding infections are very low ===#
  initial_E_raw <- model_params$number_initial_E
  initial_I_raw <- model_params$number_initial_I
  if ((initial_E_raw + initial_I_raw) < 50) {
    warning(sprintf(
      paste0(
        "Total seeding infections (number_initial_E + number_initial_I = %d) ",
        "is less than 50. With low seeding, stochastic extinction is likely for ",
        "R0 near 1, which can produce misleading attack rates. Consider increasing ",
        "number_initial_E or number_initial_I in your model_params."
      ),
      initial_E_raw + initial_I_raw
    ))
  }

  #=== Derive setting-specific beta ratios ===#
  # beta_school is always forced equal to beta_workplace
  household_ratio <- transmission_fraction["household"] / transmission_fraction["community"]
  workplace_ratio <- transmission_fraction["workplace"] / transmission_fraction["community"]
  leisure_ratio   <- transmission_fraction["leisure"]   / transmission_fraction["community"]
  school_ratio    <- workplace_ratio

  #=== Rescale initial conditions to the resolved population ===#
  prop_S <- model_params$number_initial_S / model_params$human_population
  prop_E <- model_params$number_initial_E / model_params$human_population
  prop_I <- model_params$number_initial_I / model_params$human_population

  initial_S <- round(prop_S * population)
  initial_E <- round(prop_E * population)
  initial_I <- round(prop_I * population)
  initial_R <- population - initial_S - initial_E - initial_I

  #=== Build beta grid and run the sweep ===#
  beta_grid <- seq(beta_community_range[1], beta_community_range[2], length.out = n_beta)

  jobs <- expand.grid(beta_idx = seq_along(beta_grid), rep = seq_len(n_reps))

  message(sprintf("Using %d cores", n_cores))

  # Use a PSOCK cluster (parLapply) rather than mclapply: mclapply does not
  # work on Windows.
  cl <- parallel::makeCluster(n_cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterExport(
    cl,
    varlist = c(
      "beta_grid", "jobs", "seed", "population",
      "initial_S", "initial_E", "initial_I", "initial_R",
      "sim_time", "household_ratio", "workplace_ratio",
      "school_ratio", "leisure_ratio", "model_params"
    ),
    envir = environment()
  )
  parallel::clusterEvalQ(cl, library(helios))

  # Per-replicate seed depends only on rep, never on the swept beta value, so
  # replicate i uses the same base randomness at every beta (paired
  # comparison). Sweeping beta with seeds tied to beta_idx instead would make
  # each point along the sweep an independent random draw, which can produce
  # non-monotonic-looking sweep curves near the epidemic threshold that are
  # actually just sampling noise rather than a real model effect
  raw_attack_rates <- parallel::parLapply(
    cl,
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
      model_params          = model_params
    )
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

    R0_vals_super <- R0_vals[!is.na(R0_vals) & R0_vals >= 1]

    sweep_rows[[i]] <- data.frame(
      beta_community = beta_grid[i],
      AR_mean        = mean(ar_vals),
      AR_sd          = sd(ar_vals),
      R0_fs_mean     = if (length(R0_vals_super) > 0) mean(R0_vals_super) else NA_real_,
      R0_fs_sd       = if (length(R0_vals_super) > 0) sd(R0_vals_super)   else NA_real_
    )
  }

  sweep_table <- do.call(rbind, sweep_rows)

  message(sprintf("Sweep complete (%d beta values x %d reps)", n_beta, n_reps))

  #=== Assemble and save result ===#
  result <- list(
    sweep_table = sweep_table,
    args = list(
      transmission_fraction = transmission_fraction,
      model_params          = model_params,
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
#' @param model_params Base parameters list supplying natural
#'   history and other non-swept parameters
#'
#' @return Numeric scalar: final attack rate (fraction of population
#'   ultimately infected). Includes R, D, E, I_mild, and I_hosp compartments
#'   so the estimate is robust when sim_time is short and the epidemic has not
#'   fully resolved.
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
  model_params
) {

  params_list <- get_parameters(overrides = modifyList(model_params, list(
    human_population             = population,
    number_initial_S             = initial_S,
    number_initial_E             = initial_E,
    number_initial_I             = initial_I,
    number_initial_R             = initial_R,
    simulation_time              = sim_time,
    time_varying_transmission_on = FALSE,
    seed                         = seed,
    beta_community               = beta,
    beta_household               = household_ratio * beta,
    beta_workplace               = workplace_ratio * beta,
    beta_school                  = school_ratio    * beta,
    beta_leisure                 = leisure_ratio   * beta
  )))

  sim_result  <- run_simulation(parameters_list = params_list)$result
  attack_rate <- (tail(sim_result$R_count, 1) +
                  tail(sim_result$D_count, 1) +
                  tail(sim_result$E_count, 1) +
                  tail(sim_result$I_mild_count, 1) +
                  tail(sim_result$I_hosp_count, 1)) / population
  attack_rate
}

#' Back-solve R0 from a final attack rate via the final-size relation
#'
#' Helper for [run_beta_sweep()]. Solves `AR = 1 - exp(-R0 * AR)` explicitly
#' for R0: rearranging gives `R0 = -log(1 - AR) / AR` (R0 appears once after
#' taking logs, so no numerical root-finder is needed).
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
#'
#' @export
generate_beta_sweep_plot <- function(sweep_result) {

  sweep_table <- sweep_result$sweep_table

  original_par <- par(mfrow = c(1, 2))

  plot(
    sweep_table$beta_community, sweep_table$R0_fs_mean,
    type = "b", pch = 19, col = "steelblue",
    xlab = "beta_community", ylab = "R0 (back-solved)",
    main = "beta_community -> R0 mapping"
  )
  abline(h = 1, lty = 2, col = "grey60")

  plot(
    sweep_table$beta_community, sweep_table$AR_mean,
    type = "b", pch = 19, col = "coral", ylim = c(0, 1),
    xlab = "beta_community", ylab = "Attack rate (mean over replicates)",
    main = "beta_community -> Attack rate"
  )
  arrows(
    sweep_table$beta_community, sweep_table$AR_mean - sweep_table$AR_sd,
    sweep_table$beta_community, sweep_table$AR_mean + sweep_table$AR_sd,
    length = 0.03, angle = 90, code = 3, col = "coral"
  )

  par(original_par)

  invisible(NULL)
}

#' Map a target R0 (or Rt(t) series) to beta_community via final-size data
#'
#' Prints a summary of the transmission fractions used in the underlying
#' sweep, the results table, and a plot of the mapped beta_community
#' values across the target sequence, then returns that same table.
#'
#' Note: this method assumes a fully susceptible population. When applied to
#' an Rt(t) series, each value is looked up independently against the static
#' beta-R0 relationship and does not account for time-varying susceptible
#' depletion interacting with a changing Rt.
#'
#' @param target Numeric scalar (a single target R0) or numeric vector (an
#'   Rt(t) time series). Values > 1 are interpolated against the final-size
#'   table; values <= 1 use a linear extrapolation fit to swept points near
#'   the epidemic threshold (see `linear_threshold` and `min_linear_points`).
#' @param sweep_db Either the list object returned by `run_beta_sweep()`, or
#'   a file path (character string) to an `.rds` file saved by it.
#' @param linear_threshold R0 value defining the upper boundary of the
#'   near-threshold region used to fit the linear extrapolation. All swept
#'   points with R0 <= `linear_threshold` are used if there are at least
#'   `min_linear_points` of them; otherwise the `min_linear_points` swept
#'   points closest to R0 = 1 are used instead. Default 1.2.
#' @param min_linear_points Minimum number of points required in the
#'   near-threshold region before falling back to the closest-points approach.
#'   Default 3.
#'
#' @return A data.frame with one row per element of `target`, and columns:
#'   - `index`: position within `target` (useful for an Rt(t) series)
#'   - `target_R0`: the R0 or Rt value supplied
#'   - `beta_community`: the corresponding beta_community value
#'   - `source`: `"sweep_data"` if `target_R0 > 1` (interpolated directly
#'     from the sweep), or `"linear_extrapolation"` if `target_R0 <= 1`
#'
#' @export
get_beta_from_R0_finalsize <- function(
  target,
  sweep_db,
  linear_threshold  = 1.2,
  min_linear_points = 3L
) {

  #=== Validate inputs ===#
  if (!is.numeric(target)) {
    stop("target must be numeric (a single R0 value or an Rt(t) vector)")
  }
  if (min_linear_points < 2) {
    stop("min_linear_points must be at least 2 (a line needs two points)")
  }

  #=== Load the sweep result and pull out what is needed ===#
  sweep_result           <- get_sweep_result_from_source(sweep_db)
  sweep_table            <- sweep_result$sweep_table
  transmission_fraction  <- sweep_result$args$transmission_fraction

  valid_rows <- sweep_table[!is.na(sweep_table$R0_fs_mean), ]
  valid_rows <- valid_rows[order(valid_rows$beta_community), ]

  if (nrow(valid_rows) < min_linear_points) {
    stop("fewer valid sweep rows than min_linear_points -- widen the sweep or lower min_linear_points")
  }

  #Lookup
  # Interpolation for target > 1, spanning the whole final-size table
  interpolate_beta <- approxfun(
    x = valid_rows$R0_fs_mean,
    y = valid_rows$beta_community,
    rule = 2
  )

  # For target R0 <= 1, the sweep table has no reliable back-solved R0 values
  # below the threshold, so we cannot interpolate directly. Instead, we fit a
  # line through the swept points just above the epidemic threshold
  # (1 <= R0 <= linear_threshold), where the beta-R0 relationship is
  # approximately linear, and extrapolate below R0 = 1. If fewer than
  # min_linear_points swept points fall in that region, we fall back to
  # the min_linear_points points with R0 closest to 1.
  near_threshold <- valid_rows[valid_rows$R0_fs_mean <= linear_threshold, ]
  if (nrow(near_threshold) >= min_linear_points) {
    fit_rows <- near_threshold
  } else {
    distances <- abs(valid_rows$R0_fs_mean - 1)
    fit_rows  <- valid_rows[order(distances)[seq_len(min_linear_points)], ]
  }
  extrapolation_fit <- lm(beta_community ~ R0_fs_mean, data = fit_rows)

  #Look up beta for each target value
  beta_out   <- vector(mode = "numeric",   length = length(target))
  source_out <- vector(mode = "character", length = length(target))

  for (i in seq_along(target)) {
    if (target[i] > 1) {
      beta_out[i]   <- interpolate_beta(target[i])
      source_out[i] <- "sweep_data"
    } else {
      beta_out[i]   <- predict(extrapolation_fit, newdata = data.frame(R0_fs_mean = target[i]))
      source_out[i] <- "linear_extrapolation"
    }
  }

  result_table <- data.frame(
    index          = seq_along(target),
    target_R0      = target,
    beta_community = beta_out,
    source         = source_out
  )

  #=== Print a summary, the results table, and a plot ===#
  cat("Transmission fractions used in the underlying sweep:\n")
  for (setting_name in names(transmission_fraction)) {
    cat(sprintf("  %s: %.1f%%\n", setting_name, transmission_fraction[[setting_name]] * 100))
  }
  cat("\n")
  print(result_table)

  plot(
    result_table$index, result_table$beta_community,
    type = "l", col = "steelblue",
    xlab = "index", ylab = "beta_community",
    main = "Mapped beta_community across target sequence"
  )

  result_table
}

#' Load a sweep result from a run_beta_sweep() result or a saved file
#'
#' Helper for [get_beta_from_R0_finalsize()]. Accepts either the list
#' object returned directly by `run_beta_sweep()`, or a file path to an
#' `.rds` file saved by it, and returns that list either way.
#'
#' @param sweep_db The list returned by `run_beta_sweep()`, or a character
#'   file path to an `.rds` file containing that list
#'
#' @return The full list returned by `run_beta_sweep()` (containing
#'   `sweep_table`, `args`, and `call_time`)
get_sweep_result_from_source <- function(sweep_db) {

  if (is.character(sweep_db)) {
    sweep_result <- readRDS(sweep_db)
  } else {
    sweep_result <- sweep_db
  }

  sweep_result
}

#' Expand a transmission_fraction split and a beta_community vector into
#' the five setting-specific beta vectors Helios needs
#'
#' Given a time-varying beta_community vector and a fixed transmission fraction
#' split, this function derives the corresponding time-varying beta values for
#' all settings. Setting-specific betas are scaled relative to beta_community
#' using the transmission fraction ratios, producing five vectors ready to pass
#' into `get_parameters()`.
#'
#' @param transmission_fraction Named numeric vector with names `household`,
#'   `workplace`, `leisure`, `community`, summing to 1.
#' @param beta_community_vector Numeric vector, the time-varying community beta
#'   (one value per simulated day). Typically the `beta_community` column from
#'   the table returned by [get_beta_from_R0_finalsize()].
#'
#' @return A named list with five numeric vectors, each the same length as
#'   `beta_community_vector`: `beta_household`, `beta_workplace`, `beta_school`,
#'   `beta_leisure`, `beta_community`. `beta_school` is always identical to
#'   `beta_workplace`. Ready to pass directly into
#'   `get_parameters(overrides = ...)` alongside
#'   `time_varying_transmission_on = TRUE`.
#'
#' @export
generate_time_varying_beta_vectors <- function(transmission_fraction, beta_community_vector) {

  #=== Validate inputs ===#
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
  if (!is.numeric(beta_community_vector)) {
    stop("beta_community_vector must be numeric")
  }

  #=== Derive setting-specific ratios relative to community ===#
  household_ratio <- transmission_fraction["household"] / transmission_fraction["community"]
  workplace_ratio <- transmission_fraction["workplace"] / transmission_fraction["community"]
  leisure_ratio   <- transmission_fraction["leisure"]   / transmission_fraction["community"]
  school_ratio    <- workplace_ratio

  #=== Expand into five vectors, one per setting ===#
  result <- list(
    beta_household = unname(household_ratio * beta_community_vector),
    beta_workplace = unname(workplace_ratio * beta_community_vector),
    beta_school    = unname(school_ratio    * beta_community_vector),
    beta_leisure   = unname(leisure_ratio   * beta_community_vector),
    beta_community = beta_community_vector
  )

  result
}
