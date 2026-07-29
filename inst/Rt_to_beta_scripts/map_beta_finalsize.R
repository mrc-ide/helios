# map_beta_finalsize.R
#
# Script 2 of the beta/R0 calibration pipeline: maps a target R0 value, or
# an Rt(t) time series, to the corresponding beta_community value(s), using
# only the final-size (attack-rate-derived) table produced by Script 1
# (beta_R0_calibration.R).
#
# For target values strictly greater than 1, this interpolates between
# swept beta values using the final-size table directly.
#
# For target values at or below 1, this fits a linear model to the bottom
# few points of the final-size table (the points closest to R0 = 1, where
# the beta-to-R0 relationship is closest to linear) and extrapolates that
# line.
#
# When target is an Rt(t) vector, each value is looked up independently
# against this static beta-R0 relationship. This is an approximation: it
# does not account for time-varying susceptible depletion interacting with
# a changing Rt. Documented here as a known simplification, not assumed
# away silently.

#' Map a target R0 (or Rt(t) series) to beta_community via final-size data
#'
#' Prints a summary of the transmission fractions used in the underlying
#' sweep, followed by a results table, then returns that same table.
#'
#' @param target Numeric scalar (a single target R0) or numeric vector (an
#'   Rt(t) time series). Values > 1 are interpolated against the final-size
#'   table; values <= 1 use a linear extrapolation of that same table's
#'   points closest to R0 = 1 (see `n_linear_bottom`).
#' @param sweep_db Either the list object returned by `run_beta_sweep()`, or
#'   a file path (character string) to an `.rds` file saved by it.
#' @param n_linear_bottom Number of the lowest-beta rows of the final-size
#'   table to use for the linear extrapolation fit when `target <= 1`.
#'   Default 5.
#'
#' @return A data.frame with one row per element of `target`, and columns:
#'   - `index`: position within `target` (useful for an Rt(t) series)
#'   - `target_R0`: the R0 or Rt value supplied
#'   - `beta_community`: the corresponding beta_community value
#'   - `source`: `"sweep_data"` if `target_R0 > 1` (interpolated directly
#'     from Script 1's simulated data), or `"linear_extrapolation"` if
#'     `target_R0 <= 1`
#'
#' @export
get_beta_from_R0_finalsize <- function(
  target,
  sweep_db,
  n_linear_bottom = 5
) {

  #=== Validate inputs ===#
  if (!is.numeric(target)) {
    stop("target must be numeric (a single R0 value or an Rt(t) vector)")
  }
  if (n_linear_bottom < 2) {
    stop("n_linear_bottom must be at least 2 (a line needs two points)")
  }

  #=== Load the sweep result and pull out what is needed ===#
  sweep_result           <- get_sweep_result_from_source(sweep_db)
  sweep_table            <- sweep_result$sweep_table
  transmission_fraction  <- sweep_result$args$transmission_fraction

  valid_rows <- sweep_table[!is.na(sweep_table$R0_fs_mean), ]
  valid_rows <- valid_rows[order(valid_rows$beta), ]

  if (nrow(valid_rows) < n_linear_bottom) {
    stop("fewer valid final-size rows than n_linear_bottom -- widen the sweep or lower n_linear_bottom")
  }

  #Lookup
  # Interpolation for target > 1, spanning the whole final-size table
  interpolate_beta <- approxfun(
    x = valid_rows$R0_fs_mean,
    y = valid_rows$beta,
    rule = 2
  )

  # Linear extrapolation for target <= 1, fit only on the rows closest to
  # R0 = 1 (lowest beta), since the beta-to-R0 relationship is concave
  # overall and only close to linear near the threshold
  bottom_rows <- valid_rows[seq_len(n_linear_bottom), ]
  extrapolation_fit <- lm(beta ~ R0_fs_mean, data = bottom_rows)

  #Look up beta for each target value
  # Each element of target is looked up independently against this static
  # beta-R0 relationship. For an Rt(t) series, this treats every timepoint
  # as an isolated instantaneous lookup.
  beta_out   <- vector(mode = "numeric", length = length(target))
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

  #=== Print a summary and the results table ===#
  cat("Transmission fractions used in the underlying sweep:\n")
  for (setting_name in names(transmission_fraction)) {
    cat(sprintf("  %s: %.1f%%\n", setting_name, transmission_fraction[[setting_name]] * 100))
  }
  cat("\n")
  print(result_table)

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
