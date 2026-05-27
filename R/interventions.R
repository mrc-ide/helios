#' Update model parameters with intervention switches
#'
#' @description
#' Dispatcher that determines which locations have an intervention deployed.
#' If `intervention_joint_active` is TRUE, coverage is computed across the
#' workplace/school/leisure pool using `generate_joint_intervention_switches()`.
#' Otherwise, per-setting coverage is computed for each setting whose
#' `intervention_<setting>_active` flag is TRUE via
#' `generate_setting_intervention_switches()`.
#'
#' @param parameters_list A list of model parameters as generated using `get_parameters()`
#' @param variables_list A list of model variables as generated using `create_variables()`
#'
#' @family intervention
#' @export
generate_intervention_switches <- function(parameters_list, variables_list) {
  setting_types <- c("workplace", "school", "leisure", "household")
  if (
    isTRUE(parameters_list$intervention_joint_active) &
    any(unlist(parameters_list[paste0("intervention_", setting_types, "_active")]))
  ) {
    stop(
      "If intervention_joint_active is set to TRUE, setting-type specific intervention switches must be set to FALSE"
    )
  }

  if (isTRUE(parameters_list$intervention_joint_active)) {
    parameters_list <- generate_joint_intervention_switches(
      parameters_list,
      variables_list
    )
  } else {
    for (setting in setting_types) {
      if (isTRUE(parameters_list[[paste0("intervention_", setting, "_active")]])) {
        parameters_list <- generate_setting_intervention_switches(
          parameters_list,
          variables_list,
          setting = setting
        )
      }
    }
  }
  return(parameters_list)
}


#' Generate joint intervention switches
#'
#' Helper to generate joint intervention coverage across the workplace, school,
#' and leisure pool (household is excluded from joint deployment by convention).
#' Pools location sizes into a single budget, picks locations until cumulative
#' size meets the target coverage, and splits the result back into per-setting
#' coverage vectors stored at `intervention_<setting>_covered`.
#'
#' Also propagates the joint intervention list and timestep into the per-setting
#' state so that downstream efficacy calculation runs uniformly via the
#' per-setting code path.
#'
#'
#' @param parameters_list A list of model parameters as generated using `get_parameters()`
#' @param variables_list A list of model variables as generated using `create_variables()`
#'
#' @family intervention
#' @export
generate_joint_intervention_switches <- function(parameters_list, variables_list) {
  # Defining how coverage is defined (individuals vs square_footage)
  if (parameters_list[["intervention_joint_coverage_target"]] == "individuals") {
    setting_size_list <- list(
      "workplace" = get_setting_size(variables_list, "workplace"),
      "school"    = get_setting_size(variables_list, "school"),
      "leisure"   = parameters_list$setting_sizes$leisure
    )
  } else if (parameters_list[["intervention_joint_coverage_target"]] == "square_footage") {
    setting_size_list <- list(
      "workplace" = get_setting_size(variables_list, "workplace") *
        parameters_list$size_per_individual_workplace,
      "school"    = get_setting_size(variables_list, "school") *
        parameters_list$size_per_individual_school,
      "leisure"   = parameters_list$setting_sizes$leisure *
        parameters_list$size_per_individual_leisure
    )
  } else {
    stop(
      "intervention_joint_coverage_target must be either individuals or square_footage"
    )
  }

  # Pool sizes into a single budget
  setting_size_flat <- unlist(setting_size_list, use.names = FALSE)
  total_size        <- sum(setting_size_flat)
  total_length      <- length(setting_size_flat)
  intervention_switches <- rep(0, total_length)
  total_with_intervention <- total_size * parameters_list[["intervention_joint_coverage"]]

  # Pick locations until cumulative size meets the budget, either at random
  # or in decreasing order of riskiness
  if (parameters_list[["intervention_joint_coverage_type"]] == "random") {
    sum <- 0
    indices <- c()
    location_indices <- 1:total_length
    while (sum < total_with_intervention) {
      i <- sample(location_indices, 1)
      sum <- sum + setting_size_flat[i]
      indices <- c(indices, i)
      location_indices <- setdiff(location_indices, i)
      if (length(location_indices) == 0 & sum < total_with_intervention) {
        stop("Insufficient space to meet joint intervention coverage")
      }
    }
  } else if (parameters_list[["intervention_joint_coverage_type"]] == "targeted_riskiness") {
    riskiness_list <- list(
      "workplace" = parameters_list$workplace_specific_riskiness,
      "school"    = parameters_list$school_specific_riskiness,
      "leisure"   = parameters_list$leisure_specific_riskiness
    )
    riskiness_flat   <- unlist(riskiness_list, use.names = FALSE)
    riskiness_sorted <- sort(
      x = riskiness_flat,
      decreasing = TRUE,
      index.return = TRUE
    )
    final_index <- min(which(
      cumsum(setting_size_flat[riskiness_sorted$ix]) >= total_with_intervention
    ))
    indices <- riskiness_sorted$ix[1:final_index]
  } else {
    stop(
      "intervention_joint_coverage_type must be either random or targeted_riskiness"
    )
  }
  intervention_switches[indices] <- 1

  # Split the pooled switches back into per-setting vectors
  setting_name_index <- rep(
    names(setting_size_list),
    lengths(setting_size_list)
  )
  parameters_list[["intervention_workplace_covered"]] <- intervention_switches[
    setting_name_index == "workplace"
  ]
  parameters_list[["intervention_school_covered"]] <- intervention_switches[
    setting_name_index == "school"
  ]
  parameters_list[["intervention_leisure_covered"]] <- intervention_switches[
    setting_name_index == "leisure"
  ]

  # Propagate joint intervention list and timestep to each per-setting slot,
  # and switch each setting's _active flag on so the per-setting efficacy
  # calculation runs through the same code path.
  for (s in c("workplace", "school", "leisure")) {
    parameters_list[[paste0("intervention_", s, "_active")]]   <- TRUE
    parameters_list[[paste0("intervention_", s, "_list")]]     <- parameters_list$intervention_joint_list
    parameters_list[[paste0("intervention_", s, "_timestep")]] <- parameters_list$intervention_joint_timestep
  }

  return(parameters_list)
}

#' Generate intervention switches for a particular setting
#'
#' Helper to generate the intervention coverage vector for one setting, as used
#' in `generate_intervention_switches()`. Coverage is interpreted as the
#' fraction of total size to cover (size weighted either by number of
#' individuals or by square footage). Locations are picked until cumulative
#' size meets the budget, either at random or in decreasing order of riskiness.
#'
#' @param parameters_list A list of model parameters as generated using `get_parameters()`
#' @param variables_list A list of model variables as generated using `create_variables()`
#' @param setting One of `"workplace"`, `"school"`, `"leisure"`, or `"household"`
#'
#' @family intervention
#' @export
generate_setting_intervention_switches <- function(
    parameters_list,
    variables_list,
    setting
) {
  if (parameters_list[[paste0("intervention_", setting, "_coverage_target")]] == "individuals") {
    if (setting == "leisure") {
      setting_size <- parameters_list$setting_sizes$leisure
    } else {
      setting_size <- get_setting_size(variables_list, setting = setting)
    }
  } else if (
    parameters_list[[paste0("intervention_", setting, "_coverage_target")]] == "square_footage"
  ) {
    if (setting == "leisure") {
      setting_size <- parameters_list$setting_sizes$leisure *
        parameters_list[[paste0("size_per_individual_", setting)]]
    } else {
      setting_size <- get_setting_size(variables_list, setting = setting) *
        parameters_list[[paste0("size_per_individual_", setting)]]
    }
  } else {
    stop("coverage_target must be either individuals or square_footage")
  }

  total <- sum(setting_size)
  intervention_switches <- rep(0, length(setting_size))
  interventions <- parameters_list[[paste0("intervention_", setting, "_list")]]
  total_with_intervention <- floor(interventions[[1]]$coverage * total)


  if (parameters_list[[paste0("intervention_", setting, "_coverage_type")]] == "random") {
    sum <- 0
    indices <- c()
    location_indices <- 1:length(setting_size)

    while (sum < total_with_intervention) {
      i <- sample(location_indices, 1)
      sum <- sum + setting_size[i]
      indices <- c(indices, i)
      location_indices <- setdiff(location_indices, i)
      if (length(location_indices) == 0 & sum < total_with_intervention) {
        stop("Insufficient individuals to meet intervention coverage")
      }
    }
    intervention_switches[indices] <- 1
    parameters_list[[paste0("intervention_", setting, "_covered")]] <- intervention_switches
  } else if (
    parameters_list[[paste0("intervention_", setting, "_coverage_type")]] == "targeted_riskiness"
  ) {
    riskiness <- parameters_list[[paste0(setting, "_specific_riskiness")]]
    riskiness_sorted <- sort(
      x = riskiness,
      decreasing = TRUE,
      index.return = TRUE
    )
    final_index <- min(which(
      cumsum(setting_size[riskiness_sorted$ix]) >= total_with_intervention
    ))
    indices <- riskiness_sorted$ix[1:final_index]
    intervention_switches[indices] <- 1
    parameters_list[[paste0("intervention_", setting, "_covered")]] <- intervention_switches
  } else {
    stop("coverage_type must be either random or targeted_riskiness")
  }

  return(parameters_list)
}

# Wells-Riley ACH-based intervention pipeline

#' Construct an intervention object
#'
#' @description
#' Defines a single intervention that can be installed into one or more
#' settings via [set_intervention_ach()]. The intervention's effect on
#' per-location ventilation is encoded by `delta_function`, which returns
#' the extra equivalent ACH (eACH) the intervention adds to each covered
#' location. The function can either return a constant (same delta for every
#' covered location) or a value that depends on the location's baseline ACH.
#' Optional unit-to-unit variation can be added via `variation_function`.
#'
#' @param name A character string used as a human-readable label for the
#' intervention.
#' @param delta_depends_on_baseline_ach Logical. If `FALSE` (default),
#' `delta_function` is called with no arguments — the same delta is applied
#' to every covered location. If `TRUE`, `delta_function` is called with the
#' location's baseline ACH as its first argument — delta varies per location.
#' @param delta_function A function returning the eACH that the intervention
#' adds. Required (not NULL). When `delta_depends_on_baseline_ach = FALSE`,
#' should take no arguments other than those in `delta_params`. When
#' `delta_depends_on_baseline_ach = TRUE`, the first argument must be the
#' baseline ACH.
#' @param delta_params A named list of additional arguments passed to
#' `delta_function`. Default = `list()`.
#' @param variation Logical. If `TRUE`, location-to-location noise is added
#' to the delta using `variation_function`. Default = `FALSE`.
#' @param variation_function A noise-generating function (e.g. `rnorm`)
#' called as `variation_function(n_locations, <variation_params>)`.
#' @param variation_params A named list of additional arguments passed to
#' `variation_function`. Default = `list()`.
#' @param coverage Numeric in `[0, 1]`. Fraction of total setting size to
#' cover when the intervention is installed.
#'
#' @return A named list with the eight fields above, used as the intervention
#' object consumed by [set_intervention_ach()] and downstream pipeline.
#'
#' @family intervention
#' @export
make_intervention <- function(name,
                              delta_depends_on_baseline_ach = FALSE,
                              delta_function    = NULL,
                              delta_params      = list(),
                              variation                = FALSE,
                              variation_function       = NULL,
                              variation_params         = list(),
                              coverage                 = NULL) {
  list(
    name                     = name,
    delta_depends_on_baseline_ach = delta_depends_on_baseline_ach,
    delta_function    = delta_function,
    delta_params      = delta_params,
    variation                = variation,
    variation_function       = variation_function,
    variation_params         = variation_params,
    coverage                 = coverage
  )
}

#' Install an intervention into the parameters list
#'
#' @description
#' Stores a single intervention object (as produced by [make_intervention()])
#' into the parameters list under the chosen `setting`. The intervention is
#' not yet allocated to specific locations — that happens later via
#' [generate_intervention_switches()] when [create_variables()] runs.
#'
#' Setting "joint" pools workplace + school + leisure into a single coverage
#' budget; household is intentionally excluded from joint deployment. The
#' four per-setting options ("workplace", "school", "leisure", "household")
#' install the intervention independently into one setting.
#'
#' Currently single-intervention only — passing more than one intervention
#' will error. Multi-intervention support lives on a separate branch.
#'
#' @param parameters_list A list of model parameters as generated by
#' [get_parameters()].
#' @param setting Character. One of `"workplace"`, `"school"`, `"leisure"`,
#' `"household"`, or `"joint"`.
#' @param coverage_target Character. What the coverage fraction applies to —
#' either `"individuals"` (count of people) or `"square_footage"`
#' (size-weighted, using `size_per_individual_<setting>`).
#' @param coverage_type Character. How locations are selected for coverage —
#' either `"random"` (uniform sampling until the coverage budget is met) or
#' `"targeted_riskiness"` (locations ranked in decreasing order of riskiness).
#' @param timestep Numeric. First simulation timestep at which the
#' intervention's efficacy is applied in the FOI calculation.
#' @param ... One intervention object as returned by [make_intervention()].
#'
#' @return The input `parameters_list` with the `intervention_<setting>_*`
#' slots populated. For `setting = "joint"`, the `intervention_joint_coverage`
#' slot is additionally populated from the intervention object's `coverage`
#' field.
#'
#' @family intervention
#' @export
set_intervention_ach <- function(parameters_list,
                                 setting,
                                 coverage_target,
                                 coverage_type,
                                 timestep,
                                 ...) {
  interventions <- list(...)

  if (length(setting) > 1) {
    stop(
      "Error: Number of settings input greater than 1, parameterise for one setting at a time"
    )
  }
  if (!(setting %in% c("workplace", "school", "leisure", "household", "joint"))) {
    stop(
      "Error: Input setting invalid - intervention only deployable in workplace, school, leisure, household, or joint settings"
    )
  }
  if (length(interventions) == 0) {
    stop("set_intervention_ach requires at least one intervention")
  }
  if (length(interventions) > 1) {
    stop("multi-intervention support is not yet implemented; please pass a single intervention")
  }
  if (length(coverage_target) > 1) {
    stop(
      "Error: Number of coverage targets input greater than 1, parameterise for one coverage target at a time"
    )
  }
  if (!(coverage_target %in% c("individuals", "square_footage"))) {
    stop(
      "Error: coverage_target must be either 'individuals' or 'square_footage'"
    )
  }
  if (length(coverage_type) > 1) {
    stop(
      "Error: Number of coverage types input greater than 1, parameterise for one coverage type at a time"
    )
  }
  if (!(coverage_type %in% c("random", "targeted_riskiness"))) {
    stop(
      "Error: coverage_type must be either 'random' or 'targeted_riskiness'"
    )
  }

  # Joint mode is keyed under intervention_joint_*; per-setting modes under
  # intervention_<setting>_*. Same paste0 pattern works for both.
  parameters_list[[paste0("intervention_", setting, "_active")]]          <- TRUE
  parameters_list[[paste0("intervention_", setting, "_list")]]            <- interventions
  parameters_list[[paste0("intervention_", setting, "_coverage_target")]] <- coverage_target
  parameters_list[[paste0("intervention_", setting, "_coverage_type")]]   <- coverage_type
  parameters_list[[paste0("intervention_", setting, "_timestep")]]        <- timestep

  if (setting == "joint") {
    parameters_list[["intervention_joint_coverage"]] <- interventions[[1]]$coverage
  }

  return(parameters_list)
}

#' Compute per-location intervention efficacy from baseline ACH
#'
#' @description
#' Applies the Wells-Riley framework to compute per-location efficacy for an
#' installed intervention. For each covered location `i`:
#'
#' \deqn{efficacy_i = 1 - p_{post}(i) / p_{pre}(i)}
#'
#' where `p_pre` and `p_post` are the Wells-Riley infection probabilities
#' before and after the intervention's delta-eACH is added to that location's
#' baseline ACH. Uncovered locations get `delta = 0`, which gives
#' `efficacy = 0` and so leave FOI unchanged downstream.
#'
#' The intervention's delta per location is determined by the intervention
#' object's `delta_function` (and `delta_params`, `variation_function`,
#' `variation_params`); see [make_intervention()].
#'
#' Called once at simulation init from [create_variables()].
#'
#' @param ach_values Numeric vector of baseline ACH values per location in
#' the setting.
#' @param parameters_list A list of model parameters as generated by
#' [get_parameters()] (and populated by [set_intervention_ach()] and
#' [generate_intervention_switches()]).
#' @param setting Character. One of `"workplace"`, `"school"`, `"leisure"`,
#' or `"household"`.
#'
#' @return Numeric vector of per-location efficacies, length equal to
#' `length(ach_values)`. Each value is in `[0, 1]`; uncovered locations
#' return 0.
#'
#' @family intervention
#' @export
calculate_efficacy_from_ach <- function(ach_values, parameters_list, setting) {
  I    <- 1
  pi   <- parameters_list$wells_riley_emission_rate
  kD   <- parameters_list$wells_riley_decay_rate
  r    <- parameters_list$wells_riley_infection_prob_per_ffu
  RRtv <- parameters_list$wells_riley_respiratory_rate_factor
  t    <- parameters_list$wells_riley_time_in_room
  V    <- parameters_list[[paste0("volume_per_person_", setting)]]

  n           <- length(ach_values)
  total_delta <- rep(0, n)

  interventions <- parameters_list[[paste0("intervention_", setting, "_list")]]

  if (is.null(interventions) || length(interventions) == 0) {
    return(rep(0, n))
  }

  # Single-intervention only. Multi-intervention support lives on a separate
  # branch (clumped vs. independent coverage still to be settled).
  intervention <- interventions[[1]]

  # Coverage vector: 1 if location is covered, 0 if not. NULL = full coverage
  # (used by unit tests that bypass set_intervention_ach).
  coverage_vector <- parameters_list[[paste0("intervention_", setting, "_covered")]]

  # Call delta_function to get the delta for each location
  if (intervention$delta_depends_on_baseline_ach) {
    # pass baseline ACH as first argument, then params
    delta_i <- mapply(
      function(ach) do.call(intervention$delta_function,
                            c(list(ach), intervention$delta_params)),
      ach_values
    )
  } else {
    # function only uses its own params — same delta replicated across locations
    delta_i <- rep(
      do.call(intervention$delta_function, intervention$delta_params),
      n
    )
  }

  # Add location-to-location variation if requested
  if (intervention$variation && !is.null(intervention$variation_function)) {
    noise   <- do.call(intervention$variation_function,
                       c(list(n), intervention$variation_params))
    delta_i <- pmax(0, delta_i + noise)
  }

  # Zero out delta for uncovered locations
  if (!is.null(coverage_vector)) {
    delta_i <- delta_i * coverage_vector
  }

  total_delta <- delta_i

  alpha_pre  <- ach_values + kD
  alpha_post <- ach_values + kD + total_delta

  p_pre  <- 1 - exp(-r * (I * pi / (alpha_pre  * V)) * RRtv * t)
  p_post <- 1 - exp(-r * (I * pi / (alpha_post * V)) * RRtv * t)

  return(1 - p_post / p_pre)
}


# =============================================================================
# Helper functions for ACH / efficacy / UV-C conversions
# =============================================================================

#' Convert UV-C parameters to equivalent ACH (eACH)
#'
#' @description
#' Converts a UV-C intervention's photophysical parameters into an
#' equivalent air-change rate (eACH) that can be used as a delta in the
#' Wells-Riley framework. The conversion is
#' `delta = f * E_avg * k * 3.6`, where the factor 3.6 converts
#' (per-second) inactivation to (per-hour) eACH.
#'
#' @param f Fraction of the room volume that is irradiated (dimensionless,
#' in `[0, 1]`).
#' @param E_avg Average fluence rate within the irradiated volume
#' (mW/cm^2 or equivalent unit consistent with `k`).
#' @param k UV inactivation constant for the target pathogen
#' (units consistent with `E_avg`; product `E_avg * k` is per-second).
#'
#' @return Equivalent ACH contribution from UV-C (units: 1/hour).
#'
#' @family intervention
#' @export
uv_to_delta <- function(f, E_avg, k) {
  f * E_avg * k * 3.6
}

#' Wells-Riley efficacy for a single location given baseline ACH and delta
#'
#' @description
#' Standalone helper that computes the Wells-Riley efficacy for a single
#' (baseline_ach, delta) pair, useful for sanity checks and one-off
#' calibration outside of a full simulation run. Inside `helios`, the
#' equivalent per-location calculation is performed by
#' [calculate_efficacy_from_ach()] for all covered locations at once.
#'
#' Default values for the W-R parameters match those used by the live
#' pipeline (see Wells-Riley parameters in [get_parameters()]).
#' `V` is setting-specific (workplace = 27, school = 10, leisure = 8,
#' household = 50) and must be supplied explicitly.
#'
#' @param baseline_ach Baseline air-change rate of the location (1/hour).
#' @param delta Extra eACH added by the intervention (1/hour). Default = 0.
#' @param V Air volume per person in the setting (m^3 per person). Required.
#' @param kD Natural decay rate of the airborne pathogen (1/hour).
#' Default = 0.64.
#' @param r Probability of infection per inhaled FFU. Default = 1.37e-2.
#' @param pi Emission rate of infectious units (FFU/hour). Default = 27.
#' @param I Number of infectious individuals in the location. Default = 1
#' (and cancels in the efficacy ratio).
#' @param RRtv Respiratory rate * tidal volume (m^3/hour). Default = 0.45.
#' @param t Exposure window for the Wells-Riley calculation (hours).
#' Default = 4.
#'
#' @return Efficacy in `[0, 1]`: the relative reduction in single-exposure
#' infection probability the intervention produces in this hypothetical
#' location.
#'
#' @family intervention
#' @export
ach_to_efficacy <- function(baseline_ach,
                            delta = 0,
                            V,
                            kD   = 0.64,
                            r    = 1.37e-2,
                            pi   = 27,
                            I    = 1,
                            RRtv = 0.45,
                            t    = 4) {
  A <- r * I * pi * RRtv * t / V
  alpha_pre  <- baseline_ach + kD
  alpha_post <- baseline_ach + kD + delta
  p_pre  <- 1 - exp(-A / alpha_pre)
  p_post <- 1 - exp(-A / alpha_post)
  return(1 - p_post / p_pre)
}

#' Wells-Riley delta-eACH required to hit a target efficacy
#'
#' @description
#' Inverse of [ach_to_efficacy()]. Given a desired efficacy at a hypothetical
#' location with a specified baseline ACH and air volume per person, returns
#' the delta-eACH the intervention needs to add to produce that efficacy.
#' Useful for calibrating constant-delta interventions to match a target
#' "scalar efficacy" (e.g. to compare against main-branch runs).
#'
#' Default values for W-R parameters match those used by the live pipeline
#' (see Wells-Riley parameters in [get_parameters()]).
#'
#' Note: because efficacy is nonlinear in baseline ACH, applying this delta
#' to a population of locations with varying baseline ACH will produce a
#' *distribution* of realised efficacies centred near (but not exactly
#' equal to) `target_efficacy`.
#'
#' @param target_efficacy Desired efficacy in `[0, 1)`.
#' @param baseline_ach Baseline air-change rate of the hypothetical location
#' used for calibration (1/hour).
#' @param V Air volume per person in the setting (m^3 per person). Required.
#' @param kD Natural decay rate of the airborne pathogen (1/hour).
#' Default = 0.64.
#' @param r Probability of infection per inhaled FFU. Default = 1.37e-2.
#' @param pi Emission rate of infectious units (FFU/hour). Default = 27.
#' @param I Number of infectious individuals. Default = 1.
#' @param RRtv Respiratory rate * tidal volume (m^3/hour). Default = 0.45.
#' @param t Exposure window (hours). Default = 4.
#'
#' @return The required delta-eACH (1/hour), suitable for use as the return
#' value of a constant-delta `delta_function` in [make_intervention()].
#'
#' @family intervention
#' @export
efficacy_to_delta <- function(target_efficacy,
                              baseline_ach,
                              V,
                              kD   = 0.64,
                              r    = 1.37e-2,
                              pi   = 27,
                              I    = 1,
                              RRtv = 0.45,
                              t    = 4) {
  A          <- r * I * pi * RRtv * t / V
  alpha_pre  <- baseline_ach + kD
  p_pre      <- 1 - exp(-A / alpha_pre)
  alpha_post <- -A / log(1 - p_pre * (1 - target_efficacy))
  return(alpha_post - alpha_pre)
}
