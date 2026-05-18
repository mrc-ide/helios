# # Old far-UVC setter. Superseded by set_intervention_ach() + make_intervention()
# # in the ACH/efficacy pipeline. Kept commented for reference.
# #' Update model parameters with far UVC settings
# #'
# #' @description
# #' The `set_uvc()` function is a user-facing function that is used to parameterise far UVC deployment
# #' for an individual setting type (e.g. workplace). The function takes as arguments a `helios`
# #' parameter list, the `setting` type for which far UVC deployment is being parameterised, the `coverage`
# #' of far UVC within each location of a setting type (e.g. coverage of individual workplaces within
# #' the workplace setting class), the `coverage_type` (currently supporting random and targeted at the
# #' most populated settings), the `efficacy` of far UVC in the setting type, and the `timestep` on
# #' which far UVC is deployed. The function appends these additional setting-specific parameters to
# #' the parameter list and returns an updated version of it.
# #'
# #' @param parameters_list A list of parameters as generated using `get_parameters()`
# #' @param setting A character string describing the setting type in which far UVC is being deployed
# #' @param coverage A numeric value describing the coverage of far UVC within the setting class for which which far UVC is deployed
# #' @param coverage_target A character describing the target of the coverage ("buildings" or "individuals")
# #' @param coverage_type A character describing the type of coverage ("random" or "targeted")
# #' @param efficacy A numeric value describing the efficacy of the far UVC deployed
# #' @param timestep A numeric value describing the timestep in which far UVC is deployed
# #'
# #' @family intervention
# #' @export
# set_uvc <- function(
    #   parameters_list,
#   setting,
#   coverage,
#   coverage_target,
#   coverage_type,
#   efficacy,
#   timestep
# ) {
#   if (length(setting) > 1) {
#     stop(
#       "Error: Number of settings input greater than 1, parameterise for one setting at a time"
#     )
#   }
#
#   if (!(setting %in% c("workplace", "school", "leisure", "household", "joint"))) {
#     stop(
#       "Error: Input setting invalid -far UVC only deployable in workplace, school, leisure, household, or joint settings"
#     )
#   }
#
#   if (coverage < 0 | coverage > 1) {
#     stop("Error: coverage must take a value between 0 and 1")
#   }
#
#   if (length(coverage_target) > 1) {
#     stop(
#       "Error: Number of coverage targets input greater than 1, parameterise for one coverage target at a time"
#     )
#   }
#
#   if (coverage_target != "individuals" & coverage_target != "square_footage") {
#     stop(
#       "Error: Input setting invalid - far UVC coverage only applicable to individuals or square_footage"
#     )
#   }
#
#   if (length(coverage_type) > 1) {
#     stop(
#       "Error: Number of coverage types input greater than 1, parameterise for one coverage type at a time"
#     )
#   }
#
#   if (coverage_type != "random" & coverage_type != "targeted_riskiness") {
#     stop(
#       "Error: Input setting invalid - far UVC only deployable in random or targeted_riskiness coverage types"
#     )
#   }
#
#   if (efficacy < 0 | efficacy > 1) {
#     stop("Error: efficacy must take a value between 0 and 1")
#   }
#
#   parameters_list[[paste0("far_uvc_", setting)]] <- TRUE
#   parameters_list[[paste0("far_uvc_", setting, "_coverage")]] <- coverage
#   parameters_list[[paste0(
#     "far_uvc_",
#     setting,
#     "_coverage_target"
#   )]] <- coverage_target
#   parameters_list[[paste0(
#     "far_uvc_",
#     setting,
#     "_coverage_type"
#   )]] <- coverage_type
#   parameters_list[[paste0("far_uvc_", setting, "_efficacy")]] <- efficacy
#   parameters_list[[paste0("far_uvc_", setting, "_timestep")]] <- timestep
#
#   return(parameters_list)
# }

# #' Update model parameters with far UVC switches
# #'
# #' @description
# #' `generate_far_uvc_switches()` determines which locations will deploy far UVC given the setting type is
# #' switched on, the setting-specific coverages, and the setting-specific coverage types. The function returns,
# #' for each the workplace, school, leisure, and household settings, a vector of length equal to the
# #' the number of locations within the setting type (e.g. number of schools within the school setting type)
# #' populated with 1's and 0's, where a 1 represents the presence of far UVC and a 0 the absence of far
# #' UVC. The function returns an updated parameter list with these vectors appended for each setting type
# #' for which far UVC has been parameterised using the `set_uvc()` function.
# #'
# #' @param parameters_list A list of model parameters as generated using `get_parameters()`
# #' @param variables_list A list of model variables as generated using `create_variables()`
# #'
# #' @family intervention
# #' @export
# generate_far_uvc_switches <- function(parameters_list, variables_list) {
#   # Checking that if far_uvc_joint = TRUE, no Setting-Type specific farUVC switches have been turned on
#   setting_types <- c("workplace", "school", "leisure", "household")
#   if (
#     parameters_list$far_uvc_joint &
#       any(unlist(parameters_list[paste0("far_uvc_", setting_types)]))
#   ) {
#     stop(
#       "If far_uvc_joint is set to TRUE, setting-type specific far_UVC switches must be set to FALSE"
#     )
#   }
#
#   # If far_uvc_joint = TRUE calculate far UVC coverage for all locations across all setting-types altogether
#   if (parameters_list$far_uvc_joint) {
#     parameters_list <- generate_joint_far_uvc_switches(
#       parameters_list,
#       variables_list
#     )
#   } else {
#     # Else, check if there is UVC for any of these setting-types and turn it on if so
#     for (setting in setting_types) {
#       # If the setting-type has farUVC, generate the switches using the helper function generate_setting_far_uvc_switches
#       if (parameters_list[[paste0("far_uvc_", setting)]]) {
#         parameters_list <- generate_setting_far_uvc_switches(
#           parameters_list,
#           variables_list,
#           setting = setting
#         )
#       }
#     }
#   }
#   return(parameters_list)
# }


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
#' Adapted from `generate_far_uvc_switches()` (kept as comments above).
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


# #' Generate joint far UVC switches
# #'
# #' This is a helper function to generate the joint far UVC switches across all
# #' locations, as used in `generate_far_uvc_switches()`.
# #'
# #' @param parameters_list A list of model parameters as generated using `get_parameters()`
# #' @param variables_list A list of model variables as generated using `create_variables()`
# #'
# #' @family intervention
# #' @export
# generate_joint_far_uvc_switches <- function(parameters_list, variables_list) {
#   # Defining how coverage is defined (i.e. based on the number of individuals a location holds, or its square footage)
#   if (parameters_list[["far_uvc_joint_coverage_target"]] == "individuals") {
#     ## based on number of individuals
#     # A list with vectors containing the setting sizes
#     setting_size_list <- list(
#       "workplace" = get_setting_size(variables_list, "workplace"),
#       "school" = get_setting_size(variables_list, "school"),
#       # "household" = get_setting_size(variables_list, "household"),
#       "leisure" = parameters_list$setting_sizes$leisure
#     )
#     # Defining coverage according to the size of the setting (i.e. number of individuals multiplied by square footage per person)
#   } else if (parameters_list[["far_uvc_joint_coverage_target"]] == "square_footage") {
#     ## based on square footage
#     # A list with vectors containing the setting sizes multiplied by the size per individual.
#     setting_size_list <- list(
#       "workplace" = get_setting_size(variables_list, "workplace") *
#         parameters_list$size_per_individual_workplace,
#       "school" = get_setting_size(variables_list, "school") *
#         parameters_list$size_per_individual_school,
#       # "household" = get_setting_size(variables_list, "household") * parameters_list$size_per_individual_household,
#       "leisure" = parameters_list$setting_sizes$leisure *
#         parameters_list$size_per_individual_leisure
#     )
#   } else {
#     stop(
#       "far_uvc_joint_coverage_target must be either individuals or square_footage"
#     )
#   }
#
#   # Creating a single vector with all setting sizes together that we use to assign farUVC coverage
#   setting_size_flat <- unlist(setting_size_list, use.names = FALSE)
#   total_size <- sum(setting_size_flat)
#   total_length <- length(setting_size_flat)
#   uvc_switches <- rep(0, total_length)
#   total_uvc_size <- total_size * parameters_list[["far_uvc_joint_coverage"]]
#
#   # Assigning farUVC to settings either at random or based on their riskiness
#   if (parameters_list[["far_uvc_joint_coverage_type"]] == "random") {
#     sum <- 0
#     indices <- c()
#     location_indices <- 1:total_length
#     while (sum < total_uvc_size) {
#       i <- sample(location_indices, 1)
#       sum <- sum + setting_size_flat[i]
#       indices <- c(indices, i)
#       location_indices <- setdiff(location_indices, i)
#       if (length(location_indices) == 0 & sum < total_uvc_size) {
#         stop("Insufficient space to meet far UVC coverage")
#       }
#     }
#   } else if (parameters_list[["far_uvc_joint_coverage_type"]] == "targeted_riskiness") {
#     riskiness_list <- list(
#       "workplace" = parameters_list$workplace_specific_riskiness,
#       "school" = parameters_list$school_specific_riskiness,
#       # "household" = parameters_list$household_specific_riskiness,
#       "leisure" = parameters_list$leisure_specific_riskiness
#     )
#     riskiness_flat <- unlist(riskiness_list, use.names = FALSE)
#     riskiness_sorted <- sort(
#       x = riskiness_flat,
#       decreasing = TRUE,
#       index.return = TRUE
#     )
#     final_index <- min(which(
#       cumsum(setting_size_flat[riskiness_sorted$ix]) >= total_uvc_size
#     ))
#     indices <- riskiness_sorted$ix[1:final_index]
#   } else {
#     stop(
#       "far_uvc_joint_coverage_type must be either random or targeted_riskiness"
#     )
#   }
#   uvc_switches[indices] <- 1
#
#   # Now we need to extract out the parts of uvc_switches which correspond to each setting
#   setting_name_index <- rep(
#     names(setting_size_list),
#     lengths(setting_size_list)
#   )
#   parameters_list[["uvc_workplace"]] <- uvc_switches[
#     setting_name_index == "workplace"
#   ]
#   parameters_list[["uvc_school"]] <- uvc_switches[
#     setting_name_index == "school"
#   ]
#   # parameters_list[["uvc_household"]] <- uvc_switches[setting_name_index == "household"]
#   parameters_list[["uvc_leisure"]] <- uvc_switches[
#     setting_name_index == "leisure"
#   ]
#
#   return(parameters_list)
# }


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
#' Adapted from `generate_joint_far_uvc_switches()` (kept as comments above).
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

# #' Generate far UVC switches for particular setting
# #'
# #' This is a helper function to generate the far UVC switches for each given
# #' location, as used in `generate_far_uvc_switches()`. With buildings as the
# #' target, then a specified number of buildings have far UVC installed (either
# #' randomly selected, or in decreasing order of size). Alternatively, with
# #' individuals as the target, buildings are chosen (again either at random or
# #' in decreasing order of size) until a specified number of individuals recieve
# #' the far UVC intervention.
# #'
# #' @param parameters_list A list of model parameters as generated using `get_parameters()`
# #' @param variables_list A list of model variables as generated using `create_variables()`
# #' @param setting One of `"workplace"`, `"school"`, `"household"`, or `"leisure"`
# #'
# #' @family intervention
# #' @export
# generate_setting_far_uvc_switches <- function(
    #   parameters_list,
#   variables_list,
#   setting
# ) {
#   # Defining how coverage is defined (i.e. based on the number of individuals a location holds, or its square footage)
#   if (parameters_list[[paste0("far_uvc_", setting, "_coverage_target")]] == "individuals") {
#     ## based on number of individuals
#     if (setting == "leisure") {
#       setting_size <- parameters_list$setting_sizes$leisure
#     } else {
#       setting_size <- get_setting_size(variables_list, setting = setting)
#     }
#   } else if (
#     parameters_list[[paste0("far_uvc_", setting, "_coverage_target")]] == "square_footage"
#   ) {
#     ## based on square footage
#     if (setting == "leisure") {
#       setting_size <- parameters_list$setting_sizes$leisure *
#         parameters_list[[paste0("size_per_individual_", setting)]]
#     } else {
#       setting_size <- get_setting_size(variables_list, setting = setting) *
#         parameters_list[[paste0("size_per_individual_", setting)]]
#     }
#   } else {
#     stop("coverage_target must be either individuals or square_footage")
#   }
#
#   # Summing total size of locations and creating a vector to store the farUVC indicator variable
#   total <- sum(setting_size)
#   uvc_switches <- rep(0, length(setting_size))
#   total_with_uvc <- floor(
#     parameters_list[[paste0("far_uvc_", setting, "_coverage")]] * total
#   )
#
#   if (parameters_list[[paste0("far_uvc_", setting, "_coverage_type")]] == "random") {
#     sum <- 0
#     indices <- c()
#     location_indices <- 1:length(setting_size)
#
#     while (sum < total_with_uvc) {
#       i <- sample(location_indices, 1)
#       sum <- sum + setting_size[i]
#       indices <- c(indices, i)
#       location_indices <- setdiff(location_indices, i)
#       if (length(location_indices) == 0 & sum < total_with_uvc) {
#         stop("Insufficient individuals to meet far UVC coverage")
#       }
#     }
#     uvc_switches[indices] <- 1
#     parameters_list[[paste0("uvc_", setting)]] <- uvc_switches
#   } else if (
#     parameters_list[[paste0("far_uvc_", setting, "_coverage_type")]] == "targeted_riskiness"
#   ) {
#     riskiness <- parameters_list[[paste0(setting, "_specific_riskiness")]]
#     riskiness_sorted <- sort(
#       x = riskiness,
#       decreasing = TRUE,
#       index.return = TRUE
#     )
#     final_index <- min(which(
#       cumsum(setting_size[riskiness_sorted$ix]) >= total_with_uvc
#     ))
#     indices <- riskiness_sorted$ix[1:final_index]
#     uvc_switches[indices] <- 1
#     parameters_list[[paste0("uvc_", setting)]] <- uvc_switches
#   } else {
#     stop("coverage_type must be either random or targeted_riskiness")
#   }
#
#   return(parameters_list)
# }


#' Generate intervention switches for a particular setting
#'
#' Helper to generate the intervention coverage vector for one setting, as used
#' in `generate_intervention_switches()`. Coverage is interpreted as the
#' fraction of total size to cover (size weighted either by number of
#' individuals or by square footage). Locations are picked until cumulative
#' size meets the budget, either at random or in decreasing order of riskiness.
#'
#' Adapted from `generate_setting_far_uvc_switches()` (kept as comments above).
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


# =============================================================================
# Wells-Riley ACH-based intervention pipeline
# =============================================================================
# Constructor for an intervention object. The intervention's effect on per-
# location alpha (ACH + decay) is described by `baseline_ach_function`, which
# can either depend on the location's baseline ACH or be a fixed delta.
make_intervention <- function(name,
                              affected_by_baseline_ach = FALSE,
                              baseline_ach_function    = NULL,
                              baseline_ach_params      = list(),
                              variation                = FALSE,
                              variation_function       = NULL,
                              variation_params         = list(),
                              coverage                 = NULL) {
  list(
    name                     = name,
    affected_by_baseline_ach = affected_by_baseline_ach,
    baseline_ach_function    = baseline_ach_function,
    baseline_ach_params      = baseline_ach_params,
    variation                = variation,
    variation_function       = variation_function,
    variation_params         = variation_params,
    coverage                 = coverage
  )
}

# Store an intervention for a setting in parameters_list. Single-intervention
# only for now; multi-intervention design is deferred until clumped vs.
# independent coverage is settled.
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

# # Replaced by generate_setting_intervention_switches (which uses the
# # size-weighted budget approach matching the old far-UVC infrastructure).
# # The function below treated coverage as fraction-of-locations rather than
# # fraction-of-size, which differs from how analyses have been parameterised.
# generate_intervention_coverage_vector <- function(parameters_list, setting, num_locations) {
#   coverage      <- parameters_list[[paste0("intervention_", setting, "_coverage")]]
#   coverage_type <- parameters_list[[paste0("intervention_", setting, "_coverage_type")]]
#
#   n_covered <- round(coverage * num_locations)
#
#   if (is.null(coverage_type) || coverage_type == "random") {
#     covered_idx <- sample.int(num_locations, n_covered)
#   } else if (coverage_type == "targeted_riskiness") {
#     riskiness <- parameters_list[[paste0(setting, "_specific_riskiness")]]
#     if (is.null(riskiness)) {
#       stop(paste0("coverage_type = 'targeted_riskiness' requires ",
#                   setting, "_specific_riskiness to be populated"))
#     }
#     covered_idx <- order(riskiness, decreasing = TRUE)[seq_len(n_covered)]
#   } else {
#     stop(paste0("Unknown coverage_type: ", coverage_type))
#   }
#
#   coverage_vector <- rep(0L, num_locations)
#   coverage_vector[covered_idx] <- 1L
#   return(coverage_vector)
# }

# Compute per-location intervention efficacy from baseline ACH using W-R.
# Efficacy at location i = 1 - p_post[i] / p_pre[i], where the post-intervention
# alpha is augmented by the sum of intervention deltas (zeroed for uncovered
# locations via the coverage vector).
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

  for (intervention in interventions) {

    # call baseline_ach_function to get delta for each location
    if (intervention$affected_by_baseline_ach) {
      # pass baseline ACH as first argument, then params
      delta_i <- mapply(
        function(ach) do.call(intervention$baseline_ach_function,
                              c(list(ach), intervention$baseline_ach_params)),
        ach_values
      )
    } else {
      # function only uses its own params — same delta replicated across locations
      delta_i <- rep(
        do.call(intervention$baseline_ach_function, intervention$baseline_ach_params),
        n
      )
    }

    # add location-to-location variation if requested
    if (intervention$variation && !is.null(intervention$variation_function)) {
      noise   <- do.call(intervention$variation_function,
                         c(list(n), intervention$variation_params))
      delta_i <- pmax(0, delta_i + noise)
    }

    # zero out delta for uncovered locations
    if (!is.null(coverage_vector)) {
      delta_i <- delta_i * coverage_vector
    }

    total_delta <- total_delta + delta_i
  }

  alpha_pre  <- ach_values + kD
  alpha_post <- ach_values + kD + total_delta

  p_pre  <- 1 - exp(-r * (I * pi / (alpha_pre  * V)) * RRtv * t)
  p_post <- 1 - exp(-r * (I * pi / (alpha_post * V)) * RRtv * t)

  return(1 - p_post / p_pre)
}


# =============================================================================
# Helper functions for ACH / efficacy / UV-C conversions
# =============================================================================
# Convert UVC (f: fraction of room irradiated, E_avg: avg fluence rate,
# k: UV inactivation constant) to a delta in ACH-equivalent units (hr^-1).
uv_to_delta <- function(f, E_avg, k) {
  f * E_avg * k * 3.6
}

# ACH -> efficacy (Wells-Riley). delta is the total added ACH-equivalent
# (ventilation increase + UV-C inactivation expressed as eACH).
# Defaults match the live pipeline in calculate_efficacy_from_ach (kD, r, pi,
# RRtv, t). V is setting-specific (workplace=27, school=10, leisure=8,
# household=50) so the caller must supply it explicitly.
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

# efficacy -> delta ACH (inverse of ach_to_efficacy). Same default conventions
# as ach_to_efficacy: V is required, other W-R parameters match the live
# pipeline.
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
