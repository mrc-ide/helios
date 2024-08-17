#' Update model parameters with far UVC settings
#'
#' @description
#' The `set_uvc()` function is a user-facing function that is used to parameterise far UVC deployment
#' for an individual setting type (e.g. workplace). The function takes as arguments a `helios`
#' parameter list, the `setting` type for which far UVC deployment is being parameterised, the `coverage`
#' of far UVC within each location of a setting type (e.g. coverage of individual workplaces within
#' the workplace setting class), the `coverage_type` (currently supporting random and targeted at the
#' most populated settings), the `efficacy` of far UVC in the setting type, and the `timestep` on
#' which far UVC is deployed. The function appends these additional setting-specific parameters to
#' the parameter list and returns an updated version of it.
#'
#' @param parameters_list A list of parameters as generated using `get_parameters()`
#' @param setting A character string describing the setting type in which far UVC is being deployed
#' @param coverage A numeric value describing the coverage of far UVC within the setting class for which which far UVC is deployed
#' @param coverage_target A character describing the target of the coverage ("buildings" or "individuals")
#' @param coverage_type A character describing the type of coverage ("random" or "targeted")
#' @param efficacy A numeric value describing the efficacy of the far UVC deployed
#' @param timestep A numeric value describing the timestep in which far UVC is deployed
#'
#' @family intervention
#' @export
set_uvc <- function(parameters_list, setting, coverage, coverage_target, coverage_type, efficacy, timestep) {
  if (length(setting) > 1) {
    stop("Error: Number of settings input greater than 1, parameterise for one setting at a time")
  }

  if (!(setting %in% c("workplace", "school", "leisure", "household", "joint"))) {
    stop("Error: Input setting invalid - far UVC only deployable in workplace, school, leisure, household, or joint settings")
  }

  if (coverage < 0 | coverage > 1) {
    stop("Error: coverage must take a value between 0 and 1")
  }

  if (length(coverage_target) > 1) {
    stop("Error: Number of coverage targets input greater than 1, parameterise for one coverage target at a time")
  }

  if (coverage_target != "individuals" & coverage_target != "square_footage") {
    stop("Error: Input setting invalid - far UVC coverage only applicable to individuals or square_footage")
  }

  if (length(coverage_type) > 1) {
    stop("Error: Number of coverage types input greater than 1, parameterise for one coverage type at a time")
  }

  if (coverage_type != "random" & coverage_type != "targeted_riskiness") {
    stop("Error: Input setting invalid - far UVC only deployable in random or targeted_riskiness coverage types")
  }

  if (efficacy < 0 | efficacy > 1) {
    stop("Error: efficacy must take a value between 0 and 1")
  }

  parameters_list[[paste0("far_uvc_", setting)]] <- TRUE
  parameters_list[[paste0("far_uvc_", setting, "_coverage")]] <- coverage
  parameters_list[[paste0("far_uvc_", setting, "_coverage_target")]] <- coverage_target
  parameters_list[[paste0("far_uvc_", setting, "_coverage_type")]] <- coverage_type
  parameters_list[[paste0("far_uvc_", setting, "_efficacy")]] <- efficacy
  parameters_list[[paste0("far_uvc_", setting, "_timestep")]] <- timestep

  return(parameters_list)
}

#' Update model parameters with far UVC switches
#'
#' @description
#' `generate_far_uvc_switches()` determines which locations will deploy far UVC given the setting type is
#' switched on, the setting-specific coverages, and the setting-specific coverage types. The function returns,
#' for each the workplace, school, leisure, and household settings, a vector of length equal to the
#' the number of locations within the setting type (e.g. number of schools within the school setting type)
#' populated with 1's and 0's, where a 1 represents the presence of far UVC and a 0 the absence of far
#' UVC. The function returns an updated parameter list with these vectors appended for each setting type
#' for which far UVC has been parameterised using the `set_uvc()` function.
#'
#' @param parameters_list A list of model parameters as generated using `get_parameters()`
#' @param variables_list A list of model variables as generated using `create_variables()`
#'
#' @family intervention
#' @export
generate_far_uvc_switches <- function(parameters_list, variables_list) {

  # Checking that if far_uvc_joint = TRUE, no Setting-Type specific farUVC switches have been turned on
  setting_types <- c("workplace", "school", "leisure", "household")
  if (parameters_list$far_uvc_joint & any(unlist(parameters_list[paste0("far_uvc_", setting_types)]))) {
    stop("If far_uvc_joint is set to TRUE, setting-type specific far_UVC switches must be set to FALSE")
  }

  # If far_uvc_joint = TRUE calculate farUVC coverage for all locations across all setting-types altogether
  if (parameters_list$far_uvc_joint) {
    parameters_list <- generate_joint_far_uvc_switches(parameters_list, variables_list)
  } else {
    # Else, check if there is UVC for any of these setting-types and turn it on if so
    for (setting in setting_types) {
      # If the setting-type has farUVC, generate the switches using the helper function generate_setting_far_uvc_switches
      if (parameters_list[[paste0("far_uvc_", setting)]]) {
        parameters_list <- generate_setting_far_uvc_switches(
          parameters_list, variables_list, setting = setting
        )
      }
    }
  }
  return(parameters_list)
}

#' Generate joint far UVC switches
#'
#' This is a helper function to generate the joint far UVC switches across all
#' locations, as used in `generate_far_uvc_switches()`.
#'
#' @param parameters_list A list of model parameters as generated using `get_parameters()`
#' @param variables_list A list of model variables as generated using `create_variables()`
#'
#' @family intervention
#' @export
generate_joint_far_uvc_switches <- function(parameters_list, variables_list) {

  # Defining how coverage is defined (i.e. based on the number of individuals a location holds, or its square footage)
  if (parameters_list[["far_uvc_joint_coverage_target"]] == "individuals") { ## based on number of individuals
    # A list with vectors containing the setting sizes
    setting_size_list <- list(
      "workplace" = get_setting_size(variables_list, "workplace"),
      "school" = get_setting_size(variables_list, "school"),
      "household" = get_setting_size(variables_list, "household"),
      "leisure" = parameters_list$setting_sizes$leisure
    )
  # Defining coverage according to the size of the setting (i.e. number of individuals multiplied by square footage per person)
  } else if (parameters_list[["far_uvc_joint_coverage_target"]] == "square_footage") { ## based on square footage
    # A list with vectors containing the setting sizes multiplied by the size per individual.
    setting_size_list <- list(
      "workplace" = get_setting_size(variables_list, "workplace") * parameters_list$size_per_individual_workplace,
      "school" = get_setting_size(variables_list, "school") * parameters_list$size_per_individual_school,
      "household" = get_setting_size(variables_list, "household") * parameters_list$size_per_individual_household,
      "leisure" = parameters_list$setting_sizes$leisure * parameters_list$size_per_individual_leisure
    )
  } else {
    stop("far_uvc_joint_coverage_target must be either individuals or square_footage")
  }

  # Creating a single vector with all setting sizes together that we use to assign farUVC coverage
  setting_size_flat <- unlist(setting_size_list, use.names = FALSE)
  total_size <- sum(setting_size_flat)
  total_length <- length(setting_size_flat)
  uvc_switches <- rep(0, total_length)
  total_uvc_size <- total_size * parameters_list[["far_uvc_joint_coverage"]]

  # Assigning farUVC to settings either at random or based on their riskiness
  if (parameters_list[["far_uvc_joint_coverage_type"]] == "random") {
    sum <- 0
    indices <- c()
    location_indices <- 1:total_length
    while (sum < total_uvc_size) {
      i <- sample(location_indices, 1)
      sum <- sum + setting_size_flat[i]
      indices <- c(indices, i)
      location_indices <- setdiff(location_indices, i)
      if (length(location_indices) == 0) {
        stop("Insufficient space to meet far UVC coverage")
      }
    }
  } else if (parameters_list[["far_uvc_joint_coverage_type"]] == "targeted_riskiness") {
    riskiness_list <- list(
      "workplace" = parameters_list$workplace_specific_riskiness,
      "school" = parameters_list$school_specific_riskiness,
      "household" = parameters_list$household_specific_riskiness,
      "leisure" = parameters_list$leisure_specific_riskiness
    )
    riskiness_flat <- unlist(riskiness_list, use.names = FALSE)
    riskiness_sorted <- sort(x = riskiness_flat, decreasing = TRUE, index.return = TRUE)
    final_index <- min(which(cumsum(setting_size_flat[riskiness_sorted$ix]) >= total_uvc_size))
    indices <- riskiness_sorted$ix[1:final_index]
  } else {
    stop("far_uvc_joint_coverage_type must be either random or targeted_riskiness")
  }
  uvc_switches[indices] <- 1

  # Now we need to extract out the parts of uvc_switches which correspond to each setting
  setting_name_index <- rep(names(setting_size_list), lengths(setting_size_list))
  parameters_list[["uvc_workplace"]] <- uvc_switches[setting_name_index == "workplace"]
  parameters_list[["uvc_school"]] <- uvc_switches[setting_name_index == "school"]
  parameters_list[["uvc_household"]] <- uvc_switches[setting_name_index == "household"]
  parameters_list[["uvc_leisure"]] <- uvc_switches[setting_name_index == "leisure"]

  return(parameters_list)
}

#' Generate far UVC switches for particular setting
#'
#' This is a helper function to generate the far UVC switches for each given
#' location, as used in `generate_far_uvc_switches()`. With buildings as the
#' target, then a specified number of buildings have far UVC installed (either
#' randomly selected, or in decreasing order of size). Alternatively, with
#' individuals as the target, buildings are chosen (again either at random or
#' in decreasing order of size) until a specified number of individuals recieve
#' the far UVC intervention.
#'
#' @param parameters_list A list of model parameters as generated using `get_parameters()`
#' @param variables_list A list of model variables as generated using `create_variables()`
#' @param setting One of `"workplace"`, `"school"`, `"household"`, or `"leisure"`
#'
#' @family intervention
#' @export
generate_setting_far_uvc_switches <- function(parameters_list, variables_list, setting) {

  # Defining how coverage is defined (i.e. based on the number of individuals a location holds, or its square footage)
  if (parameters_list[[paste0("far_uvc_", setting, "_coverage_target")]] == "individuals") { ## based on number of individuals
    if (setting == "leisure") {
      setting_size <- parameters_list$setting_sizes$leisure
    } else {
      setting_size <- get_setting_size(variables_list, setting = setting)
    }
  } else if (parameters_list[[paste0("far_uvc_", setting, "_coverage_target")]] == "square_footage") { ## based on square footage
    if (setting == "leisure") {
      setting_size <- parameters_list$setting_sizes$leisure * parameters_list[[paste0("size_per_individual_", setting)]]
    } else {
      setting_size <- get_setting_size(variables_list, setting = setting) * parameters_list[[paste0("size_per_individual_", setting)]]
    }
  } else {
    stop("coverage_target must be either individuals or square_footage")
  }

  # Summing total size of locations and creating a vector to store the farUVC indicator variable
  total <- sum(setting_size)
  uvc_switches <- rep(0, length(setting_size))
  total_with_uvc <- floor(parameters_list[[paste0("far_uvc_", setting, "_coverage")]] * total)

  if (parameters_list[[paste0("far_uvc_", setting, "_coverage_type")]] == "random") {
    sum <- 0
    indices <- c()
    location_indices <- 1:length(setting_size)

    while (sum < total_with_uvc) {
      i <- sample(location_indices, 1)
      sum <- sum + setting_size[i]
      indices <- c(indices, i)
      location_indices <- setdiff(location_indices, i)
      if (length(location_indices) == 0) {
        stop("Insufficient individuals to meet far UVC coverage")
      }
    }
    uvc_switches[indices] <- 1
    parameters_list[[paste0("uvc_", setting)]] <- uvc_switches

  } else if (parameters_list[[paste0("far_uvc_", setting, "_coverage_type")]] == "targeted_riskiness") {

    riskiness <- parameters_list[[paste0(setting, "_specific_riskiness")]]
    riskiness_sorted <- sort(x = riskiness, decreasing = TRUE, index.return = TRUE)
    final_index <- min(which(cumsum(setting_size[riskiness_sorted$ix]) >= total_with_uvc))
    indices <- riskiness_sorted$ix[1:final_index]
    uvc_switches[indices] <- 1
    parameters_list[[paste0("uvc_", setting)]] <- uvc_switches

  } else {
    stop("coverage_type must be either random or targeted_riskiness")
  }

  return(parameters_list)
}
