#' Update model parameters with far UVC settings
#'
#' @description
#' The `set_uvc()` function is a user-facing function that is used to parameterise far UVC deployment
#' for an individual setting class (e.g. workplace). The function takes as arguments a `helios`
#' parameter list, the `setting` for which far UVC deployment is being parameterised, the `coverage`
#' of far UVC within each setting of a setting class (e.g. coverage of individual workplaces within
#' the workplace setting class), the `coverage_type` (currently supporting random and targeted at the
#' most populated settings), the `efficacy` of far UVC in the setting class, and the `timestep` on
#' which far UVC is deployed. The function appends these additional setting-specific parameters to
#' the parameter list and returns an updated version of it.
#'
#' @param parameters_list A list of parameters as generated using `get_parameters()`
#' @param setting A character string describing the setting in which far UVC is being deployed
#' @param coverage A numeric value describing the proportion of the settings in which far UVC is deployed
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

  if (!(setting %in% c("workplace", "school", "leisure", "household"))) {
    stop("Error: Input setting invalid - far UVC only deployable in workplace, school, leisure, or household settings")
  }

  if (coverage < 0 | coverage > 1) {
    stop("Error: coverage must take a value between 0 and 1")
  }

  if (length(coverage_target) > 1) {
    stop("Error: Number of coverage targets input greater than 1, parameterise for one coverage target at a time")
  }

  if (coverage_target != "individuals" & coverage_target != "buildings") {
    stop("Error: Input setting invalid - far UVC coverage only applicable to individuals or buildings")
  }

  if (length(coverage_type) > 1) {
    stop("Error: Number of coverage types input greater than 1, parameterise for one coverage type at a time")
  }

  if (coverage_type != "random" & coverage_type != "targeted") {
    stop("Error: Input setting invalid - far UVC only deployable in random or targeted coverage types")
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
#' `generate_far_uvc_switches()` determines which individual settings will deploy far UVC given the settings
#' switched on, the setting-specific coverages, and the setting-specific coverage types. The function returns,
#' for each the workplace, school, leisure, and household settings, a vector of length equal to the
#' the number of settings within in setting type (e.g. number of schools within the school setting class)
#' populated with 1's and 0's, where a 1 represents the presence of far UVC and a 0 the absence of far
#' UVC. The function returns an updated parameter list with these vectors appended for each setting class
#' for which far UVC has been parameterised using the `set_uvc()` function.
#'
#' @param parameters_list A list of model parameters as generated using `get_parameters()`
#' @param variables_list A list of model variables as generated using `create_variables()`
#'
#' @family intervention
#' @export
generate_far_uvc_switches <- function(parameters_list, variables_list) {
  for (setting in c("workplace", "school", "leisure", "household")) {
    if (parameters_list[[paste0("far_uvc_", setting)]]) {
      parameters_list <- generate_setting_far_uvc_switches(
        parameters_list, variables_list, setting = setting
      )
    }
  }

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
  if (setting == "leisure") {
    setting_size <- parameters_list$leisure_setting_sizes
  } else {
    setting_size <- get_setting_size(variables_list, setting = setting)
  }

  coverage_target <- parameters_list[[paste0("far_uvc_", setting, "_coverage_target")]]
  coverage_type <- parameters_list[[paste0("far_uvc_", setting, "_coverage_type")]]

  if (coverage_target == "buildings") {
    total <- length(setting_size)
    uvc_switches <- rep(0, total)
    total_with_uvc <- floor(parameters_list[[paste0("far_uvc_", setting, "_coverage")]] * total)

    if (coverage_type == "random") {
      indices_with_uvc <- sample.int(n = total, size = total_with_uvc, replace = FALSE)
      uvc_switches[indices_with_uvc] <- 1
      parameters_list[[paste0("uvc_", setting)]] <- uvc_switches
    } else if (coverage_type == "targeted") {
      indices_with_uvc <- sort(
        x = setting_size,
        decreasing = TRUE,
        index.return = TRUE
      )$ix[1:total_with_uvc]
      uvc_switches[indices_with_uvc] <- 1
      parameters_list[[paste0("uvc_", setting)]] <- uvc_switches
    }
  }

  # Note here that the cumulative sum is likely going to be bigger than the required total_with_uvc
  if (coverage_target == "individuals") {
    total <- sum(setting_size)
    uvc_switches <- rep(0, length(setting_size))
    total_with_uvc <- floor(parameters_list[[paste0("far_uvc_", setting, "_coverage")]] * total)

    if (coverage_type == "random") {
      sum <- 0
      indices <- c()
      household_indices <- 1:length(setting_size)

      while (sum < total_with_uvc) {
        i <- sample(household_indices, 1)
        sum <- sum + setting_size[i]
        indices <- c(indices, i)
        household_indices <- setdiff(household_indices, i)
        if (length(household_indices) == 0) {
          stop("Insufficient individuals to meet far UVC coverage")
        }
      }

      uvc_switches[indices] <- 1
      parameters_list[[paste0("uvc_", setting)]] <- uvc_switches

    } else if (coverage_type == "targeted") {
      setting_size_sorted <- sort(x = setting_size, decreasing = TRUE, index.return = TRUE)
      final_index <- min(which(cumsum(setting_size_sorted$x) >= total_with_uvc))
      uvc_switches[1:final_index] <- 1
      parameters_list[[paste0("uvc_", setting)]] <- uvc_switches
    }
  }

  return(parameters_list)
}
