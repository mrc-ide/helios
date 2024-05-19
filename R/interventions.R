#' set_uvc
#'
#' @description
#' The set_uvc() function is a user-facing function that is used to parameterise far UVC deployment
#' for an individual setting class (e.g. workplace). The function takes as arguments a `helios`
#' parameter list, the `setting` for which far UVC deployment is being parameterised, the coverage
#' of far UVC within each setting of a setting class (e.g. coverage of individual workplaces within
#' the workplace setting class), the `coverage_type` (currently supporting random and targeted at the
#' most populated settings), the `efficacy` of far UVC in the setting class, and the `timestep` on
#' which far UVC is deployed. The function appends these additional setting-specific parameters to
#' the parameter list and returns an updated version of it.
#'
#' @param parameters_list A list of parameters as generated using `get_parameters`
#' @param setting A character string describing the setting in which Far UVC is being deployed
#' @param coverage A numeric value describing the proportion of the settings in which Far UVC is deployed
#' @param coverage_style A character describing the type of coverage (random or targeted)
#' @param efficacy A numeric value describing the efficacy of the Far UVC deployed
#' @param timestep A numeric value describing the timestep in which Far UVC is deployed
#'
#' @family intervention
#' @export
set_uvc <- function(parameters_list, setting, coverage, coverage_type, efficacy, timestep) {

  # Ensure only a single setting passed to the function:
  if(length(setting) > 1) {
    stop("Error: Number of settings input greater than 1, parameterise for one setting at a time")
  }

  # Ensure only one coverage type is passed to the function:
  if(length(coverage_type) > 1) {
    stop("Error: Number of coverage types input greater than 1, parameterise for one coverage type at a time")
  }

  # Ensure input setting is from the allowed options
  if(setting != "workplace" & setting != "school" & setting != "leisure" & setting != "household") {
    stop("Error: Input setting invalid - far UVC only deployable in workplace, school, leisure, or household settings")
  }

  # Ensure the coverage type is from the allowed options:
  # Ensure input setting is from the allowed options
  if(coverage_type != "random" & coverage_type != "targeted") {
    stop("Error: Input setting invalid - far UvC only deployable in random or targeted coverage types")
  }

  # Check that coverage is between 0 and 1:
  if(coverage < 0 | coverage > 1) {
    stop("Error: coverage must take a value between 0 and 1")
  }

  # Check that efficacy is between 0 and 1:
  if(efficacy < 0 | efficacy > 1) {
    stop("Error: efficacy must take a value between 0 and 1")
  }

  # Switch on FAR UVC for the specified setting:
  parameters_list[[paste0("far_uvc_", setting)]] <- TRUE

  # Append the setting-specific Far-UVC coverage type:
  parameters_list[[paste0("far_uvc_", setting, "_coverage_type")]] <- coverage_type

  # Append the setting-specific Far-UVC coverage:
  parameters_list[[paste0("far_uvc_", setting, "_coverage")]] <- coverage

  # Append the setting-specific Far-UVC efficacy:
  parameters_list[[paste0("far_uvc_", setting, "_efficacy")]] <- efficacy

  # Append the setting-specific Far-UVC timestep:
  parameters_list[[paste0("far_uvc_", setting, "_timestep")]] <- timestep

  # Return the modified list of parameters
  parameters_list

}

#' generate_far_uvc_switches
#'
#' @description
#' generate_far_uvc_switches() determines which individual settings will deploy far UVC given the settings
#' switched on, the setting-specific coverages, and the setting-specific coverage types. The function returns
#' generates, for each the workplace, school, leisure, and household settings, a vector of length equal to the
#' the number of settings within in setting type (e.g. number of schools within the school setting class)
#' populated with 1's and 0's, where a 1 represents the presence of far UVC and a 0 the absence of far
#' UVC. The function returns an updated parameter list with these vectors appended for each setting class
#' for which far UVC has been parameterised using the `set_uvc()` function.
#'
#'
#' @param parameters_list A list of model parameters as generated using `get_parameters()`
#' @param variables_list A list of model variables as generated using `create_variables()`
#'
#' @family intervention
#' @export
generate_far_uvc_switches <- function(parameters_list, variables_list) {

  # Parameterise Far UVC in the workplace setting:
  if(parameters_list$far_uvc_workplace) {

    # Get the number of workplaces:
    number_of_workplaces <- length(variables_list$workplace$get_categories())

    # Open a vector to store the workplace UVC on/off values:
    uvc_workplace <- rep(0, number_of_workplaces)

    # Calculate the number of workplaces that will have Far UVC:
    number_of_workplaces_with_uvc <- floor(parameters_list$far_uvc_workplace_coverage * number_of_workplaces)

    # If coverage type is random, assign workplaces at random:
    if(parameters_list$far_uvc_workplace_coverage_type == "random") {

      # Sample the indices of workplaces to have Far UVC at random:
      indices_of_workplaces_with_uvc <- sample.int(n = number_of_workplaces,
                                                   size = number_of_workplaces_with_uvc,
                                                   replace = FALSE)

      # Insert 1's at the indices of workplaces selected to have Far-UVC:
      uvc_workplace[indices_of_workplaces_with_uvc] <- 1

      # Append the vector of workplace UVC on/off values to the variables_list:
      parameters_list$uvc_workplace <- uvc_workplace

    }

    # If coverage type is targeted, assign UVC to the most populous workplaces:
    if(parameters_list$far_uvc_workplace_coverage_type == "targeted") {

      # Get the indices of the indices of workplaces with Far UVC based on their sizes:
      indices_of_workplaces_with_uvc <- sort(x = parameters_list$setting_sizes$workplace,
                                             decreasing = TRUE,
                                             index.return = TRUE)$ix[1:number_of_workplaces_with_uvc]

      # Insert 1's at the indices of workplaces selected to have Far-UVC:
      uvc_workplace[indices_of_workplaces_with_uvc] <- 1

      # Append the vector of workplace UVC on/off values to the variables_list:
      parameters_list$uvc_workplace <- uvc_workplace

    }
  }

  # Parameterise Far UVC in the school setting:
  if(parameters_list$far_uvc_school) {

    # Get the number of schools:
    number_of_schools <- length(variables_list$school$get_categories())

    # Open a vector to store the school UVC on/off values:
    uvc_school <- rep(0, number_of_schools)

    # Calculate the number of schools that will have Far UVC:
    number_of_schools_with_uvc <- floor(parameters_list$far_uvc_school_coverage * number_of_schools)

    # If coverage type is random, assign schools at random:
    if(parameters_list$far_uvc_school_coverage_type == "random") {

      # Sample the indices of schools to have Far UVC at random:
      indices_of_schools_with_uvc <- sample.int(n = number_of_schools,
                                                size = number_of_schools_with_uvc,
                                                replace = FALSE)

      # Insert 1's at the indices of schools selected to have Far-UVC:
      uvc_school[indices_of_schools_with_uvc] <- 1

      # Append the vector of school UVC on/off values to the variables_list:
      parameters_list$uvc_school <- uvc_school

    }
    # If coverage type is targeted, assign UVC to the most populous schools:
    if(parameters_list$far_uvc_school_coverage_type == "targeted") {

      # Get the indices of the indices of schools with Far UVC based on their sizes:
      indices_of_schools_with_uvc <- sort(x = parameters_list$setting_sizes$school,
                                          decreasing = TRUE,
                                          index.return = TRUE)$ix[1:number_of_schools_with_uvc]

      # Insert 1's at the indices of schools selected to have Far-UVC:
      uvc_school[indices_of_schools_with_uvc] <- 1

      # Append the vector of school UVC on/off values to the variables_list:
      parameters_list$uvc_school <- uvc_school

    }
  }

  # Parameterise Far UVC in the leisure setting:
  if(parameters_list$far_uvc_leisure) {

    # Get the number of leisure settings:
    number_of_leisures <- length(parameters_list$leisure_setting_sizes)

    # Open a vector to store the leisure UVC on/off values:
    uvc_leisure <- rep(0, number_of_leisures)

    # Calculate the number of leisure settings that will have Far UVC:
    number_of_leisures_with_uvc <- floor(parameters_list$far_uvc_leisure_coverage * number_of_leisures)

    # If coverage type is random, assign leisure settings at random:
    if(parameters_list$far_uvc_leisure_coverage_type == "random") {

      # Sample the indices of leisure settings to have Far UVC at random:
      indices_of_leisures_with_uvc <- sample.int(n = number_of_leisures,
                                                 size = number_of_leisures_with_uvc,
                                                 replace = FALSE)

      # Insert 1's at the indices of leisure settings selected to have Far-UVC:
      uvc_leisure[indices_of_leisures_with_uvc] <- 1

      # Append the vector of leisure UVC on/off values to the variables_list:
      parameters_list$uvc_leisure <- uvc_leisure

    }
    # If coverage type is targeted, assign UVC to the most populous leisure settings:
    if(parameters_list$far_uvc_leisure_coverage_type == "targeted") {

      # Get the indices of the indices of leisure settings with Far UVC based on their sizes:
      indices_of_leisures_with_uvc <- sort(x = parameters_list$setting_sizes$leisure,
                                           decreasing = TRUE,
                                           index.return = TRUE)$ix[1:number_of_leisures_with_uvc]

      # Insert 1's at the indices of leisure settings selected to have Far-UVC:
      uvc_leisure[indices_of_leisures_with_uvc] <- 1

      # Append the vector of leisure setting UVC on/off values to the variables_list:
      parameters_list$uvc_leisure <- uvc_leisure

    }
  }

  # Parameterise Far UVC in the household setting:
  if(parameters_list$far_uvc_household) {

    # Get the number of households:
    number_of_households <- length(variables_list$household$get_categories())

    # Open a vector to store the household UVC on/off values:
    uvc_household <- rep(0, number_of_households)

    # Calculate the number of households that will have Far UVC:
    number_of_households_with_uvc <- floor(parameters_list$far_uvc_household_coverage * number_of_households)

    # If coverage type is random, assign households at random:
    if(parameters_list$far_uvc_household_coverage_type == "random") {

      # Sample the indices of households to have Far UVC at random:
      indices_of_households_with_uvc <- sample.int(n = number_of_households,
                                                   size = number_of_households_with_uvc,
                                                   replace = FALSE)

      # Insert 1's at the indices of households selected to have Far-UVC:
      uvc_household[indices_of_households_with_uvc] <- 1

      # Append the vector of household UVC on/off values to the variables_list:
      parameters_list$uvc_household <- uvc_household

    }

    # If coverage type is targeted, assign UVC to the most populous households:
    if(parameters_list$far_uvc_household_coverage_type == "targeted") {

      # Get the indices of the indices of households with Far UVC based on their sizes:
      indices_of_households_with_uvc <- sort(x = parameters_list$setting_sizes$household,
                                             decreasing = TRUE,
                                             index.return = TRUE)$ix[1:number_of_households_with_uvc]

      # Insert 1's at the indices of households selected to have Far-UVC:
      uvc_household[indices_of_households_with_uvc] <- 1

      # Append the vector of leisure setting UVC on/off values to the variables_list:
      parameters_list$uvc_household <- uvc_household

    }
  }

  # Return the update list of model parameters:
  parameters_list

}
