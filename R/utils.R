#' get_setting_size
#'
#' @description
#' get_setting_size() retrieves the number of individuals in each individual
#' location within a setting type (e.g. number of individuals in each workplace,
#' school, or household). The function calculates these from the list of model
#' variables, and returns a vector of individual location sizes.
#'
#' Note that this function is not able to calculate the location sizes for the
#' leisure setting.
#'
#' @param variables_list A list of model variables as generated using `create_variables()`
#' @param leisure_sizes The sizes of the leisure settings as sampled using `sample_negbinom()`
#' @param setting One of `"workplace"`, `"school"`, or `"household"`
#' @family miscellaneous
#' @export
get_setting_size <- function(variables_list, setting) {
  if (!setting %in% c("workplace", "school", "household")) {
    stop('setting must be either "workplace", "school" or "household"')
  }

  location_sizes <- vector()

  if (setting == "workplace") {
    workplaces <- 1:max(as.numeric(variables_list$workplace$get_categories()))
    for (i in workplaces) {
      location_sizes[i] <- variables_list$workplace$get_size_of(as.character(i))
    }
  }

  if (setting == "school") {
    schools <- 1:max(as.numeric(variables_list$school$get_categories()))
    for (i in schools) {
      location_sizes[i] <- variables_list$school$get_size_of(as.character(i))
    }
  }

  if (setting == "household") {
    households <- 1:max(as.numeric(variables_list$household$get_categories()))
    for (i in households) {
      location_sizes[i] <- variables_list$household$get_size_of(as.character(i))
    }
  }

  return(location_sizes)
}

#' timestep_to_day_of_year
#'
#' @description
#' Converts a simulation timestep into a day-of-year index in 1:365, used to look
#' up the seasonal multiplier for time-varying betas. There are `1 / dt` timesteps
#' per day, so the timestep is first mapped to a day via `ceiling(t * dt)` and then
#' recycled annually with a modulo over 365. All timesteps within the same day map
#' to the same day-of-year, and day 366 of a multi-year run wraps back to day 1.
#'
#' @param t The simulation timestep (a positive integer, as passed to a process function).
#' @param dt The timestep length as a fraction of a day (e.g. 0.5 for two timesteps per day).
#' @return An integer in 1:365 giving the day of the year for timestep `t`.
#' @family miscellaneous
#' @export
timestep_to_day_of_year <- function(t, dt) {
  day <- ceiling(t * dt)
  ((day - 1) %% 365) + 1
}

#' generate_betas
#'
#' @description
#' generate_betas() takes a beta_community and returns beta_household, beta_school, beta_workplace,
#' and beta_leisure given user-defined ratios. The function also calculates the total beta value and
#' returns the proportion of the total corresponding to each setting. The beta values are key inputs
#' in the parameters_list as generated using the `get_parameters()` function. The function returns a
#' dataframe containing the beta values and their proportions of the total betas.
#'
#' @param beta_community The beta value, or values, for the community setting for which the user wants to generate corresponding household, school, workplace, and leisure settings.
#' @param household_ratio The household beta as a ratio to the community beta
#' @param school_ratio  The school beta as a ratio to the community beta
#' @param workplace_ratio The workplace beta as a ratio to the community beta
#' @param leisure_ratio The leisure beta as a ratio to the community beta
#' @family miscellaneous
#' @export
generate_betas <- function(
  beta_community,
  household_ratio,
  school_ratio,
  workplace_ratio,
  leisure_ratio
) {
  # Use the community betas to generate the household, school, workplace, and leisure betas:
  beta_household <- household_ratio * beta_community
  beta_school <- school_ratio * beta_community
  beta_workplace <- workplace_ratio * beta_community
  beta_leisure <- leisure_ratio * beta_community

  # Combine the betas into a dataframe:
  betas <- data.frame(
    beta_household = beta_household,
    beta_school = beta_school,
    beta_workplace = beta_workplace,
    beta_leisure = beta_leisure,
    beta_community = beta_community
  )

  # Append columns giving the proportion of the total beta accounted for in each setting:
  betas <- betas %>%
    mutate(
      beta_total = beta_household +
        beta_school +
        beta_workplace +
        beta_leisure +
        beta_community
    ) %>%
    mutate(
      prop_household = beta_household / beta_total,
      prop_school = beta_school / beta_total,
      prop_workplace = beta_workplace / beta_total,
      prop_leisure = beta_leisure / beta_total,
      prop_community = beta_community / beta_total
    )

  # Return the data frame of betas:
  return(betas)
}
