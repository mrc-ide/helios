#' get_setting_sizes
#'
#' @description
#' get_setting_sizes() retrieves the number of individuals in each individual setting within
#' a setting type (e.g. number of individuals in each workplace). The function calculates these
#' from the list of model variables for the workplace, school, and household settings, but takes them
#' directly as an input for leisure.The function returns a list with a vector of individual setting
#' sizes for each setting type.
#'
#' @param variables_list A list of model variables as generated using `create_variables()`
#' @param leisure_sizes The sizes of the leisure settings as sampled using `sample_negbinom()`
#'
#' @export
get_setting_sizes <- function(variables_list, leisure_sizes) {

  # Open list to store the vectors of setting population sizes:
  setting_populations <- list()

  # Retrieve the workplace sizes
  workplace_sizes <- vector()
  workplaces <- 1:max(as.numeric(variables_list$workplace$get_categories()))
  for(i in workplaces) {
    workplace_sizes[i] <- variables_list$workplace$get_size_of(as.character(i))
  }

  # Retrieve the school sizes
  school_sizes <- vector()
  schools <- 1:max(as.numeric(variables_list$school$get_categories()))
  for(i in schools) {
    school_sizes[i] <- variables_list$school$get_size_of(as.character(i))
  }

  # Retrieve the household sizes
  household_sizes <- vector()
  households <- 1:max(as.numeric(variables_list$household$get_categories()))
  for(i in households) {
    household_sizes[i] <- variables_list$household$get_size_of(as.character(i))
  }

  # Populate the list of workplace populations:
  setting_populations$workplace <- workplace_sizes
  setting_populations$school <- school_sizes
  setting_populations$leisure <- leisure_sizes
  setting_populations$household <- household_sizes

  # Return the list of setting populations:
  return(setting_populations)

}

#' generate_betas
#'
#' @param beta_community The beta value, or values, for the community setting for which the user wants to generate corresponding household, school, workplace, and leisure settings.
#' @param household_ratio The household beta as a ratio to the community beta
#' @param school_ratio  The school beta as a ratio to the community beta
#' @param workplace_ratio The workplace beta as a ratio to the community beta
#' @param leisure_ratio The leisure beta as a ratio to the community beta
#'
#' @description
#' generate_betas() takes a beta_community and returns beta_household, beta_school, beta_workplace,
#' and beta_leisure given user-defined ratios. The function also calculates the total beta value and
#' returns the proportion of the total corresponding to each setting. The beta values are key inputs
#' in the parameters_list as generated using the `get_parameters()` function. The function returns a
#' dataframe containing the beta values and their proportions of the total betas.
#'
#' @export
generate_betas <- function(beta_community, household_ratio, school_ratio, workplace_ratio, leisure_ratio) {

  # Use the community betas to generate the household, school, workplace, and leisure betas:
  beta_household = household_ratio * beta_community
  beta_school = school_ratio * beta_community
  beta_workplace = workplace_ratio * beta_community
  beta_leisure = leisure_ratio * beta_community

  # Combine the betas into a dataframe:
  betas <- data.frame(
    beta_household = beta_household,
    beta_school = beta_school,
    beta_workplace = beta_workplace,
    beta_leisure = beta_leisure,
    beta_community = beta_community)

  # Append columns giving the proportion of the total beta accounted for in each setting:
  betas %>%
    mutate(beta_total = beta_household + beta_school + beta_workplace + beta_leisure + beta_community) %>%
    mutate(prop_household = beta_household / beta_total,
           prop_school = beta_school / beta_total,
           prop_workplace = beta_workplace / beta_total,
           prop_leisure = beta_leisure / beta_total,
           prop_community = beta_community / beta_total) -> betas

  # Return the data frame of betas:
  return(betas)

}
