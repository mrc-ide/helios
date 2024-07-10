#' Establish the list of model parameters
#'
#' This function creates a named list of model parameters which are to be used
#' in the model. For example, the output of [get_parameters()] provides input to
#' functions such as [create_variables()] and [create_events()].
#'
#' @param overrides A named list of parameters values to be used instead of the defaults.
#' These parameters are:
#'
#' * `human_population`: the number of humans to include in the model
#' * `initial_proportion_child`: proportion of population initially in the 'child' age class
#' * `initial_proportion_adult`: proportion of population initially in the 'adult' age class
#' * `initial_proportion_elderly`: proportion of population initially in the 'elderly' age class
#' * `number_initially_exposed`: number of humans initially exposed (state = E)
#' * `seed`: a seed to run the simulation with
#' * `mean_household_size`: TBD
#' * `workplace_prop_max`: maximum size of a workplace as a proportion of total adult population size
#' * `workplace_a`: the a parameter for the Zipf-like distribution on workplace size
#' * `workplace_c`: the c parameter for the Zipf-like distribution on workplace size
#' * `school_prop_max`: maximum size of a school as a proporiton of total child population size
#' * `school_meanlog`: the meanlog parameter for the log-normal distribution on school size
#' * `school_sdlog`: the sdlog parameter for the log-normal distribution on school size
#' * `school_student_staff_ratio`: the number of students to each adult staff member
#' * `leisure_mean_number_settings`: TBD
#' * `leisure_mean_size`: TBD
#' * `leisure_overdispersion_size`: TBD
#' * `leisure_prop_max`: TBD
#' * `duration_exposed`: TBD
#' * `duration_infectious`: TBD
#' * `prob_inf_external`: The probability a susceptible individual is infected from an external source
#' * `beta_household`: TBD
#' * `beta_workplace`: TBD
#' * `beta_school`: TBD
#' * `beta_leisure`: TBD
#' * `beta_community`: TBD
#' * `dt`: TBD
#' * `simulation_time`: TBD
#'
#' Rendering Parameters
#' * `render_diagnostics`: FALSE
#'
#' Far UVC Intervention Parameters:
#' * `far_uvc_workplace`: boolean switch set to TRUE if far UVC intervention parameterised in the workplace setting using `set_ucv()`; default = FALSE
#' * `far_uvc_workplace_coverage_type`:Type of coverage. Select "random" for random selecting workplaces for UVC interventions and "targeted" for targeting interventions at most populous workplaces
#' * `far_uvc_workplace_coverage`: Proportion of workplaces covered with far UVC (must be a numeric value between 0 and 1)
#' * `far_uvc_workplace_efficacy`: : Efficacy of far UVC in the workplace setting (must be a numeric value between 0 and 1)
#' * `far_uvc_workplace_timestep`: The timestep on which far UVC is implemented in the workplace setting (must be a numeric value greater than or equal to 0)
#' * `far_uvc_school`: boolean switch set to TRUE if far UVC intervention parameterised in the school setting using `set_ucv()`; default = FALSE
#' * `far_uvc_school_coverage_type`:Type of coverage. Select "random" for random selecting schools for UVC interventions and "targeted" for targeting interventions at most populous schools
#' * `far_uvc_school_coverage`: Proportion of schools covered with far UVC (must be a numeric value between 0 and 1)
#' * `far_uvc_school_efficacy`: : Efficacy of far UVC in the school setting (must be a numeric value between 0 and 1)
#' * `far_uvc_school_timestep`: The timestep on which far UVC is implemented in the school setting (must be a numeric value greater than or equal to 0)
#' * `far_uvc_leisure`: boolean switch set to TRUE if far UVC intervention parameterised in the leisure setting using `set_ucv()`; default = FALSE
#' * `far_uvc_leisure_coverage_type`:Type of coverage. Select "random" for random selecting leisure settings for UVC interventions and "targeted" for targeting interventions at most populous leisure settings
#' * `far_uvc_leisure_coverage`: Proportion of leisure settings covered with far UVC (must be a numeric value between 0 and 1)
#' * `far_uvc_leisure_efficacy`: : Efficacy of far UVC in the leisure setting (must be a numeric value between 0 and 1)
#' * `far_uvc_leisure_timestep`: The timestep on which far UVC is implemented in the leisure setting (must be a numeric value greater than or equal to 0)
#' * `far_uvc_household`: boolean switch set to TRUE if far UVC intervention parameterised in the household setting using `set_ucv()`; default = FALSE
#' * `far_uvc_household_coverage_type`:Type of coverage. Select "random" for random selecting households for UVC interventions and "targeted" for targeting interventions at most populous households
#' * `far_uvc_household_coverage`: Proportion of households covered with far UVC (must be a numeric value between 0 and 1)
#' * `far_uvc_household_efficacy`: : Efficacy of far UVC in the household setting (must be a numeric value between 0 and 1)
#' * `far_uvc_household_timestep`: The timestep on which far UVC is implemented in the household setting (must be a numeric value greater than or equal to 0)
#' @param archetype A text string indicating the pathogen archetype parameter set to load (default = "none", current options are flu, sars_cov_2, and measles)
#' @family parameters
#' @export
get_parameters <- function(overrides = list(), archetype = "none") {

  # Open a list of parameters to store
  parameters <- list(
    human_population = 10000,
    initial_proportion_child = 0.2,
    initial_proportion_adult = 0.6,
    initial_proportion_elderly = 0.2,
    number_initially_exposed = 5,
    seed = NULL,
    mean_household_size = 3,
    workplace_prop_max = 0.1,
    workplace_a = 5.36,
    workplace_c = 1.34,
    school_prop_max = 0.1,
    school_meanlog = 5.49,
    school_sdlog = 1.02,
    school_student_staff_ratio = 20,
    leisure_prob_visit = 0.6,
    leisure_mean_number_settings = 3,
    leisure_mean_size = 50,
    leisure_overdispersion_size = 2,
    leisure_prop_max = 0.1,

    duration_exposed = 2,
    duration_infectious = 4,
    beta_household = 0.5, # check this as default
    beta_workplace = 0.5, # check this as default
    beta_school = 0.5, # check this as default
    beta_leisure = 0.5, # check this as default
    beta_community = 0.2, # check this as default
    dt = 0.5, # check this as default
    simulation_time = 150,
    render_diagnostics = FALSE,
    household_distribution_generation = "empirical",
    school_distribution_generation = "empirical",
    endemic_or_epidemic = "epidemic",
    duration_immune = NULL,
    prob_inf_external = NULL,

    # Setting-Specific Riskiness Parameters: Workplace
    setting_specific_riskiness_workplace = FALSE,
    setting_specific_riskiness_workplace_meanlog = 0,
    setting_specific_riskiness_workplace_sdlog = 0.37,
    setting_specific_riskiness_workplace_min = 0.4472,
    setting_specific_riskiness_workplace_max = 2.236,

    # Setting-Specific Riskiness Parameters: School
    setting_specific_riskiness_school = FALSE,
    setting_specific_riskiness_school_meanlog = 0,
    setting_specific_riskiness_school_sdlog = 0.37,
    setting_specific_riskiness_school_min = 0.4472,
    setting_specific_riskiness_school_max = 2.236,

    # Setting-Specific Riskiness Parameters: Leisure
    setting_specific_riskiness_leisure = FALSE,
    setting_specific_riskiness_leisure_meanlog = 0,
    setting_specific_riskiness_leisure_sdlog = 0.37,
    setting_specific_riskiness_leisure_min = 0.4472,
    setting_specific_riskiness_leisure_max = 2.236,

    # Setting-Specific Riskiness Parameters: Household
    setting_specific_riskiness_household = FALSE,
    setting_specific_riskiness_household_meanlog = 0,
    setting_specific_riskiness_household_sdlog = 0.37,
    setting_specific_riskiness_household_min = 0.4472,
    setting_specific_riskiness_household_max = 2.236,

    # Far UVC Parameters: Workplace
    far_uvc_workplace = FALSE,
    far_uvc_workplace_coverage_type = NULL,
    far_uvc_workplace_coverage = NULL,
    far_uvc_workplace_efficacy = NULL,
    far_uvc_workplace_timestep = NULL,

    # Far UVC Parameters: School
    far_uvc_school = FALSE,
    far_uvc_school_coverage_type = NULL,
    far_uvc_school_coverage = NULL,
    far_uvc_school_efficacy = NULL,
    far_uvc_school_timestep = NULL,

    # Far UVC Parameters: Leisure:
    far_uvc_leisure = FALSE,
    far_uvc_leisure_coverage_type = NULL,
    far_uvc_leisure_coverage = NULL,
    far_uvc_leisure_efficacy = NULL,
    far_uvc_leisure_timestep = NULL,

    # Far UVC Parameters: Household:
    far_uvc_household = FALSE,
    far_uvc_household_coverage_type = NULL,
    far_uvc_household_coverage = NULL,
    far_uvc_household_efficacy = NULL,
    far_uvc_household_timestep = NULL

  )

  # Ensure overridden parameters are passed as a list
  if (!is.list(overrides)) {
    stop('overrides must be a list')
  }

  # Override parameter values in the overrides input
  for (name in names(overrides)) {
    if (!(name %in% names(parameters))) {
      stop(paste('unknown parameter', name, sep=' '))
    }
    parameters[[name]] <- overrides[[name]]
  }

  # Ensure archetype input from recognised options:
  if(!(archetype %in% c("none", "flu", "measles", "sars_cov_2"))) {
    stop('archetype not recognised')
  }

  # Check if dt is < 1 and whether it can evenly divide 1 (i.e. 1/x should return an integer)
  if (parameters$dt > 1 | parameters$dt == 0) {
    stop("dt must be less than 1 and greater than 0")
  }
  if ((1/parameters$dt) != floor(1/parameters$dt)) {
    stop("dt must evenly divide into 1 e.g. 0.1, 0.2, 0.25, 0.5")
  }

  # Check duration_immune is set if endemic_or_epidemic == "endemic"
  if (!(parameters$endemic_or_epidemic %in% c("endemic", "epidemic"))) {
    stop("endemic_or_epidemic must be set to either epidemic or endemic")
  }
  if (parameters$endemic_or_epidemic == "endemic" & is.null(parameters$duration_immune)) {
    stop("duration_immune must be specified if endemic_or_epidemic is set to endemic")
  }
  if (parameters$endemic_or_epidemic == "endemic" & is.null(parameters$prob_inf_external)) {
    stop("prob_inf_external must be specified if endemic_or_epidemic is set to endemic")
  }

  # Overwrite parameters if archetype specified:
  # Flu (R0 ~ 1.5)
  if(archetype == "flu") {
    parameters$duration_exposed = 1
    parameters$duration_infectious = 2
    parameters$beta_household = 0.132
    parameters$beta_workplace = 0.132
    parameters$beta_school = 0.132
    parameters$beta_leisure = 0.132
    parameters$beta_community = 0.044
  }

  # SARS-CoV-2 (R0 ~ 2.5)
  if(archetype == "sars_cov_2") {
    parameters$duration_exposed = 2
    parameters$duration_infectious = 4
    parameters$beta_household = 0.24
    parameters$beta_workplace = 0.24
    parameters$beta_school = 0.24
    parameters$beta_leisure = 0.24
    parameters$beta_community = 0.08
  }

  # Measles (R0 ~ 9)
  if(archetype == "measles") {
    parameters$duration_exposed = 8
    parameters$duration_infectious = 5
    parameters$beta_household = 1.26
    parameters$beta_workplace = 1.26
    parameters$beta_school = 1.26
    parameters$beta_leisure = 1.26
    parameters$beta_community = 0.42
  }

  # Check that all setting-specific betas are of length 1:
  if(any(length(parameters$beta_household) != 1,
         length(parameters$beta_school) != 1,
         length(parameters$beta_workplace) != 1,
         length(parameters$beta_leisure) != 1,
         length(parameters$beta_community) != 1)) {
    stop("ERROR: A setting-specific beta has length not equal to 1. All setting-specific betas must be of length 1")
  }

  ## ADD MORE CHECKS IN HERE FOR PARAMETERS ##

  # Return the list of parameters
  parameters

}

