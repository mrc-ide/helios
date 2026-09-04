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
#' * `number_initial_S`: number of humans initially Susceptible (state = S)
#' * `number_initial_E`: number of humans initially Exposed (state = E)
#' * `number_initial_I`: number of humans initially Infectious (state = I)
#' * `number_initial_R`: number of humans initially Recovered (state = R)
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
#' * `beta_household`: The transmission rate for household contacts, used to compute
#'   the per-timestep force of infection in household settings. A numeric scalar
#'   when `time_varying_transmission_on` is `FALSE`, or a numeric vector of length
#'   `simulation_time` (one value per simulated calendar day) when it is `TRUE`.
#' * `beta_workplace`: The transmission rate for workplace contacts, used to compute
#'   the per-timestep force of infection in workplace settings. A numeric scalar
#'   when `time_varying_transmission_on` is `FALSE`, or a numeric vector of length
#'   `simulation_time` (one value per simulated calendar day) when it is `TRUE`.
#' * `beta_school`: The transmission rate for school contacts, used to compute
#'   the per-timestep force of infection in school settings. A numeric scalar
#'   when `time_varying_transmission_on` is `FALSE`, or a numeric vector of length
#'   `simulation_time` (one value per simulated calendar day) when it is `TRUE`.
#' * `beta_leisure`: The transmission rate for leisure contacts, used to compute
#'   the per-timestep force of infection in leisure settings. A numeric scalar
#'   when `time_varying_transmission_on` is `FALSE`, or a numeric vector of length
#'   `simulation_time` (one value per simulated calendar day) when it is `TRUE`.
#' * `beta_community`: The transmission rate for community contacts, used to compute
#'   the per-timestep force of infection in community settings. A numeric scalar
#'   when `time_varying_transmission_on` is `FALSE`, or a numeric vector of length
#'   `simulation_time` (one value per simulated calendar day) when it is `TRUE`.
#' * `time_varying_transmission_on`: Logical flag (default `FALSE`). When `FALSE`,
#'   each setting-specific beta must be a numeric scalar. When `TRUE`, each
#'   setting-specific beta must instead be a numeric vector of length
#'   `simulation_time`, giving that setting's beta for each simulated calendar day.
#' * `dt`: TBD
#' * `simulation_time`: TBD
#' * `household_distribution_country`: TBD
#' * `school_distribution_country`: TBD
#' * `workplace_distribution_country`: TBD
#' * `endemic_or_epidemic`: TBD
#' * `duration_immune`: TBD
#' * `prob_inf_external`: TBD
#'
#' Rendering Parameters
#' * `render_diagnostics`: FALSE
#'
#' Setting-Specific ACH Parameters:
#' * `setting_specific_ach_workplace`: boolean switch set to TRUE if setting-specific ACH parameterised in the workplace setting using `set_setting_specific_ach()`; default = FALSE
#' * `setting_specific_ach_workplace_mean`: The mean of the  truncated normal distribution from which the setting-specific ACH of individual workplaces is drawn; default =4.8
#' * `setting_specific_ach_workplace_sd`: The standard deviation of the truncated normal distribution from which the setting-specific ACH of individual workplaces is drawn; default =1.5
#' * `setting_specific_ach_school`: boolean switch set to TRUE if setting-specific ACH parameterised in the school setting using `set_setting_specific_ach()`; default = FALSE
#' * `setting_specific_ach_school_mean`: The mean of the truncated normal distribution from which the setting-specific ACH of individual schools is drawn; default = 4.0
#' * `setting_specific_ach_school_sd`: The standard deviation of the truncated normal distribution from which the setting-specific ACH of individual schools is drawn; default = 1.2
#' * `setting_specific_ach_leisure`: boolean switch set to TRUE if setting-specific ACH parameterised in the leisure setting using `set_setting_specific_ach()`; default = FALSE
#' * `setting_specific_ach_leisure_mean`: The mean of the truncated normal distribution from which the setting-specific ACH of individual leisure locations is drawn; default = 3.0
#' * `setting_specific_ach_leisure_sd`: The standard deviation of the truncated normal distribution from which the setting-specific ACH of individual leisure settings is drawn; default = 1.0
#' * `setting_specific_ach_household`: boolean switch set to TRUE if setting-specific ACH parameterised in the household setting using `set_setting_specific_ach()`; default = FALSE
#' * `setting_specific_ach_household_mean`: The mean of the truncated normal distribution from which the setting-specific ACH of individual households is drawn; default = 0.5
#' * `setting_specific_ach_household_sd`: The standard deviation of the truncated normal distribution from which the setting-specific ACH of individual households is drawn; default = 0.2
#'
#' Default (Uniform) ACH Parameters (used when `setting_specific_ach_<setting>` is FALSE; if both are unset, `generate_setting_specific_ach()` errors):
#' * `default_ach_workplace`: uniform ACH assigned to every workplace location when `setting_specific_ach_workplace` is FALSE. Set via [set_default_ach()]. Default = NULL (must be set explicitly).
#' * `default_ach_school`: uniform ACH assigned to every school location when `setting_specific_ach_school` is FALSE. Default = NULL.
#' * `default_ach_leisure`: uniform ACH assigned to every leisure location when `setting_specific_ach_leisure` is FALSE. Default = NULL.
#' * `default_ach_household`: uniform ACH assigned to every household location when `setting_specific_ach_household` is FALSE. Default = NULL.
#'
#' Volume Per Person Parameters (used in the Wells-Riley calculation; units: m^3 per person):
#' * `volume_per_person_workplace`: average air volume per person in a workplace; default = 27 (assumes ~10 m^2 floor area at 2.7 m height)
#' * `volume_per_person_school`: average air volume per person in a school; default = 10 (assumes ~3.33 m^2 floor area at 3 m height)
#' * `volume_per_person_leisure`: average air volume per person in a leisure setting; default = 8 (assumes ~2 m^2 floor area at 4 m height)
#' * `volume_per_person_household`: average air volume per person in a household; default = 50 (assumes ~20 m^2 floor area at 2.5 m height)
#'
#' Wells-Riley Parameters (used by `convert_ach_to_riskiness()` to derive per-location riskiness from ACH, and by `calculate_efficacy_from_ach()` to derive per-location intervention efficacy from ACH and the intervention delta):
#' * `wells_riley_emission_rate`: rate at which an infectious individual emits airborne infectious units; units = FFU/hour; default = 27
#' * `wells_riley_decay_rate`: natural decay rate of airborne pathogens (denoted k_D); units = 1/hour; default = 0.64
#' * `wells_riley_infection_prob_per_ffu`: probability of infection per inhaled FFU (denoted r); default = 1.37e-2
#' * `wells_riley_respiratory_rate_factor`: respiratory rate multiplied by tidal volume (denoted RR_tv); units = m^3/hour; default = 0.45
#' * `wells_riley_time_in_room`: exposure window used inside the Wells-Riley calculation (denoted t); units = hours; default = 4
#'
#' Intervention Parameters (populated internally by `set_intervention_ach()` and by `generate_intervention_switches()`; users do not normally set these directly. One block per scope <s> in {joint, workplace, school, leisure, household}):
#' * `intervention_<s>_active`: boolean flag set to TRUE when an intervention has been installed in scope <s>. Default = FALSE
#' * `intervention_<s>_list`: list of intervention objects (each as returned by `make_intervention()`) deployed in scope <s>. Currently single-intervention only — list always has length 1 when active. Default = NULL
#' * `intervention_<s>_coverage`: fraction of total setting size to cover (numeric in `[0, 1]`); inherited from the intervention object's `coverage` field. Default = NULL
#' * `intervention_<s>_coverage_target`: what the coverage fraction applies to. Either "individuals" or "square_footage". Default = NULL
#' * `intervention_<s>_coverage_type`: how locations are selected for coverage. Either "random" (uniform sampling) or "targeted_riskiness" (locations ranked in decreasing order of riskiness). Default = NULL
#' * `intervention_<s>_timestep`: first simulation timestep at which the intervention's efficacy is applied in the FOI calculation. Default = NULL
#' * `intervention_<setting>_covered` (per-setting scopes only — workplace/school/leisure/household): 0/1 vector of length equal to the number of locations in the setting, populated by the dispatcher to mark which locations received the intervention. Default = NULL
#'
#' Setting-Specific Room Size Per Individual Parameters:
#' * `size_per_individual_workplace`: The volume or surface area for each individual in the workplace setting type; default = 1 (in which case "square_footage" coverage_target gives same results as "individuals" coverage_target)
#' * `size_per_individual_school`: The volume or surface area for each individual in the school setting type; default = 1 (in which case "square_footage" coverage_target gives same results as "individuals" coverage_target)
#' * `size_per_individual_leisure`: The volume or surface area for each individual in the leisure setting type; default = 1 (in which case "square_footage" coverage_target gives same results as "individuals" coverage_target)
#' * `size_per_individual_household`: The volume or surface area for each individual in the household setting type; default = 1 (in which case "square_footage" coverage_target gives same results as "individuals" coverage_target)
#'
#' Hospitalizations and Deaths
#' * `prob_hosp_child`: Probability that an infected child is hospitalized
#' * `prob_hosp_adult`: Probability that an infected adult is hospitalized
#' * `prob_hosp_elderly`: Probability that an infected elderly individual is hospitalized
#' * `prob_death_hosp_child`: Probability of death for a hospitalized child, conditional on hospitalization
#' * `prob_death_hosp_adult`: Probability of death for a hospitalized adult, conditional on hospitalization
#' * `prob_death_hosp_elderly`: Probability of death for a hospitalized elderly individual, conditional on hospitalization
#' * `duration_hospitalized`: Average duration (in days) of a hospitalization
#'
#'
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
    number_initial_S = 9995,
    number_initial_E = 5,
    number_initial_I = 0,
    number_initial_R = 0,
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

    time_varying_transmission_on = FALSE,
    dt = 0.5, # check this as default
    simulation_time = 150,
    render_diagnostics = FALSE,
    household_distribution_country = "USA",
    school_distribution_country = "USA",
    workplace_distribution_country = "USA",
    endemic_or_epidemic = "epidemic",
    duration_immune = NULL,
    prob_inf_external = NULL,

    # Setting-Specific ACH Parameters: Workplace
    setting_specific_ach_workplace = FALSE,
    setting_specific_ach_workplace_mean = NULL,
    setting_specific_ach_workplace_sd = NULL,


    # Setting-Specific ACH Parameters: School
    setting_specific_ach_school = FALSE,
    setting_specific_ach_school_mean = NULL,
    setting_specific_ach_school_sd = NULL,

    # Setting-Specific ACH Parameters: Leisure
    setting_specific_ach_leisure = FALSE,
    setting_specific_ach_leisure_mean = NULL,
    setting_specific_ach_leisure_sd = NULL,

    # Setting-Specific ACH Parameters: Household
    setting_specific_ach_household = FALSE,
    setting_specific_ach_household_mean = NULL,
    setting_specific_ach_household_sd = NULL,

    # Default (uniform) ACH per setting, used when setting_specific_ach_<setting>
    # is FALSE. NULL means "not configured" — generate_setting_specific_ach()
    # will error if both the switch is FALSE and the default is NULL.
    default_ach_workplace = NULL,
    default_ach_school    = NULL,
    default_ach_leisure   = NULL,
    default_ach_household = NULL,

    # Volume per person parameters (density values, m^3, room heights are assumptions)
    volume_per_person_workplace = 27, #10m^2 *2.7m
    volume_per_person_school = 10, # 3.33m^2 *3m
    volume_per_person_leisure = 8, # 2m^2*4M
    volume_per_person_household = 50, # 20m^2 * 2.5m

    # W-R parameters
    wells_riley_emission_rate = 27, # FFU/hour
    wells_riley_decay_rate = 0.64, # k_D, natural decay rate (1/hr)
    wells_riley_infection_prob_per_ffu = 1.37e-2, # r, infection probability per FFU
    wells_riley_respiratory_rate_factor = 0.45, # RR_tv
    wells_riley_time_in_room = 4,# t, hours spent in room


  # Intervention parameters (Wells-Riley ACH-based efficacy):
    intervention_joint_active              = FALSE,
    intervention_joint_list                = NULL,
    intervention_joint_coverage            = NULL,
    intervention_joint_coverage_target     = NULL,
    intervention_joint_coverage_type       = NULL,
    intervention_joint_timestep            = NULL,

    intervention_workplace_active          = FALSE,
    intervention_workplace_list            = NULL,
    intervention_workplace_coverage        = NULL,
    intervention_workplace_coverage_target = NULL,
    intervention_workplace_coverage_type   = NULL,
    intervention_workplace_timestep        = NULL,
    intervention_workplace_covered         = NULL,

    intervention_school_active             = FALSE,
    intervention_school_list               = NULL,
    intervention_school_coverage           = NULL,
    intervention_school_coverage_target    = NULL,
    intervention_school_coverage_type      = NULL,
    intervention_school_timestep           = NULL,
    intervention_school_covered            = NULL,

    intervention_leisure_active            = FALSE,
    intervention_leisure_list              = NULL,
    intervention_leisure_coverage          = NULL,
    intervention_leisure_coverage_target   = NULL,
    intervention_leisure_coverage_type     = NULL,
    intervention_leisure_timestep          = NULL,
    intervention_leisure_covered           = NULL,

    intervention_household_active          = FALSE,
    intervention_household_list            = NULL,
    intervention_household_coverage        = NULL,
    intervention_household_coverage_target = NULL,
    intervention_household_coverage_type   = NULL,
    intervention_household_timestep        = NULL,
    intervention_household_covered         = NULL,

    # Room Size Per Individual Parameters: (currently used for coverage allocation)
    size_per_individual_workplace = 1,
    size_per_individual_school = 1,
    size_per_individual_leisure = 1,
    size_per_individual_household = 1,

    # Hospitalization & Death Parameters
    prob_hosp_child = 0.001,
    prob_hosp_adult = 0.03,
    prob_hosp_elderly = 0.18,
    prob_death_hosp_child = 0.01,
    prob_death_hosp_adult = 0.08,
    prob_death_hosp_elderly = 0.3,
    duration_hospitalized = 10
  )

  # Overwrite parameters if archetype specified. This runs before the
  # overrides loop below so that explicit overrides always take final
  # precedence over archetype defaults.
  # Flu (R0 ~ 1.5)
  if (archetype == "flu") {
    parameters$duration_exposed = 1
    parameters$duration_infectious = 2
    parameters$beta_household = 0.207
    parameters$beta_workplace = 0.207
    parameters$beta_school = 0.207
    parameters$beta_leisure = 0.207
    parameters$beta_community = 0.069
    parameters$prob_hosp_child = 0.001
    parameters$prob_hosp_adult = 0.03
    parameters$prob_hosp_elderly = 0.18
    parameters$prob_death_hosp_child = 0.01
    parameters$prob_death_hosp_adult = 0.08
    parameters$prob_death_hosp_elderly = 0.3
    parameters$duration_hospitalized = 5
  }

  # SARS-CoV-2 (R0 ~ 2.5)
  if (archetype == "sars_cov_2") {
    parameters$duration_exposed = 2
    parameters$duration_infectious = 4
    parameters$beta_household = 0.24
    parameters$beta_workplace = 0.24
    parameters$beta_school = 0.24
    parameters$beta_leisure = 0.24
    parameters$beta_community = 0.08
    parameters$prob_hosp_child = 0.001
    parameters$prob_hosp_adult = 0.03
    parameters$prob_hosp_elderly = 0.18
    parameters$prob_death_hosp_child = 0.01
    parameters$prob_death_hosp_adult = 0.08
    parameters$prob_death_hosp_elderly = 0.3
    parameters$duration_hospitalized = 10
  }

  # Measles (R0 ~ 9)
  if (archetype == "measles") {
    parameters$duration_exposed = 8
    parameters$duration_infectious = 5
    parameters$beta_household = 1.26
    parameters$beta_workplace = 1.26
    parameters$beta_school = 1.26
    parameters$beta_leisure = 1.26
    parameters$beta_community = 0.42
    parameters$prob_hosp_child = 0.001
    parameters$prob_hosp_adult = 0.03
    parameters$prob_hosp_elderly = 0.18
    parameters$prob_death_hosp_child = 0.01
    parameters$prob_death_hosp_adult = 0.08
    parameters$prob_death_hosp_elderly = 0.3
    parameters$duration_hospitalized = 7
  }

  # Ensure overridden parameters are passed as a list
  if (!is.list(overrides)) {
    stop('overrides must be a list')
  }

  # Override parameter values in the overrides input
  for (name in names(overrides)) {
    if (!(name %in% names(parameters))) {
      stop(paste('unknown parameter', name, sep = ' '))
    }
    parameters[[name]] <- overrides[[name]]
  }

  # Ensure size_per_individual parameters are greater than or equal to 1
  if (
    parameters$size_per_individual_workplace < 1 |
      parameters$size_per_individual_school < 1 |
      parameters$size_per_individual_leisure < 1 |
      parameters$size_per_individual_household < 1
  ) {
    stop(
      "all size_per_individual parameters must be equal to or greater than 1"
    )
  }

  # Ensure archetype input from recognised options:
  if (!(archetype %in% c("none", "flu", "measles", "sars_cov_2"))) {
    stop('archetype not recognised')
  }

  # Check if dt is < 1 and whether it can evenly divide 1 (i.e. 1/x should return an integer)
  if (parameters$dt > 1 | parameters$dt == 0) {
    stop("dt must be less than 1 and greater than 0")
  }
  if ((1 / parameters$dt) != floor(1 / parameters$dt)) {
    stop("dt must evenly divide into 1 e.g. 0.1, 0.2, 0.25, 0.5")
  }



  # Check duration_immune is set if endemic_or_epidemic == "endemic"
  if (!(parameters$endemic_or_epidemic %in% c("endemic", "epidemic"))) {
    stop("endemic_or_epidemic must be set to either epidemic or endemic")
  }
  if (
    parameters$endemic_or_epidemic == "endemic" &
      is.null(parameters$duration_immune)
  ) {
    stop(
      "duration_immune must be specified if endemic_or_epidemic is set to endemic"
    )
  }
  if (
    parameters$endemic_or_epidemic == "endemic" &
      is.null(parameters$prob_inf_external)
  ) {
    stop(
      "prob_inf_external must be specified if endemic_or_epidemic is set to endemic"
    )
  }

  # Checking distribution country is either UK, USA or custom
  if (!(parameters$household_distribution_country %in% c("UK", "USA", "custom"))) {
    stop(
      "household_distribution_country must be set to either UK, USA or custom"
    )
  }
  if (!(parameters$workplace_distribution_country %in% c("UK", "USA", "custom"))) {
    stop(
      "workplace_distribution_country must be set to either UK, USA or custom"
    )
  }
  if (!(parameters$school_distribution_country %in% c("UK", "USA", "custom"))) {
    stop("school_distribution_country must be set to either UK, USA or custom")
  }

  # Check that all setting-specific betas are of the correct length and type:
  # a single constant value when time-varying transmission is off, or a numeric vector of
  # length simulation_time (one value per simulated day, with no NAs) when
  # time-varying transmission is on
  if (isTRUE(parameters$time_varying_transmission_on)) {
    if (
      any(
        !is.numeric(parameters$beta_household) | length(parameters$beta_household) != parameters$simulation_time | anyNA(parameters$beta_household),
        !is.numeric(parameters$beta_school) | length(parameters$beta_school) != parameters$simulation_time | anyNA(parameters$beta_school),
        !is.numeric(parameters$beta_workplace) | length(parameters$beta_workplace) != parameters$simulation_time | anyNA(parameters$beta_workplace),
        !is.numeric(parameters$beta_leisure) | length(parameters$beta_leisure) != parameters$simulation_time | anyNA(parameters$beta_leisure),
        !is.numeric(parameters$beta_community) | length(parameters$beta_community) != parameters$simulation_time | anyNA(parameters$beta_community)
      )
    ) {
      stop(
        "ERROR: when time_varying_transmission_on is TRUE, all setting-specific betas must be numeric vectors of length equal to simulation_time, with no NAs"
      )
    }
  } else {
    if (
      any(
        length(parameters$beta_household) != 1,
        length(parameters$beta_school) != 1,
        length(parameters$beta_workplace) != 1,
        length(parameters$beta_leisure) != 1,
        length(parameters$beta_community) != 1
      )
    ) {
      stop(
        "ERROR: A setting-specific beta has length not equal to 1. All setting-specific betas must be of length 1"
      )
    }
  }

  # Check that initial numbers in each state sum to the human population size
  number_initial <- parameters$number_initial_S +
    parameters$number_initial_E +
    parameters$number_initial_I +
    parameters$number_initial_R
  if (number_initial != parameters$human_population) {
    stop(
      "total of number_initial_S, number_initial_E, number_initial_I and number_initial_R should sum to human_population"
    )
  }

  ## ADD MORE CHECKS IN HERE FOR PARAMETERS ##

  # Return the list of parameters
  parameters
}
