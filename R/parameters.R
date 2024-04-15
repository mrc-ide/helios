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
#' * `beta_household`: TBD
#' * `beta_workplace`: TBD
#' * `beta_school`: TBD
#' * `beta_leisure`: TBD
#' * `beta_community`: TBD
#' * `dt`: TBD
#' * `simulation_time`: TBD
#'
#' @family parameters
#' @export
get_parameters <- function(overrides = list()) {

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
    household_distribution_generation = "empirical",
    school_distribution_generation = "empirical",
    endemic_or_epidemic = "epidemic",
    duration_immune = NULL
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

  # Return the list of parameters
  parameters

}
