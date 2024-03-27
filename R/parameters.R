#' Establish the list of model parameters
#'
#' @description
#' The get_parameters() function creates a named list of model parameters.
#'
#'
#' @param overrides a named list of parameters values to be used instead of the defaults. The
#' parameters are defined below.
#'
#' Human Population Parameters:
#'
#' * human_population - the number of humans to model; default = 10000
#' * initial_proportion_child - proportion of population initially in the 'child' age class; default = 0.2
#' * initial_proportion_adult - proportion of population initially in the 'adult' age class; default = 0.6
#' * initial_proportion_elderly - proportion of population initially in the 'elderly' age class; default = 0.2
#' * number_initially_exposed - number of humans initially exposed (state = E); default = 0.0005
#' * seed - a seed to run the simulation with ;default = NULL
#' * workplace_prop_max - maximum size of a workplace as a proportion of total adult population size; default = 0.1
#' * workplace_a - the a parameter for the Zipf-like distribution on workplace size; default = 5.36
#' * workplace_c - the c parameter for the Zipf-like distribution on workplace size; default = 1.34
#' * school_meanlog - the meanlog parameter for the log-normal distribution on school size; default = 5.49
#' * school_sdlog - the sdlog parameter for the log-normal distribution on school size; default = 1.02
#'
#' @export
get_parameters <- function(overrides = list()) {

  # Open a list of parameters to store:
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
    school_meanlog = 5.49,
    school_sdlog = 1.02
  )

  # Ensure overridden parameters are passed as a list:
  if (!is.list(overrides)) {
    stop('overrides must be a list')
  }

  # Override parameter values in the overrides input:
  for (name in names(overrides)) {
    if (!(name %in% names(parameters))) {
      stop(paste('unknown parameter', name, sep=' '))
    }
    parameters[[name]] <- overrides[[name]]
  }

  # Return the list of parameters
  parameters

}
