#' Run the simulation
#'
#' @param parameters A list of the model parameters as generated using `get_parameters`
#' @param timesteps An integer representing the number of timesteps to simulate
#'
#' @export
run_simulation <- function(parameters, timesteps) {

  # Generate the model variables:
  variables <- create_variables(parameters)

  # Generate the model events:
  events <- create_events(parameters, variables)

  # Initialise the model events: (Not currently needed, but may be when we add interventions)
  #initialise_events(events, variables, parameters)

  # Set up the model renderer:
  timesteps <- round(parameters$simulation_time/parameters$dt)
  renderer <- individual::Render$new(timesteps)

  # Generate the model processes:
  processes <- create_processes(variables, events, parameters)

  # Use individual::simulation_loop() to run the model for the specified number of timesteps
  individual::simulation_loop(
    processes = processes,
    variables = variables,
    events = unlist(events),
    timesteps = timesteps
  )
  renderer$to_dataframe()
}
