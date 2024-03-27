#' Run the simulation
#'
#' @param parameters A list of the model parameters as generated using `get_parameters`
#' @param timesteps An integer of the number of timesteps, in days, to simulate
#'
#' @export
run_simulation <- function(parameters, timesteps) {

  # Generate the model variables:
  variables <- create_variables(parameters)

  # Generate the model events:
  events <- create_events(parameters)

  # Initialise the model events: (NOT SURE IF NEEDED YET)
  #initialise_events(events, variables, parameters)

  # Set up the model renderer:
  renderer <- individual::Render$new(timesteps)

  # Attached the listeners to the model events:
  attach_event_listeners(
    events,
    variables,
    parameters,
    correlations,
    renderer
  )

  # Generate the model processes:
  processes <- create_processes(variables, events, parameters)

  # Use individual::simulation_loop() to run the model for the specified number of timesteps
  individual::simulation_loop(
    processes = create_processes(
      renderer,
      variables,
      events,
      parameters),
    variables = variables,
    events = unlist(events),
    timesteps = timesteps
  )
  renderer$to_dataframe()
}

