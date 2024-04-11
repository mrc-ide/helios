#' Run the simulation
#'
#' @param parameters_list A list of the model parameters as generated using `get_parameters`
#' @param timesteps An integer representing the number of timesteps to simulate
#'
#' @export
run_simulation <- function(parameters_list, timesteps) {

  # Generate the model variables:
  variables_list <- create_variables(parameters_list)

  # Generate the model events:
  events_list <- create_events(variables_list = variables_list, parameters_list = parameters_list)

  # Set up the model renderer:
  timesteps <- round(parameters_list$simulation_time/parameters_list$dt)
  renderer <- individual::Render$new(timesteps)

  # Generate the model processes:
  processes_list <- create_processes(variables_list = variables_list,
                                     events_list = events_list,
                                     parameters_list = parameters_list,
                                     renderer = renderer)

  # Use individual::simulation_loop() to run the model for the specified number of timesteps
  individual::simulation_loop(
    variables = variables_list,
    events = unlist(events_list),
    processes = processes_list,
    timesteps = timesteps
  )
  renderer$to_dataframe()
}
