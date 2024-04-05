# Loading required libraries
library(individual)

## Create parameters
source("R/parameters.R")
params <- get_parameters()
params$seed <- 10

## Generate the model variables:
source("R/variables.R")
source("R/sampling.R")
variables <- create_variables(params)

## Generate the model events:
source("R/events.R")
events <- create_events(params, variables)

# Set up the model renderer:
timesteps <- round(params$simulation_time/params$dt)
renderer <- individual::Render$new(timesteps)

# Generate the model processes:
source("R/processes.R")
processes <- create_processes(variables, events, params)

# Use individual::simulation_loop() to run the model for the specified number of timesteps
individual::simulation_loop(
  variables = variables,
  events = unlist(events),
  processes = create_processes(variables, events, params),
  timesteps = timesteps
)

source("R/model.R")
run_simulation(params, 10)
#
# x <- SE_process(parameters = params, variables = variables)
# x(7)
#
# for (t in seq_len(timesteps)) {
#   for (process in processes) {
#     individual:::execute_any_process(processes[[1]], 1)
#   }
#   for (event in events) {
#     event$.process()
#   }
#   for (variable in variables) {
#     variable$.update()
#   }
#   for (event in events) {
#     event$.tick()
#   }
# }
#
#
