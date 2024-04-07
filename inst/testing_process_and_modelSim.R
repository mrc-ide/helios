# Loading required libraries
library(individual)

## Create parameters
source("R/parameters.R")
parameters_list <- get_parameters()
parameters_list$seed <- 10
parameters_list$beta_household <- 1.0

## Generate the model variables:
source("R/variables.R")
source("R/sampling.R")
variables_list <- create_variables(parameters_list)

## Generate the model events:
source("R/events.R")
events_list <- create_events(variables_list = variables_list, parameters_list = parameters_list)

# Set up the model renderer:
timesteps <- round(parameters_list$simulation_time/parameters_list$dt)
renderer <- individual::Render$new(timesteps)

# Generate the model processes:
source("R/processes.R")
processes_list <- create_processes(variables_list = variables_list,
                                   events_list = events_list,
                                   parameters_list = parameters_list)

# Use individual::simulation_loop() to run the model for the specified number of timesteps
individual::simulation_loop(
  variables = variables_list,
  events = events_list,
  processes = processes_list,
  timesteps = timesteps
)

source("R/model.R")
run_simulation(parameters_list, 10)

