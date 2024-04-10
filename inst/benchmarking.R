# Loading required libraries
library(individual); library(tictoc)

## Create parameters
source("R/parameters.R")
source("R/processes.R")
source("R/variables.R")
source("R/sampling.R")
source("R/events.R")
source("R/model.R")

## Running the model
parameters_list <- get_parameters()
# parameters_list$seed <- 10
# parameters_list$beta_household <- 1.0
# parameters_list$human_population <- 100000
timesteps <- round(parameters_list$simulation_time/parameters_list$dt)
tic()
run_simulation(parameters_list = parameters_list, timesteps = timesteps)
toc()

variables_list <- create_variables(parameters_list)
events_list <- create_events(variables_list = variables_list, parameters_list = parameters_list)

variables_list$household$get_index_of("10")
