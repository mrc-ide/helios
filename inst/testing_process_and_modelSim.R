# Loading required libraries
library(individual)

## Create parameters
source("R/parameters.R")
parameters_list <- get_parameters()
parameters_list$seed <- 10
parameters_list$beta_household <- 1.0
parameters_list$human_population <- 10000

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
                                   parameters_list = parameters_list,
                                   renderer = renderer)

# Use individual::simulation_loop() to run the model for the specified number of timesteps
# profvis::profvis({
#   individual::simulation_loop(
#     variables = variables_list,
#     events = events_list,
#     processes = processes_list,
#     timesteps = timesteps
#   )
# })

library(tictoc)
tic()
individual::simulation_loop(
  variables = variables_list,
  events = events_list,
  processes = processes_list,
  timesteps = timesteps
)
toc()

states <- renderer$to_dataframe()
health_cols <-  c("royalblue3","firebrick3","darkorchid3", "orange2")
matplot(
  x = states[[1]]*parameters_list$dt, y = states[-1],
  type="l",lwd=2,lty = 1,col = adjustcolor(col = health_cols, alpha.f = 0.85),
  xlab = "Time",ylab = "Count"
)
health_states <- colnames(states)[-1]
legend(
  x = "topright",pch = rep(16,3),
  col = health_cols,bg = "transparent",
  legend = health_states, cex = 1.5
)

source("R/model.R")
x <- run_simulation(parameters_list, 10)
health_cols <-  c("royalblue3","firebrick3","darkorchid3", "orange2")
matplot(
  x = x[[1]]*parameters_list$dt, y = x[-1],
  type="l",lwd=2,lty = 1,col = adjustcolor(col = health_cols, alpha.f = 0.85),
  xlab = "Time",ylab = "Count"
)
health_states <- colnames(x)[-1]
legend(
  x = "topright",pch = rep(16,3),
  col = health_cols,bg = "transparent",
  legend = health_states, cex = 1.5
)
