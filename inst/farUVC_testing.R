# Loading required libraries
library(individual); library(tictoc)

## Sourcing required functions
source("R/parameters.R")
source("R/events.R")
source("R/processes.R")
source("R/variables.R")
source("R/sampling.R")
source("R/model.R")
source("R/utils.R")

## Running the model with no farUVC
nofarUVC_parameters_list <- get_parameters(overrides = list(beta_household = 0.1,
                                                            beta_workplace = 0.1,
                                                            beta_school = 0.1,
                                                            beta_leisure = 0.1,
                                                            beta_community = 0.05,
                                                            simulation_time = 400,
                                                            number_initially_exposed = 50))
nofarUVC_output <- run_simulation(nofarUVC_parameters_list)
health_cols <-  c("royalblue3","firebrick3","darkorchid3", "orange2")
matplot(
  x = nofarUVC_output[[1]]*nofarUVC_parameters_list$dt, y = nofarUVC_output[-1],
  type="l",lwd=2,lty = 1,col = adjustcolor(col = health_cols, alpha.f = 0.85),
  xlab = "Time",ylab = "Count"
)
# betas = 0.1 ~ AR = 38% (finished by 200)
# betas = 0.2 ~ R0 =

## Running the model with farUVC
farUVC_parameters_list <- get_parameters(overrides = list(beta_household = 0.1,
                                                          beta_workplace = 0.1,
                                                          beta_school = 0.1,
                                                          beta_leisure = 0.1,
                                                          beta_community = 0.05,
                                                          simulation_time = 400,
                                                          far_uvc_workplace = TRUE,
                                                          far_uvc_workplace_coverage_type = "random",
                                                          far_uvc_workplace_coverage = 0.25,
                                                          far_uvc_workplace_efficacy = 0.6,
                                                          far_uvc_workplace_timestep = 0,
                                                          far_uvc_school = TRUE,
                                                          far_uvc_school_coverage_type = "random",
                                                          far_uvc_school_coverage = 0.25,
                                                          far_uvc_school_efficacy = 0.6,
                                                          far_uvc_school_timestep = 0,
                                                          far_uvc_leisure = TRUE,
                                                          far_uvc_leisure_coverage_type = "random",
                                                          far_uvc_leisure_coverage = 0.25,
                                                          far_uvc_leisure_efficacy = 0.6,
                                                          far_uvc_leisure_timestep = 0,
                                                          far_uvc_household = TRUE,
                                                          far_uvc_household_coverage_type = "random",
                                                          far_uvc_household_coverage = 0.25,
                                                          far_uvc_household_efficacy = 0.6,
                                                          far_uvc_household_timestep = 0))
farUVC_output <- run_simulation(farUVC_parameters_list)

par(mfrow = c(1, 2))
health_cols <-  c("royalblue3","firebrick3","darkorchid3", "orange2")
matplot(
  x = nofarUVC_output[[1]]*nofarUVC_parameters_list$dt, y = nofarUVC_output[-c(1, 2, 5)],
  type="l",lwd=2,lty = 1,col = adjustcolor(col = health_cols, alpha.f = 0.85),
  xlab = "Time",ylab = "Count"
)
matplot(
  x = farUVC_output[[1]]*farUVC_parameters_list$dt, y = farUVC_output[-c(1, 2, 5)],
  type="l",lwd=2,lty = 1,col = adjustcolor(col = health_cols, alpha.f = 0.85),
  xlab = "Time",ylab = "Count"
)

plot(nofarUVC_output[[1]]*nofarUVC_parameters_list$dt, nofarUVC_output[, 3], type = "l")
lines(farUVC_output[[1]]*farUVC_parameters_list$dt, farUVC_output[, 3], col = "red")

par(mfrow = c(1, 1))
plot(nofarUVC_output[[1]]*nofarUVC_parameters_list$dt, nofarUVC_output[, 5], type = "l", xlab = "Time (Days)", ylab = "Total Infected")
lines(farUVC_output[[1]]*farUVC_parameters_list$dt, farUVC_output[, 5], col = "red")


parameters_list$seed <- 10
parameters_list$beta_household <- 1.0
parameters_list$beta_workplace <- 1.0
parameters_list$beta_school <- 1.0
parameters_list$beta_community <- 1.0
parameters_list$beta_leisure <- 1.0
parameters_list$human_population <- 10000
parameters_list$simulation_time <- 50

## Generate the model variables:

variables_list <- create_variables(parameters_list)

## Generate the model events:
events_list <- create_events(variables_list = variables_list, parameters_list = parameters_list)

# Set up the model renderer:
timesteps <- round(parameters_list$simulation_time/parameters_list$dt)
renderer <- individual::Render$new(timesteps)

# Generate the model processes:
processes_list <- create_processes(variables_list = variables_list,
                                   events_list = events_list,
                                   parameters_list = parameters_list,
                                   renderer = renderer)
tic()
individual::simulation_loop(
  variables = variables_list,
  events = events_list,
  processes = processes_list,
  timesteps = 100
)
toc()

states <- renderer$to_dataframe()
health_cols <-  c("royalblue3","firebrick3","darkorchid3", "orange2")
matplot(
  x = states[[1]]*parameters_list$dt, y = states[-1],
  type="l",lwd=2,lty = 1,col = adjustcolor(col = health_cols, alpha.f = 0.85),
  xlab = "Time",ylab = "Count"
)


## Running the model
par(mfrow = c(1, 2))
source("R/model.R")
epidemic_parameters_list <- get_parameters(overrides = list(endemic_or_epidemic = "epidemic",
                                                            beta_household = 1,
                                                            beta_workplace = 1,
                                                            beta_school = 1,
                                                            beta_community = 1,
                                                            beta_leisure = 1))
x <- run_simulation(epidemic_parameters_list, 10)
health_cols <-  c("royalblue3","firebrick3","darkorchid3", "orange2")
matplot(
  x = x[[1]]*epidemic_parameters_list$dt, y = x[-1],
  type="l",lwd=2,lty = 1,col = adjustcolor(col = health_cols, alpha.f = 0.85),
  xlab = "Time",ylab = "Count"
)

endemic_parameters_list <- get_parameters(overrides = list(endemic_or_epidemic = "endemic",
                                                           beta_household = 1,
                                                           beta_workplace = 1,
                                                           beta_school = 1,
                                                           beta_community = 1,
                                                           beta_leisure = 1,
                                                           duration_immune = 10))
y <- run_simulation(endemic_parameters_list, 10)
health_cols <-  c("royalblue3","firebrick3","darkorchid3", "orange2")
matplot(
  x = y[[1]]*endemic_parameters_list$dt, y = y[-1],
  type="l",lwd=2,lty = 1,col = adjustcolor(col = health_cols, alpha.f = 0.85),
  xlab = "Time",ylab = "Count"
)
