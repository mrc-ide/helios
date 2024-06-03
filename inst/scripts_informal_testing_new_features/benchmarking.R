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
x <- run_simulation(parameters_list = parameters_list, timesteps = timesteps)
toc()

health_cols <-  c("royalblue3","firebrick3","darkorchid3", "orange2")
matplot(
  x = x[[1]]*parameters_list$dt, y = x[-1],
  type="l",lwd=2,lty = 1,col = adjustcolor(col = health_cols, alpha.f = 0.85),
  xlab = "Time",ylab = "Count"
)
