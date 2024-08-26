#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+++++ Helios hipercow development script +++#
#++++++++++++++++++++++++++++++++++++++++++++#

# Load packages:
library(hipercow)
library(orderly2)
library(tidyverse)
library(stringr)
library(sf)
library(patchwork)

# In the parameter lists, we have the following we can use to ID the simulations:
# parameter_lists[[1]]$ID
# parameter_lists[[1]]$iteration
# parameter_lists[[1]]$pathogen
# parameter_lists[[1]]$far_uvc_joint_coverage_type
# parameter_lists[[1]]$far_uvc_joint_coverage
# parameter_lists[[1]]$far_uvc_joint_efficacy
# parameter_lists[[1]]$endemic_or_epidemic

#----- 1) Cluster set up ---------------------------------------------------------------------------

# Set working directtory:
setwd("./inst/blueprint_output_3_Sep9/")

## Prepare for cluster use
## see https://mrc-ide.github.io/hipercow/
hipercow::hipercow_init(driver = 'windows')

# User-authentication process:
hipercow::windows_authenticate()

## Provision packages required on the cluster (hipercow looks for provision.R by default)
## see https://mrc-ide.github.io/hipercow/articles/packages.html
hipercow::hipercow_provision()

# Check the configuration:
hipercow::hipercow_configuration()

#----- 2) Run helios in parallel on Desktop --------------------------------------------------------

# Read in the parameter lists:
parameter_lists <- readRDS("./Report_3_Endemic/endemic_simulations_parameter_lists.rds")

# Reduce the number of simulation time steps:
for(i in 1:length(parameter_lists)) {
  parameter_lists[[i]]$simulation_time <- 10
}

# Get the number of available cores:
cores <- parallel::detectCores()

# Prepare the cluster given the number of cores detected:
cluster <- parallel::makeCluster(as.integer(cores))

# Set the library path:
invisible(parallel::clusterCall(cluster, ".libPaths", .libPaths()))

# Load the requisite functions
parallel::clusterCall(cluster, function() {
  library(helios)
  library(dplyr)
  library(individual)
  library(helios)
  library(tidyverse)
  library(dqrng)
  library(EnvStats)
  TRUE
})

# Run the simulations in parallel:
test <- parallel::parLapply(cl = cluster, X = parameter_lists, fun = helios::run_simulation)

#----- 3) Run simple helios jobs on hipercow -------------------------------------------------------

# Create the environment for hipercow
hipercow_environment_create(packages = c("individual", "helios", "tidyverse", "dqrng", "EnvStats"))

tictoc::tic()
parameters <- helios::get_parameters()
test1 <- helios::run_simulation(parameters_list = parameters)
test2 <- helios::run_simulation(parameters_list = parameters)
tictoc::toc()

# Create the task (2 simulations of the basic run_simulation() function:
id5 <- hipercow::task_create_explicit(

  # Everything in the expr will be run on the cluster:
  expr = quote({
    parameters <- helios::get_parameters()
    test <- list()
    for(i in 1:2) {
      test[[i]] <- helios::run_simulation(parameters_list = parameters)
      test[[i]]$ID <- i
    }
    saveRDS(test, "test.rds")

  }),

  # Specify the cluster resources etc.
  resources = hipercow_resources(cores = 2),
  parallel = hipercow_parallel("parallel")
)

# Check the task progress:
task_status(id5)
task_result(id5)

# Load and view the output:
test <- readRDS("./test.rds")
test[[2]]

#----- 4) Run parallelised helios jobs on hipercow -------------------------------------------------

# Create the environment for hipercow
hipercow_environment_create(packages = c("individual", "helios", "tidyverse", "dqrng", "EnvStats"))

parameters <- readRDS("./Report_3_Endemic/endemic_simulations_parameter_lists.rds")
for(i in 1:length(parameters)) {
  parameters[[i]]$simulation_time <- 20
}
length(parameters)

# Create the task (2 simulations of the basic run_simulation() function:
id5 <- hipercow::task_create_explicit(

  # Everything in the expr will be run on the cluster:
  expr = quote({

    parameters <- readRDS("./Report_3_Endemic/endemic_simulations_parameter_lists.rds")
    for(i in 1:length(parameters)) {
      parameters[[i]]$simulation_time <- 10
    }

    scenario_output <- parallel::parLapply(
      cl = NULL,
      X = parameters,
      fun = run_simulation
    )

    saveRDS(object = scenario_output, file = "./Report_3_Endemic/scenario_output.rds")

  }),

  # Specify the cluster resources etc.
  resources = hipercow_resources(cores = 20),
  parallel = hipercow_parallel("parallel")
)

# Check the task progress:
task_status(id5)
task_result(id5)

# Load and view the output:
test <- readRDS("./scenario_output.rds")
test[[40]]

#----- 5) Run parallelised helios jobs vis hipercow script -----------------------------------------

# Create the environment for hipercow
hipercow_environment_create(packages = c("individual", "helios", "tidyverse", "dqrng", "EnvStats"))

parameters <- readRDS("./Report_3_Endemic/endemic_simulations_parameter_lists.rds")
for(i in 1:length(parameters)) {
  parameters[[i]]$simulation_time <- 10
}
length(parameters)

# Create the task (2 simulations of the basic run_simulation() function:
id5 <- hipercow::task_create_explicit(

  # Everything in the expr will be run on the cluster:
  expr = quote({

    # Load in the parameter lists:
    parameters <- readRDS("./Report_3_Endemic/endemic_simulations_parameter_lists.rds")
    for(i in 1:length(parameters)) {
      parameters[[i]]$simulation_time <- 10
    }

    # Load in the run_simulations_hipercow() function:
    source("./Report_3_Endemic/run_simulations_hipercow.R")

    # Run through the parameter lists and run the hipercow helios script:
    scenario_output <- parallel::parLapply(
      cl = NULL,
      X = parameters,
      fun = run_simulation_hipercow
    )

    saveRDS(object = scenario_output, file = "./Report_3_Endemic/scenario_output.rds")

  }),

  # Specify the cluster resources etc.
  resources = hipercow_resources(cores = 32),
  parallel = hipercow_parallel("parallel")
)

# Check the task progress:
task_status(id5)
task_result(id5)

# Load and view the output:
test <- readRDS("./Report_3_Endemic/scenario_output.rds")


length(test)
test[[40]][[1]]
test[[40]][[2]]

test2 <- list()
for(i in 1:length(test)) {
  test2[[i]] <- test[[i]][[2]]
}

test3 <- bind_rows(test2)
test3 |>
  pivot_longer(cols = c(S_count, E_count, I_count, R_count), names_to = "State", values_to = "Individuals") |>
  ggplot(aes(x = timestep, y = Individuals, colour = as.factor(ID))) + geom_line() +
  theme_bw() +
  facet_grid(archetype~State)










