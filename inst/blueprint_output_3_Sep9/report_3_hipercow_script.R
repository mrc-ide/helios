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

#----- 2) Cluster Set-Up ---------------------------------------------------------------------------

devtools::load_all()
parameters <- get_parameters()

hipercow_environment_create(packages = c("individual", "helios", "tidyverse", "dqrng", "EnvStats"))

id5 <- hipercow::task_create_explicit(
  expr = quote({
    parameters <- helios::get_parameters()
    test <- list()
    for(i in 1:2) {
      test[[i]] <- helios::run_simulation(parameters_list = parameters)
      test[[i]]$ID <- i
    }
    saveRDS(test, "test.rds")

  }),
  resources = hipercow_resources(cores = 2),
  parallel = hipercow_parallel("parallel")
)
task_status(id5)
task_result(id5)

test <- readRDS("./test.rds")
test[[2]]

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

hipercow_environment_create(packages = c("individual", "helios", "tidyverse", "dqrng", "EnvStats"))

id5 <- hipercow::task_create_explicit(
  expr = quote({

    # Get the number of available cores:
    cores <- Sys.getenv("CCP_NUMCPUS")

    # Prepare the cluster given the number of cores detected:
    cluster <- parallel::makeCluster(as.integer(cores))

    # Set the library path:
    invisible(parallel::clusterCall(cluster, ".libPaths", .libPaths()))

    # Load the requisite functions
    parallel::clusterCall(cluster, function() {
      library(dplyr)
      library(individual)
      library(helios)
      library(tidyverse)
      library(dqrng)
      library(EnvStats)
      TRUE
    })

    # Read in the parameter lists:
    parameter_lists <- readRDS("./Report_3_Endemic/endemic_simulations_parameter_lists.rds")

    # Run the simulations:
    endemic_simulation_outputs <- parallel::parLapply(
      cl = cluster,
      X = parameter_lists,
      fun = helios::run_simulation,
    )

    # Save the simulation output:
    #saveRDS(object = parameter_lists, file = "./Report_3_Endemic/tests.rds")
    #saveRDS(object = endemic_simulation_outputs, file = "endemic_simulation_outputs.rds")

  }),
  resources = hipercow_resources(cores = 12),
  parallel = hipercow_parallel("parallel")
)
task_status(id5)
task_result(id5)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#



parallel::clusterApply(NULL,
                       1:length(parameter_lists),
                       function(x) {
                         p <- parameter_lists[[x]]
                         helios::run_simulation(parameters_list = p)
                       })


# Load the parameters lists:
parameter_lists <- readRDS("./Report_3_Endemic/endemic_simulations_parameter_lists.rds")

# Reduce the simulation time:
for(i in 1:length(parameter_lists)) {
  parameter_lists[[i]]$simulation_time <- 10
}


endemic_simulation_outputs <- parallel::parLapply(
  cl = cluster,
  X = parameter_lists,
  fun = run_simulation,
)

test_run <- lapply(parameter_lists, run_simulation)






test_run

