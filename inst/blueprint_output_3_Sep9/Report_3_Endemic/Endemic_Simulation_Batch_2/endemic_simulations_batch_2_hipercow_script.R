#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+++++ Blueprint Report 3: Endemic Simulations Hipercow Script (Batch 2) +++++#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

#+++ ABOUT +++#
#+++++++++++++#

##' This script reads in the .rds of parameters lists generated for the Report 3 endemic simulations
##' and runs them in parallel on the Imperial cluster via the hipercow package functions. The output
##' dataframes are stored, along with the corresponding parameter list, in a combined .rds file in
##' the Report_3_Endemic directory.

# Load packages:
library(hipercow)
library(tidyverse)
library(helios)
library(patchwork)

#----- 1) hipercow Set-Up --------------------------------------------------------------------------

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

# Create the environment for hipercow
hipercow::hipercow_environment_create(packages = c("individual", "helios", "tidyverse", "dqrng", "EnvStats"))

# Store the job IDs:
job_ids <- c(sim_out_batch_1,
             sim_out_batch_2,
             sim_out_batch_3,
             sim_out_batch_4,
             sim_out_batch_5,
             sim_out_batch_6,
             sim_out_batch_7)
#saveRDS(object = job_ids, file = "./Report_3_Endemic/first_batch_job_ids.rds")

# Load the job IDs:
#job_ids <- readRDS("./Report_3_Endemic/first_batch_job_ids.rds")

# View the task statuses:
task_status(sim_out_batch_1)
task_status(sim_out_batch_2)
task_status(sim_out_batch_3)
task_status(sim_out_batch_4)
task_status(sim_out_batch_5)
task_status(sim_out_batch_6)
task_status(sim_out_batch_7)

#----- 2) Batch 1: Simulations 1:64 ----------------------------------------------------------------

# Create the task (2 simulations of the basic run_simulation() function:
sim_out_batch_1 <- hipercow::task_create_explicit(

  # Everything in the expr will be run on the cluster:
  expr = quote({

    # Load in the parameter lists:
    parameters <- readRDS("./Report_3_Endemic/endemic_parameter_list_1.rds")

    # Load in the run_simulations_hipercow() function:
    source("./Report_3_Endemic/run_simulations_hipercow.R")

    # Run through the parameter lists and run the hipercow helios script:
    scenario_output_batch_1 <- parallel::parLapply(
      cl = NULL,
      X = parameters,
      fun = run_simulation_hipercow
    )

    # Store the simulation outputs:
    saveRDS(object = scenario_output_batch_1, file = "./Report_3_Endemic/scenario_output_batch_1.rds")

  }),

  # Specify the cluster resources etc.
  resources = hipercow_resources(cores = 32),
  parallel = hipercow_parallel("parallel")
)

# Check the task progress:
task_status(sim_out_batch_1)
#task_result(sim_out_batch_1)

