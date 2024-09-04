#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+++++ Blueprint Report 3: Endemic Simulations Hipercow Script (Batch 3) +++++#
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
library(individual)
library(stringr)

#----- 1) hipercow Set-Up --------------------------------------------------------------------------

##' Set working directory to where provision.R is (helios/inst/blueprint_output_3_Sep9)

# Read in the parameter table:
#parameter_table <- readRDS("./Report_3_Endemic/Endemic_Simulation_Batch_3/endemic_simulations_table_batch_3.rds")

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
hipercow_environment_create(packages = c("individual", "helios", "tidyverse", "dqrng", "EnvStats"))

# Load the job IDs:
#job_ids <- readRDS("./Report_3_Endemic/first_batch_job_ids.rds")

#----- X) Auto-assigning batches to nodes ----------------------------------------------------------

##' Re-runs:
##' 2
##' 6
##' 10
##' 11
##' 14
##' 17
##' 20
##' 27
##'

# Store the files to load:
batches <- list.files("./Report_3_Endemic/Endemic_Simulation_Batch_3/endemic_batch_3_inputs/")
batches <- str_sort(batches, numeric = TRUE)

# For each batch, run the simulations:
n <- 27; for(i in n:n) {

  # Store i as the the batch number:
  batch_number <- i
  saveRDS(object = batch_number, file = "./Report_3_Endemic/Endemic_Simulation_Batch_3/batch_number.rds")

  # Print the iteration:
  print(batch_number)

  # Run the simulation batch and assign the ID to a batch-specific object:
  assign(

    # Name the object to save the simulation batch ID to:
    x = paste0("sim_out_batch_3_", i),

    # Assign the object the hipercow ID:
    value = hipercow::task_create_explicit(

      # Everything in the expr will be run on the cluster:
      expr = quote({

        # Store the files to load:
        batches <- list.files("./Report_3_Endemic/Endemic_Simulation_Batch_3/endemic_batch_3_inputs/")
        batches <- str_sort(batches, numeric = TRUE)

        # Load the batch number to simulate:
        batch_number <- readRDS("./Report_3_Endemic/Endemic_Simulation_Batch_3/batch_number.rds")

        # Load in the i-th batch of parameter lists:
        parameters <- readRDS(
          file = paste0("./Report_3_Endemic/Endemic_Simulation_Batch_3/endemic_batch_3_inputs/", batches[batch_number])
        )

        # Load in the run_simulations_hipercow() function:
        source("Report_3_Endemic/run_simulations_hipercow.R")

        # Run through the parameter lists and run the hipercow helios script:
        scenario_outputs <- parallel::parLapply(
          cl = NULL,
          X = parameters,
          fun = run_simulation_hipercow
        )

        # Store the simulation outputs:
        saveRDS(
          object = scenario_outputs,
          file = paste0("./Report_3_Endemic/Endemic_Simulation_Batch_3/endemic_batch_3_outputs/scenario_output_batch_3_", batch_number, ".rds")
        )

      }),

      # Specify the cluster resources etc.
      resources = hipercow_resources(cores = 32),
      parallel = hipercow_parallel("parallel")
    )
  )
}

# Set running at 22:33pm
hipercow::task_status(sim_out_batch_3_2)
hipercow::task_status(sim_out_batch_3_6)
hipercow::task_status(sim_out_batch_3_10)
hipercow::task_status(sim_out_batch_3_11)
hipercow::task_status(sim_out_batch_3_14)
hipercow::task_status(sim_out_batch_3_17)
hipercow::task_status(sim_out_batch_3_20)
hipercow::task_status(sim_out_batch_3_27)


#----- 3) Save the job IDs for each batch ---------------------------------------------------------

# Store the job IDs for each simulation in an object:
job_IDS <- list()

# Specify the re-run batch numbers
reruns <- c(2, 6, 10, 11, 14, 17, 20, 27)

for(i in 1:length(reruns)) {
  job_IDS[[i]] <- get(paste0("sim_out_batch_3_", reruns[i]))
}

# View the current task statuses
for(i in 1:length(reruns)) {
  print(reruns[i])
  print(task_status(job_IDS[[i]]))
}

# Check the status of all jobs running on the cluster:
x <- sapply(job_IDS, hipercow::task_status); table(x)

# View the failures:
which(x == "failure")

# Save the job IDs:
saveRDS(object = job_IDS, file = "./Report_3_Endemic/Endemic_Simulation_Batch_3/batch_3_job_ids.rds")

#----- 3) Save the job IDs for each batch ---------------------------------------------------------

length(list.files("./Report_3_Endemic/Endemic_Simulation_Batch_3/endemic_batch_3_outputs/"))
