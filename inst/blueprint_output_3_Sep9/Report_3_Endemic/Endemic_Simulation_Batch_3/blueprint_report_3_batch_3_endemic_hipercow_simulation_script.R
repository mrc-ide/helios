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

#----- 1) hipercow Set-Up --------------------------------------------------------------------------

# Set working directory to where provision.R is:
setwd("./inst/blueprint_output_3_Sep9")

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

# View the task statuses:
task_status(sim_out_batch_2_1)
task_status(sim_out_batch_2_2)
task_status(sim_out_batch_2_3)
task_status(sim_out_batch_2_4)
task_status(sim_out_batch_2_5)
task_status(sim_out_batch_2_6)
task_status(sim_out_batch_2_7)
task_status(sim_out_batch_2_8)
task_status(sim_out_batch_2_9)
task_status(sim_out_batch_2_10)
task_status(sim_out_batch_2_11)
task_status(sim_out_batch_2_12)
task_status(sim_out_batch_2_13)
task_status(sim_out_batch_2_14)
task_status(sim_out_batch_2_15)
task_status(sim_out_batch_2_16)
task_status(sim_out_batch_2_17)
task_status(sim_out_batch_2_18)
task_status(sim_out_batch_2_19)
task_status(sim_out_batch_2_20)
task_status(sim_out_batch_2_21)

#----- 2) Batch 1: Simulations 1:64 ----------------------------------------------------------------

# Create the task (2 simulations of the basic run_simulation() function:
sim_out_batch_2_1 <- hipercow::task_create_explicit(

  # Everything in the expr will be run on the cluster:
  expr = quote({

    # Load in the parameter lists:
    parameters <- readRDS("./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_inputs/endemic_simulation_parameter_list_1_64.rds")

    # Load in the run_simulations_hipercow() function:
    source("Report_3_Endemic/run_simulations_hipercow.R")

    # Run through the parameter lists and run the hipercow helios script:
    scenario_output_batch_2_1 <- parallel::parLapply(
      cl = NULL,
      X = parameters,
      fun = run_simulation_hipercow
    )

    # Store the simulation outputs:
    saveRDS(object = scenario_output_batch_2_1,
            file = "./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_outputs/scenario_output_batch_2_1.rds")

  }),

  # Specify the cluster resources etc.
  resources = hipercow_resources(cores = 32),
  parallel = hipercow_parallel("parallel")
)

# Check the task progress:
task_status(sim_out_batch_2_1)
#task_result(sim_out_batch_2_1)

#----- 3) Batch 2: Simulations 65:128 --------------------------------------------------------------

# Create the task (2 simulations of the basic run_simulation() function:
sim_out_batch_2_2 <- hipercow::task_create_explicit(

  # Everything in the expr will be run on the cluster:
  expr = quote({

    # Load in the parameter lists:
    parameters <- readRDS("./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_inputs/endemic_simulation_parameter_list_65_128.rds")

    # Load in the run_simulations_hipercow() function:
    source("Report_3_Endemic/run_simulations_hipercow.R")

    # Run through the parameter lists and run the hipercow helios script:
    scenario_output_batch_2_2 <- parallel::parLapply(
      cl = NULL,
      X = parameters,
      fun = run_simulation_hipercow
    )

    # Store the simulation outputs:
    saveRDS(object = scenario_output_batch_2_2,
            file = "./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_outputs/scenario_output_batch_2_2.rds")

  }),

  # Specify the cluster resources etc.
  resources = hipercow_resources(cores = 32),
  parallel = hipercow_parallel("parallel")
)

# Check the task progress:
task_status(sim_out_batch_2_2)
task_result(sim_out_batch_2_2)

#----- 4) Batch 3: Simulations 129:192 -------------------------------------------------------------

# Create the task (2 simulations of the basic run_simulation() function:
sim_out_batch_2_3 <- hipercow::task_create_explicit(

  # Everything in the expr will be run on the cluster:
  expr = quote({

    # Load in the parameter lists:
    parameters <- readRDS("./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_inputs/endemic_simulation_parameter_list_129_192.rds")

    # Load in the run_simulations_hipercow() function:
    source("Report_3_Endemic/run_simulations_hipercow.R")

    # Run through the parameter lists and run the hipercow helios script:
    scenario_output_batch_2_3 <- parallel::parLapply(
      cl = NULL,
      X = parameters,
      fun = run_simulation_hipercow
    )

    # Store the simulation outputs:
    saveRDS(object = scenario_output_batch_2_3,
            file = "./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_outputs/scenario_output_batch_2_3.rds")

  }),

  # Specify the cluster resources etc.
  resources = hipercow_resources(cores = 32),
  parallel = hipercow_parallel("parallel")
)

# Check the task progress:
task_status(sim_out_batch_2_3)
task_result(sim_out_batch_2_3)

#----- 5) Batch 4: Simulations 193:256 -------------------------------------------------------------

# Create the task (2 simulations of the basic run_simulation() function:
sim_out_batch_2_4 <- hipercow::task_create_explicit(

  # Everything in the expr will be run on the cluster:
  expr = quote({

    # Load in the parameter lists:
    parameters <- readRDS("./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_inputs/endemic_simulation_parameter_list_193_256.rds")

    # Load in the run_simulations_hipercow() function:
    source("Report_3_Endemic/run_simulations_hipercow.R")

    # Run through the parameter lists and run the hipercow helios script:
    scenario_output_batch_2_4 <- parallel::parLapply(
      cl = NULL,
      X = parameters,
      fun = run_simulation_hipercow
    )

    # Store the simulation outputs:
    saveRDS(object = scenario_output_batch_2_4,
            file = "./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_outputs/scenario_output_batch_2_4.rds")

  }),

  # Specify the cluster resources etc.
  resources = hipercow_resources(cores = 32),
  parallel = hipercow_parallel("parallel")
)

# Check the task progress:
task_status(sim_out_batch_2_4)
task_result(sim_out_batch_2_4)

#----- 6) Batch 5: Simulations 257:320 -------------------------------------------------------------

# Create the task (2 simulations of the basic run_simulation() function:
sim_out_batch_2_5 <- hipercow::task_create_explicit(

  # Everything in the expr will be run on the cluster:
  expr = quote({

    # Load in the parameter lists:
    parameters <- readRDS("./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_inputs/endemic_simulation_parameter_list_257_320.rds")

    # Load in the run_simulations_hipercow() function:
    source("Report_3_Endemic/run_simulations_hipercow.R")

    # Run through the parameter lists and run the hipercow helios script:
    scenario_output_batch_2_5 <- parallel::parLapply(
      cl = NULL,
      X = parameters,
      fun = run_simulation_hipercow
    )

    # Store the simulation outputs:
    saveRDS(object = scenario_output_batch_2_5,
            file = "./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_outputs/scenario_output_batch_2_5.rds")

  }),

  # Specify the cluster resources etc.
  resources = hipercow_resources(cores = 32),
  parallel = hipercow_parallel("parallel")
)

# Check the task progress:
task_status(sim_out_batch_2_5)
#task_result(sim_out_batch_2_5)

#----- 7) Batch 6: Simulations 321:384 -------------------------------------------------------------

# Create the task (2 simulations of the basic run_simulation() function:
sim_out_batch_2_6 <- hipercow::task_create_explicit(

  # Everything in the expr will be run on the cluster:
  expr = quote({

    # Load in the parameter lists:
    parameters <- readRDS("./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_inputs/endemic_simulation_parameter_list_321_384.rds")

    # Load in the run_simulations_hipercow() function:
    source("Report_3_Endemic/run_simulations_hipercow.R")

    # Run through the parameter lists and run the hipercow helios script:
    scenario_output_batch_2_6 <- parallel::parLapply(
      cl = NULL,
      X = parameters,
      fun = run_simulation_hipercow
    )

    # Store the simulation outputs:
    saveRDS(object = scenario_output_batch_2_6,
            file = "./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_outputs/scenario_output_batch_2_6.rds")

  }),

  # Specify the cluster resources etc.
  resources = hipercow_resources(cores = 32),
  parallel = hipercow_parallel("parallel")
)

# Check the task progress:
task_status(sim_out_batch_2_6)
task_result(sim_out_batch_2_6)

#----- 8) Batch 7: Simulations 385:448 -------------------------------------------------------------

# Create the task (2 simulations of the basic run_simulation() function:
sim_out_batch_2_7 <- hipercow::task_create_explicit(

  # Everything in the expr will be run on the cluster:
  expr = quote({

    # Load in the parameter lists:
    parameters <- readRDS("./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_inputs/endemic_simulation_parameter_list_385_448.rds")

    # Load in the run_simulations_hipercow() function:
    source("Report_3_Endemic/run_simulations_hipercow.R")

    # Run through the parameter lists and run the hipercow helios script:
    scenario_output_batch_2_7 <- parallel::parLapply(
      cl = NULL,
      X = parameters,
      fun = run_simulation_hipercow
    )

    # Store the simulation outputs:
    saveRDS(object = scenario_output_batch_2_7,
            file = "./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_outputs/scenario_output_batch_2_7.rds")

  }),

  # Specify the cluster resources etc.
  resources = hipercow_resources(cores = 32),
  parallel = hipercow_parallel("parallel")
)

# Check the task progress:
task_status(sim_out_batch_2_7)
task_result(sim_out_batch_2_7)

#----- 9) Batch 8: Simulations 449:512 -------------------------------------------------------------

# Create the task (2 simulations of the basic run_simulation() function:
sim_out_batch_2_8 <- hipercow::task_create_explicit(

  # Everything in the expr will be run on the cluster:
  expr = quote({

    # Load in the parameter lists:
    parameters <- readRDS("./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_inputs/endemic_simulation_parameter_list_449_512.rds")

    # Load in the run_simulations_hipercow() function:
    source("Report_3_Endemic/run_simulations_hipercow.R")

    # Run through the parameter lists and run the hipercow helios script:
    scenario_output_batch_2_8 <- parallel::parLapply(
      cl = NULL,
      X = parameters,
      fun = run_simulation_hipercow
    )

    # Store the simulation outputs:
    saveRDS(object = scenario_output_batch_2_8,
            file = "./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_outputs/scenario_output_batch_2_8.rds")

  }),

  # Specify the cluster resources etc.
  resources = hipercow_resources(cores = 32),
  parallel = hipercow_parallel("parallel")
)

# Check the task progress:
task_status(sim_out_batch_2_8)
task_result(sim_out_batch_2_8)

#----- 10) Batch 9: Simulations 513:576 ------------------------------------------------------------

# Create the task (2 simulations of the basic run_simulation() function:
sim_out_batch_2_9 <- hipercow::task_create_explicit(

  # Everything in the expr will be run on the cluster:
  expr = quote({

    # Load in the parameter lists:
    parameters <- readRDS("./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_inputs/endemic_simulation_parameter_list_513_576.rds")

    # Load in the run_simulations_hipercow() function:
    source("Report_3_Endemic/run_simulations_hipercow.R")

    # Run through the parameter lists and run the hipercow helios script:
    scenario_output_batch_2_9 <- parallel::parLapply(
      cl = NULL,
      X = parameters,
      fun = run_simulation_hipercow
    )

    # Store the simulation outputs:
    saveRDS(object = scenario_output_batch_2_9,
            file = "./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_outputs/scenario_output_batch_2_9.rds")

  }),

  # Specify the cluster resources etc.
  resources = hipercow_resources(cores = 32),
  parallel = hipercow_parallel("parallel")
)

# Check the task progress:
task_status(sim_out_batch_2_9)
task_result(sim_out_batch_2_9)

#----- 11) Batch 10: Simulations 577:640 -----------------------------------------------------------

# Create the task (2 simulations of the basic run_simulation() function:
sim_out_batch_2_10 <- hipercow::task_create_explicit(

  # Everything in the expr will be run on the cluster:
  expr = quote({

    # Load in the parameter lists:
    parameters <- readRDS("./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_inputs/endemic_simulation_parameter_list_577_640.rds")

    # Load in the run_simulations_hipercow() function:
    source("Report_3_Endemic/run_simulations_hipercow.R")

    # Run through the parameter lists and run the hipercow helios script:
    scenario_output_batch_2_10 <- parallel::parLapply(
      cl = NULL,
      X = parameters,
      fun = run_simulation_hipercow
    )

    # Store the simulation outputs:
    saveRDS(object = scenario_output_batch_2_10,
            file = "./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_outputs/scenario_output_batch_2_10.rds")

  }),

  # Specify the cluster resources etc.
  resources = hipercow_resources(cores = 32),
  parallel = hipercow_parallel("parallel")
)

# Check the task progress:
task_status(sim_out_batch_2_10)
task_result(sim_out_batch_2_10)

#----- 12) Batch 11: Simulations 641:704 -----------------------------------------------------------

# Create the task (2 simulations of the basic run_simulation() function:
sim_out_batch_2_11 <- hipercow::task_create_explicit(

  # Everything in the expr will be run on the cluster:
  expr = quote({

    # Load in the parameter lists:
    parameters <- readRDS("./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_inputs/endemic_simulation_parameter_list_641_704.rds")

    # Load in the run_simulations_hipercow() function:
    source("Report_3_Endemic/run_simulations_hipercow.R")

    # Run through the parameter lists and run the hipercow helios script:
    scenario_output_batch_2_11 <- parallel::parLapply(
      cl = NULL,
      X = parameters,
      fun = run_simulation_hipercow
    )

    # Store the simulation outputs:
    saveRDS(object = scenario_output_batch_2_11,
            file = "./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_outputs/scenario_output_batch_2_11.rds")

  }),

  # Specify the cluster resources etc.
  resources = hipercow_resources(cores = 32),
  parallel = hipercow_parallel("parallel")
)

# Check the task progress:
task_status(sim_out_batch_2_11)
#task_result(sim_out_batch_2_11)

#----- 13) Batch 12: Simulations 705:768 -----------------------------------------------------------

# Create the task (2 simulations of the basic run_simulation() function:
sim_out_batch_2_12 <- hipercow::task_create_explicit(

  # Everything in the expr will be run on the cluster:
  expr = quote({

    # Load in the parameter lists:
    parameters <- readRDS("./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_inputs/endemic_simulation_parameter_list_705_768.rds")

    # Load in the run_simulations_hipercow() function:
    source("Report_3_Endemic/run_simulations_hipercow.R")

    # Run through the parameter lists and run the hipercow helios script:
    scenario_output_batch_2_12 <- parallel::parLapply(
      cl = NULL,
      X = parameters,
      fun = run_simulation_hipercow
    )

    # Store the simulation outputs:
    saveRDS(object = scenario_output_batch_2_12,
            file = "./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_outputs/scenario_output_batch_2_12.rds")

  }),

  # Specify the cluster resources etc.
  resources = hipercow_resources(cores = 32),
  parallel = hipercow_parallel("parallel")
)

# Check the task progress:
task_status(sim_out_batch_2_12)
#task_result(sim_out_batch_2_12)

#----- 14) Batch 13: Simulations 769:832 -----------------------------------------------------------

# Create the task (2 simulations of the basic run_simulation() function:
sim_out_batch_2_13 <- hipercow::task_create_explicit(

  # Everything in the expr will be run on the cluster:
  expr = quote({

    # Load in the parameter lists:
    parameters <- readRDS("./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_inputs/endemic_simulation_parameter_list_769_832.rds")

    # Load in the run_simulations_hipercow() function:
    source("Report_3_Endemic/run_simulations_hipercow.R")

    # Run through the parameter lists and run the hipercow helios script:
    scenario_output_batch_2_13 <- parallel::parLapply(
      cl = NULL,
      X = parameters,
      fun = run_simulation_hipercow
    )

    # Store the simulation outputs:
    saveRDS(object = scenario_output_batch_2_13,
            file = "./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_outputs/scenario_output_batch_2_13.rds")

  }),

  # Specify the cluster resources etc.
  resources = hipercow_resources(cores = 32),
  parallel = hipercow_parallel("parallel")
)

# Check the task progress:
task_status(sim_out_batch_2_13)
#task_result(sim_out_batch_2_13)

#----- 15) Batch 14: Simulations 833:896 -----------------------------------------------------------

# Create the task (2 simulations of the basic run_simulation() function:
sim_out_batch_2_14 <- hipercow::task_create_explicit(

  # Everything in the expr will be run on the cluster:
  expr = quote({

    # Load in the parameter lists:
    parameters <- readRDS("./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_inputs/endemic_simulation_parameter_list_833_896.rds")

    # Load in the run_simulations_hipercow() function:
    source("Report_3_Endemic/run_simulations_hipercow.R")

    # Run through the parameter lists and run the hipercow helios script:
    scenario_output_batch_2_14 <- parallel::parLapply(
      cl = NULL,
      X = parameters,
      fun = run_simulation_hipercow
    )

    # Store the simulation outputs:
    saveRDS(object = scenario_output_batch_2_14,
            file = "./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_outputs/scenario_output_batch_2_14.rds")

  }),

  # Specify the cluster resources etc.
  resources = hipercow_resources(cores = 32),
  parallel = hipercow_parallel("parallel")
)

# Check the task progress:
task_status(sim_out_batch_2_14)
#task_result(sim_out_batch_2_14)

#----- 16) Batch 15: Simulations 897:960 -----------------------------------------------------------

# Create the task (2 simulations of the basic run_simulation() function:
sim_out_batch_2_15 <- hipercow::task_create_explicit(

  # Everything in the expr will be run on the cluster:
  expr = quote({

    # Load in the parameter lists:
    parameters <- readRDS("./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_inputs/endemic_simulation_parameter_list_897_960.rds")

    # Load in the run_simulations_hipercow() function:
    source("Report_3_Endemic/run_simulations_hipercow.R")

    # Run through the parameter lists and run the hipercow helios script:
    scenario_output_batch_2_15 <- parallel::parLapply(
      cl = NULL,
      X = parameters,
      fun = run_simulation_hipercow
    )

    # Store the simulation outputs:
    saveRDS(object = scenario_output_batch_2_15,
            file = "./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_outputs/scenario_output_batch_2_15.rds")

  }),

  # Specify the cluster resources etc.
  resources = hipercow_resources(cores = 32),
  parallel = hipercow_parallel("parallel")
)

# Check the task progress:
task_status(sim_out_batch_2_15)
#task_result(sim_out_batch_2_15)

#----- 17) Batch 16: Simulations 961:1024 ----------------------------------------------------------

# Create the task (2 simulations of the basic run_simulation() function:
sim_out_batch_2_16 <- hipercow::task_create_explicit(

  # Everything in the expr will be run on the cluster:
  expr = quote({

    # Load in the parameter lists:
    parameters <- readRDS("./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_inputs/endemic_simulation_parameter_list_961_1024.rds")

    # Load in the run_simulations_hipercow() function:
    source("Report_3_Endemic/run_simulations_hipercow.R")

    # Run through the parameter lists and run the hipercow helios script:
    scenario_output_batch_2_16 <- parallel::parLapply(
      cl = NULL,
      X = parameters,
      fun = run_simulation_hipercow
    )

    # Store the simulation outputs:
    saveRDS(object = scenario_output_batch_2_16,
            file = "./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_outputs/scenario_output_batch_2_16.rds")

  }),

  # Specify the cluster resources etc.
  resources = hipercow_resources(cores = 32),
  parallel = hipercow_parallel("parallel")
)

# Check the task progress:
task_status(sim_out_batch_2_16)
#task_result(sim_out_batch_2_16)

#----- 18) Batch 17: Simulations 1025:1088 ---------------------------------------------------------

# Create the task (2 simulations of the basic run_simulation() function:
sim_out_batch_2_17 <- hipercow::task_create_explicit(

  # Everything in the expr will be run on the cluster:
  expr = quote({

    # Load in the parameter lists:
    parameters <- readRDS("./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_inputs/endemic_simulation_parameter_list_1025_1088.rds")

    # Load in the run_simulations_hipercow() function:
    source("Report_3_Endemic/run_simulations_hipercow.R")

    # Run through the parameter lists and run the hipercow helios script:
    scenario_output_batch_2_17 <- parallel::parLapply(
      cl = NULL,
      X = parameters,
      fun = run_simulation_hipercow
    )

    # Store the simulation outputs:
    saveRDS(object = scenario_output_batch_2_17,
            file = "./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_outputs/scenario_output_batch_2_17.rds")

  }),

  # Specify the cluster resources etc.
  resources = hipercow_resources(cores = 32),
  parallel = hipercow_parallel("parallel")
)

# Check the task progress:
task_status(sim_out_batch_2_17)
#task_result(sim_out_batch_2_17)

#----- 19) Batch 18: Simulations 1089:1152 ---------------------------------------------------------

# Create the task (2 simulations of the basic run_simulation() function:
sim_out_batch_2_18 <- hipercow::task_create_explicit(

  # Everything in the expr will be run on the cluster:
  expr = quote({

    # Load in the parameter lists:
    parameters <- readRDS("./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_inputs/endemic_simulation_parameter_list_1089_1152.rds")

    # Load in the run_simulations_hipercow() function:
    source("Report_3_Endemic/run_simulations_hipercow.R")

    # Run through the parameter lists and run the hipercow helios script:
    scenario_output_batch_2_18 <- parallel::parLapply(
      cl = NULL,
      X = parameters,
      fun = run_simulation_hipercow
    )

    # Store the simulation outputs:
    saveRDS(object = scenario_output_batch_2_18,
            file = "./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_outputs/scenario_output_batch_2_18.rds")

  }),

  # Specify the cluster resources etc.
  resources = hipercow_resources(cores = 32),
  parallel = hipercow_parallel("parallel")
)

# Check the task progress:
task_status(sim_out_batch_2_18)
#task_result(sim_out_batch_2_18)

#----- 20) Batch 19: Simulations 1153:1216 ---------------------------------------------------------

# Create the task (2 simulations of the basic run_simulation() function:
sim_out_batch_2_19 <- hipercow::task_create_explicit(

  # Everything in the expr will be run on the cluster:
  expr = quote({

    # Load in the parameter lists:
    parameters <- readRDS("./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_inputs/endemic_simulation_parameter_list_1153_1216.rds")

    # Load in the run_simulations_hipercow() function:
    source("Report_3_Endemic/run_simulations_hipercow.R")

    # Run through the parameter lists and run the hipercow helios script:
    scenario_output_batch_2_19 <- parallel::parLapply(
      cl = NULL,
      X = parameters,
      fun = run_simulation_hipercow
    )

    # Store the simulation outputs:
    saveRDS(object = scenario_output_batch_2_19,
            file = "./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_outputs/scenario_output_batch_2_19.rds")

  }),

  # Specify the cluster resources etc.
  resources = hipercow_resources(cores = 32),
  parallel = hipercow_parallel("parallel")
)

# Check the task progress:
task_status(sim_out_batch_2_19)
#task_result(sim_out_batch_2_19)

#----- 21) Batch 20: Simulations 1217:1280 ---------------------------------------------------------

# Create the task (2 simulations of the basic run_simulation() function:
sim_out_batch_2_20 <- hipercow::task_create_explicit(

  # Everything in the expr will be run on the cluster:
  expr = quote({

    # Load in the parameter lists:
    parameters <- readRDS("./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_inputs/endemic_simulation_parameter_list_1217_1280.rds")

    # Load in the run_simulations_hipercow() function:
    source("Report_3_Endemic/run_simulations_hipercow.R")

    # Run through the parameter lists and run the hipercow helios script:
    scenario_output_batch_2_20 <- parallel::parLapply(
      cl = NULL,
      X = parameters,
      fun = run_simulation_hipercow
    )

    # Store the simulation outputs:
    saveRDS(object = scenario_output_batch_2_20,
            file = "./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_outputs/scenario_output_batch_2_20.rds")

  }),

  # Specify the cluster resources etc.
  resources = hipercow_resources(cores = 32),
  parallel = hipercow_parallel("parallel")
)

# Check the task progress:
task_status(sim_out_batch_2_20)
#task_result(sim_out_batch_2_20)

#----- 22) Batch 21: Simulations 1281:1320 ---------------------------------------------------------

# Create the task (2 simulations of the basic run_simulation() function:
sim_out_batch_2_21 <- hipercow::task_create_explicit(

  # Everything in the expr will be run on the cluster:
  expr = quote({

    # Load in the parameter lists:
    parameters <- readRDS("./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_inputs/endemic_simulation_parameter_list_1281_1320.rds")

    # Load in the run_simulations_hipercow() function:
    source("Report_3_Endemic/run_simulations_hipercow.R")

    # Run through the parameter lists and run the hipercow helios script:
    scenario_output_batch_2_21 <- parallel::parLapply(
      cl = NULL,
      X = parameters,
      fun = run_simulation_hipercow
    )

    # Store the simulation outputs:
    saveRDS(object = scenario_output_batch_2_21,
            file = "./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_outputs/scenario_output_batch_2_21.rds")

  }),

  # Specify the cluster resources etc.
  resources = hipercow_resources(cores = 32),
  parallel = hipercow_parallel("parallel")
)

# Check the task progress:
task_status(sim_out_batch_2_21)
#task_result(sim_out_batch_2_21)

#----- 23) Save the job IDs for each batch ---------------------------------------------------------

# Store the job IDs:
job_ids <- c(sim_out_batch_2_1,
             sim_out_batch_2_2,
             sim_out_batch_2_3,
             sim_out_batch_2_4,
             sim_out_batch_2_5,
             sim_out_batch_2_6,
             sim_out_batch_2_7,
             sim_out_batch_2_8,
             sim_out_batch_2_9,
             sim_out_batch_2_10,
             sim_out_batch_2_11,
             sim_out_batch_2_12,
             sim_out_batch_2_13,
             sim_out_batch_2_14,
             sim_out_batch_2_15,
             sim_out_batch_2_16,
             sim_out_batch_2_17,
             sim_out_batch_2_18,
             sim_out_batch_2_19,
             sim_out_batch_2_20,
             sim_out_batch_2_21)
saveRDS(object = job_ids, file = "./Report_3_Endemic/Endemic_Simulation_Batch_2/endemic_batch_2_b_outputs/batch_2_b_job_ids.rds")

