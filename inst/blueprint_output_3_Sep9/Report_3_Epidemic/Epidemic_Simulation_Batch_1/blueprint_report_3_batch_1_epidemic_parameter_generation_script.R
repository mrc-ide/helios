#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+++++ Blueprint Report 3.a: Far UVC under Epidemic Pathogens +++++#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

#+++ Introduction +++#
#++++++++++++++++++++#
##'
##' SARS-CoV-2 and Flu archetypes
##' far UVC coverages 10% - 80% (in chunks of 10%)
##' far UVC efficacies of 60% and 80%
##' human population size of 100,000
##'
##' Timesteps: Sufficient for dynamics to complete (try 3 years?)
##'
##' Run each simulation for 10 replicates
##'
##' Setting specifi riskiness switched on
##'
##' Far UVC coverage using "joint" setting with 10% of locations receiving far UVC
##'   a. At Random
##'   b. Targeted at 10% of locations with the greatest square footage
##'


#----- 1) Preamble ---------------------------------------------------------------------------------

# Set the working directory in the Epidemic_Simulation_Batch_3 folder:
setwd("./inst/blueprint_output_3_Sep9/Report_3_Epidemic/Epidemic_Simulation_Batch_1/")

# Load in the requisite packages:
library(helios)
library(tidyverse)
library(tictoc)
library(individual)
library(parallel)

#----- 2) Parameter Sweep Set-Up -------------------------------------------------------------------

# Number of iterations to simulate for each parameterisation:
iterations <- seq(25)

# Calculate the simulation_time required to simulate a 2 year period:
years_to_simulate <- 10
simulation_time_days <- (365 * years_to_simulate)

# Specify the timestep on which to switch far_UVC on:
timestep_uvc_on <- 1

# Set the human population size:
human_population <- 100000

# Archetypes to simulate for:
archetypes <- c("flu", "sars_cov_2")

# Riskiness setting:
riskiness <- c("setting_specific_riskiness")

# Set up a vector of far-UVC efficacies to simulate
far_uvc_efficacy <- c(0.4, 0.6, 0.8)

# Set up a vector of far-UVC coverages to simulate:
far_uvc_joint_coverage <- seq(0, 1, by = 0.1)

# Specify joint far UVC coverage type (random vs targeted)
uvc_joint_coverage_type <- c("random", "targeted_riskiness")

# Specify the setting-specific sizes per individual:
setting_size_per_ind_workplace <- 10
setting_size_per_ind_school <- 3.33
setting_size_per_ind_leisure <- 2
setting_size_per_ind_household <- 20

# Set up the unique simulations to run
simulations_to_run <- expand.grid("archetype" = archetypes,
                                  "coverage_type" = uvc_joint_coverage_type,
                                  "coverage" = far_uvc_joint_coverage,
                                  "efficacy" = far_uvc_efficacy,
                                  "iteration" = iterations,
                                  "riskiness" = riskiness,
                                  stringsAsFactors = FALSE)

# Arrange the dataframe:
simulations_to_run |>
  mutate(scenario = "epidemic") |>
  arrange(archetype, coverage_type, coverage, efficacy, iteration) |>
  mutate(ID = 1:nrow(simulations_to_run)) -> simulations_to_run

# View the simulations_to_run dataframe:
head(simulations_to_run)

# Set up a list to store the parameter_lists:
parameter_lists <- list()

# Set up the simulation parameter lists for each row of the simulations_to_run dataframe:
for(i in 1:nrow(simulations_to_run)) {

  #+++ SARS-CoV-2 +++#
  #++++++++++++++++++#
  if (simulations_to_run$archetype[i] == "sars_cov_2") {

    ## Setting up initial conditions (approx endemic equilibrium solution for R0 2.5 pathogen)
    initial_S_SC2 <- floor(0.9995 * human_population)
    initial_E_SC2 <- floor(0.0005 * human_population)
    initial_I_SC2 <- 0
    initial_R_SC2 <- human_population - initial_S_SC2 - initial_E_SC2 - initial_I_SC2

    # Establish the base parameter list:
    parameter_lists[[i]] <- get_parameters(archetype = simulations_to_run$archetype[i],
                                           overrides = list(

                                             # Specify the human population size and initial disease states:
                                             human_population = human_population,
                                             number_initial_S = initial_S_SC2,
                                             number_initial_E = initial_E_SC2,
                                             number_initial_I = initial_I_SC2,
                                             number_initial_R = initial_R_SC2,

                                             # Set the setting sizes:
                                             size_per_individual_workplace = setting_size_per_ind_workplace,
                                             size_per_individual_school = setting_size_per_ind_school,
                                             size_per_individual_leisure = setting_size_per_ind_leisure,
                                             size_per_individual_household = setting_size_per_ind_household,

                                             # Specify model to run in "endemic" disease setting:
                                             endemic_or_epidemic = "epidemic",

                                             # Specify the simulation duration
                                             simulation_time = simulation_time_days))

  } else if (simulations_to_run$archetype[i] == "flu") {

    #+++ SARS-CoV-2 +++#
    #++++++++++++++++++#
    # Setting up initial conditions (approx endemic equilibrium solution for R0 1.5 pathogen)
    initial_S_flu <- floor(0.9995 * human_population)
    initial_E_flu <- floor(0.0005 * human_population)
    initial_I_flu <- 0
    initial_R_flu <- human_population - initial_S_flu - initial_E_flu - initial_I_flu

    # Establish the base parameter list:
    parameter_lists[[i]] <- get_parameters(archetype = simulations_to_run$archetype[i],
                                           overrides = list(

                                             # Specify the human population size and initial disease states:
                                             human_population = human_population,
                                             number_initial_S = initial_S_flu,
                                             number_initial_E = initial_E_flu,
                                             number_initial_I = initial_I_flu,
                                             number_initial_R = initial_R_flu,

                                             # Set the setting sizes:
                                             size_per_individual_workplace = setting_size_per_ind_workplace,
                                             size_per_individual_school = setting_size_per_ind_school,
                                             size_per_individual_leisure = setting_size_per_ind_leisure,
                                             size_per_individual_household = setting_size_per_ind_household,

                                             # Specify model to run in "endemic" disease setting:
                                             endemic_or_epidemic = "epidemic",

                                             # Specify the simulation duration
                                             simulation_time = simulation_time_days))

  } else {

    # If the archetype is neither fly nor SARS-CoV-2 then halt the parameter generation process:
    stop("something's gone wrong withspecifying archetype")

  }

  # Set Far UVC intervention parameters:
  if(simulations_to_run$coverage[i] > 0) {
    parameter_lists[[i]] |>

      # Set UVC jointly:
      set_uvc(setting = "joint",
              coverage = simulations_to_run$coverage[i],
              coverage_target = "square_footage",
              coverage_type = simulations_to_run$coverage_type[i],
              efficacy = simulations_to_run$efficacy[i],
              timestep = timestep_uvc_on) -> parameter_lists[[i]]

  }

  # Set setting-specific riskiness:
  if(simulations_to_run$riskiness[i] == "setting_specific_riskiness") {
    parameter_lists[[i]] |>

      # Set setting-specific riskiness in Schools:
      set_setting_specific_riskiness(setting = "school",
                                     mean = 0,
                                     sd = 0.3544,
                                     min = 1/sqrt(4.75),
                                     max = sqrt(4.75)) |>

      # Set setting-specific riskiness in workplaces:
      set_setting_specific_riskiness(setting = "workplace",
                                     mean = 0,
                                     sd = 0.5072,
                                     min = 1/sqrt(6.35),
                                     max = sqrt(6.35)) |>

      # Set setting-specific riskiness in households:
      set_setting_specific_riskiness(setting = "household",
                                     mean = 0,
                                     sd = 0.0871,
                                     min = 1/sqrt(2.5),
                                     max = sqrt(2.5)) |>

      # Set setting-specific riskiness in leisure settings:
      set_setting_specific_riskiness(setting = "leisure",
                                     mean = 0,
                                     sd = 0.4278,
                                     min = 1/sqrt(5.5),
                                     max = sqrt(5.5)) -> parameter_lists[[i]]
  }
}

# Save the parameter lists and the simulations dataframe:
saveRDS(simulations_to_run, file = "./epidemic_simulations_table_batch_1.rds")
saveRDS(parameter_lists, file = "./epidemic_simulations_parameter_lists_batch_1.rds")

#----- 3) Batch Saving -----------------------------------------------------------------------------

# Get the number of simulation parameter sets:
number_of_simulations <- length(parameter_lists)

# Get the number of 32 core nodes you want to distribute the work over
nodes_to_use <- 30

# Get the number of simulations per node:
simulations_per_node <- number_of_simulations / nodes_to_use; simulations_per_node

# Get the lower bound of each simulation batch:
lower_bounds <- simulations_per_node * c(0, head(seq(nodes_to_use), -1)) + 1
upper_bounds <- simulations_per_node * seq(nodes_to_use)

# Assign each of the simulations to a sub-list:
for(i in 1:length(lower_bounds)) {

  # Store the parameter list batch into a sub-list with a name identifying the simulation set:
  assign(x = paste0("epidemic_simulation_parameter_list_", lower_bounds[i], "_", upper_bounds[i]),
         value = parameter_lists[lower_bounds[i]:upper_bounds[i]])

  # Save the parameter list as an object
  saveRDS(object = get(paste0("epidemic_simulation_parameter_list_", lower_bounds[i], "_", upper_bounds[i])),
          file = paste0("./epidemic_batch_1_inputs/epidemic_simulation_parameter_list_", i, "_", lower_bounds[i], "_", upper_bounds[i], ".rds"))

}

# Store an object with the batch indices:
saveRDS(1:nodes_to_use, "./batches.rds")

#--------------------------------------------------------------------------------------------------#
