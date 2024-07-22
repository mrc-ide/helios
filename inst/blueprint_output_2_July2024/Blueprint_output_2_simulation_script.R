#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+++++ Blueprint Report 2: Setting-Specific Riskiness Simulations +++++#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

#+++ Introduction +++#
#++++++++++++++++++++#
##'
##' In this suite of simulations we are:
##'
##' + Modelling an endemic scenario
##' + Modelling two pathogen archtypes - influenza and SARS-CoV-2
##' + Modelling random Far UVC coverage
##' + Modelling varied setting-specific riskiness
##' + Sweeping over Far UVC coverage and efficacy
##' + Running 10 iterations for each parameterisation
##'

#----- 1) Preamble ---------------------------------------------------------------------------------

# Load in the requisite packages:
library(ggplot2)
library(tidyverse)
library(grid)
library(gridExtra)

#----- 2) Parameter Sweep Set-Up -------------------------------------------------------------------

# Calculate the simulation_time required to simulate a 2 year period:
# years_to_simulate <- 10
# simulation_timesteps <- (365 * years_to_simulate)
simulation_timesteps <- 10

# Set the human population size:
human_population <- 100

# Specify a duration of immunity following infection:
duration_of_immunity <- 365

# Set a probability of infection from an external source:
external_infection_probability <- 0.005

# Archetypes to simulate for:
archetypes <- c("flu", "sars_cov_2")

# Riskiness setting:
riskiness <- c("uniform_riskiness", "setting_specific_riskiness")

# Set up a vector of far-UVC coverages to simulate:
far_uvc_coverage <- c(0, 0.1, 0.25, 0.5, 1)

# Set up a vector of far-UVC efficacies to simulate
far_uvc_efficacy <- seq(0.2, 1, 0.2)

# Number of iterations to simulate for each parameterisation:
iterations <- 1:5

# Set up the unique simulations to run
simulations_to_run <- expand.grid("riskiness" = riskiness,
                                  "archetype" = archetypes,
                                  "coverage" = far_uvc_coverage,
                                  "efficacy" = far_uvc_efficacy,
                                  "iteration" = iterations)

# Append a column for simulation ID:
simulations_to_run$ID <- 1:nrow(simulations_to_run)

# Check the number of simulations that will be run (1800):
nrow(simulations_to_run)

# View the simulations_to_run dataframe:
head(simulations_to_run)

# Set up a list to store the parameter_lists:
parameter_lists <- list()

# Set up the simulation parameter lists for each row of the simulations_to_run dataframe:
for(i in 1:nrow(simulations_to_run)) {

  # Set up the parameters list:
  parameter_lists[[i]] <- get_parameters(archetype = ,
                                         overrides = list(
                                           human_population = human_population,
                                           endemic_or_epidemic = "endemic",
                                           duration_immune = duration_of_immunity,
                                           prob_inf_external = external_infection_probability,
                                           simulation_time = simulation_timesteps))

  # If coverage is greater than 0 append the far UVC parameters:
  if(simulations_to_run$coverage[i] > 0) {
    parameter_lists[[i]] |>
      set_uvc(setting = "school",
              coverage = simulations_to_run$coverage[i],
              coverage_target = "buildings",
              coverage_type = "random",
              efficacy = simulations_to_run$efficacy[i],
              timestep = 1) |>
      set_uvc(setting = "workplace",
              coverage = simulations_to_run$coverage[i],
              coverage_target = "buildings",
              coverage_type = "random",
              efficacy = simulations_to_run$efficacy[i],
              timestep = 1) |>
      set_uvc(setting = "leisure",
              coverage = simulations_to_run$coverage[i],
              coverage_target = "buildings",
              coverage_type = "random",
              efficacy = simulations_to_run$efficacy[i],
              timestep = 1) -> parameter_lists[[i]]
  }

  # If setting-specific-riskiness is parameterised, append the setting-specific riskiness parameters:
  if(simulations_to_run$riskiness[i] == "setting_specific_riskiness") {
    parameter_lists[[i]] |>
      set_setting_specific_riskiness(setting = "school",
                                     mean = 0,
                                     sd = 0.37,
                                     min = 0.4472,
                                     max = 2.236) |>
      set_setting_specific_riskiness(setting = "workplace",
                                     mean = 0,
                                     sd = 0.37,
                                     min = 0.4472,
                                     max = 2.236) |>
      set_setting_specific_riskiness(setting = "leisure",
                                     mean = 0,
                                     sd = 0.37,
                                     min = 0.4472,
                                     max = 2.236) -> parameter_lists[[i]]
  }
}

#----- 3) Simulation Runs --------------------------------------------------------------------------

# Set up a list to store the simulation_outputs:
simulation_outputs <- list()

# Run through the simulations in simulations_to_run:
for(i in 1:length(parameter_lists)) {
  simulation_outputs[[i]] <- run_simulation(parameters_list = parameter_lists[[i]])
  simulation_outputs[[i]]$ID <- simulations_to_run$ID[i]
  simulation_outputs[[i]]$riskiness_setting <- simulations_to_run$riskiness[i]
  simulation_outputs[[i]]$archetype <- simulations_to_run$archetype[i]
  simulation_outputs[[i]]$coverage <- simulations_to_run$coverage[i]
  simulation_outputs[[i]]$efficacy <- simulations_to_run$efficacy[i]
  simulation_outputs[[i]]$iteration <- simulations_to_run$iteration[i]
  print(paste0(i, "th simulation complete (", (i/length(parameter_lists))*100, "% complete)"))
}
