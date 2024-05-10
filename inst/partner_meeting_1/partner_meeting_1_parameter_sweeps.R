#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+++++ Partner Meeting 1: Parameter Sweeps +++++#
#+++++++++++++++++++++++++++++++++++++++++++++++#

#----- 1) Preamble ---------------------------------------------------------------------------------

# Load in the requisite packages:
library(ggplot2)
library(tidyverse)
library(grid)
library(gridExtra)

#----- 2) Parameter Sweep Set-Up -------------------------------------------------------------------

##' In a separate script, we used the final_size() function from the finalsize package to idenfity
##' following beta_community values required to parameterise simulation with the following R0s of
##' interest, when the betas are parameterised in a ratio of 3:3:3:3:beta_community:
##'
##' R0      Exp. size   beta_community    Actual Prop.  Approx. time to Eq
##' 1.25    0.37        0.036             0.3607        t = 700
##' 1.5     0.58        0.044             0.588         t = 500
##' 2       0.8         0.062             0.8001        t = 250
##' 2.5     0.89        0.08              0.893         t = 200
##' 3       0.94        0.097             0.9415        t = 175
##'

# Calculate the simulation_time required to simulate a 2 year period:
years_to_simulate <- 3
simulation_timesteps <- (365 * years_to_simulate)

# Set up a vector of R0 values:
r0 = c(1.25, 1.5, 2, 2.5, 3)

# Store the beta_community values corresponding to each R0 in a vector:
beta_communities <- c(0.036, 0.044, 0.062, 0.080, 0.097)

# Use the generate_betas() function to generate the correpsonding betas for the rest of the settings:
simulation_betas <- generate_betas(beta_community = beta_communities,
                                   household_ratio = 3,
                                   school_ratio = 3,
                                   workplace_ratio = 3,
                                   leisure_ratio = 3)[,1:6]

# Append a vector of R0 values to the corresponding betas:
simulation_betas <- data.frame(r0, simulation_betas)

# Set up a vector of far-UVC coverages to simulate:
far_uvc_coverage <- seq(from = 0, to = 1, by = 0.2)

# Set up a vector of far-UVC efficacies to simulate
far_uvc_efficacy <- seq(from = 0, to = 1, by = 0.2)

# Set up the coverage types to be simulated
coverage_type <- c("random", "targeted")

# Number of iterations to simulate for each parameterisation:
iterations <- 1:5

# Set up the unique simulations to run
simulations_to_run <- expand.grid("iteration" = iterations,
                                  "r0" = r0,
                                  "coverage" = far_uvc_coverage,
                                  "efficacy" = far_uvc_efficacy,
                                  "coverage_type" = coverage_type)

# Append a column for simulation ID:
simulations_to_run$ID <- 1:nrow(simulations_to_run)

# Check the number of simulations that will be run (1800):
nrow(simulations_to_run)

# Attach the beta values corresponding to each R0:
simulations_to_run %>%
  inner_join(simulation_betas, by = "r0") -> simulations_to_run

# View the simulations_to_run dataframe:
head(simulations_to_run)

# Set up a list to store the parameter_lists:
parameter_lists <- list()

# Set up the simulation parameter lists for each row of the simulations_to_run dataframe:
for(i in 1:nrow(simulations_to_run)) {

  # Set up the parameters list:
  parameter_lists[[i]] <- get_parameters(overrides = list(
    human_population = 10000,
    beta_household = simulations_to_run$beta_household[i],
    beta_school = simulations_to_run$beta_school[i],
    beta_workplace = simulations_to_run$beta_workplace[i],
    beta_leisure = simulations_to_run$beta_leisure[i],
    beta_community = simulations_to_run$beta_community[i],
    endemic_or_epidemic = "epidemic",
    simulation_time = simulation_timesteps
  ))

  # If coverage and efficacy are greater than 0, append the far UVC parameters:
  if(simulations_to_run$coverage[i] > 0 | simulations_to_run$efficacy[i] > 0) {
    parameter_lists[[i]] %>%
      set_uvc(setting = "school",
              coverage = simulations_to_run$coverage[i],
              coverage_type = as.character(simulations_to_run$coverage_type[i]),
              efficacy = simulations_to_run$efficacy[i],
              timestep = 1) %>%
      set_uvc(setting = "workplace",
              coverage = simulations_to_run$coverage[i],
              coverage_type = as.character(simulations_to_run$coverage_type[i]),
              efficacy = simulations_to_run$efficacy[i],
              timestep = 1) %>%
      set_uvc(setting = "leisure",
              coverage = simulations_to_run$coverage[i],
              coverage_type = as.character(simulations_to_run$coverage_type[i]),
              efficacy = simulations_to_run$efficacy[i],
              timestep = 1) -> parameter_lists[[i]]
  }
}

#----- 3) Simulation Runs --------------------------------------------------------------------------

# Set up a list to store the simulation_outputs:
simulation_outputs <- list()

# Run through the simulations in simulations_to_run:
for(i in 1:length(parameter_lists)) {
  simulation_outputs[[i]] <- run_simulation(parameters_list = parameter_lists[[i]])
  print(paste0(i, "th simulation complete (", (i/length(parameter_lists))*100, "% complete)"))
}

#----- 4) Simulation Post-Processing ---------------------------------------------------------------




#----- 5) Visualisation ----------------------------------------------------------------------------


