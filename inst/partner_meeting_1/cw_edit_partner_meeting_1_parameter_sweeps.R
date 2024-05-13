#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+++++ Partner Meeting 1: Parameter Sweeps +++++#
#+++++++++++++++++++++++++++++++++++++++++++++++#

#----- 1) Preamble ---------------------------------------------------------------------------------

# Load in the requisite packages:
library(ggplot2)
library(tidyverse)
library(grid)
library(gridExtra)
library(parallel)

generate_betas <- function(beta_community, household_ratio, school_ratio, workplace_ratio, leisure_ratio) {

  # Use the community betas to generate the household, school, workplace, and leisure betas:
  beta_household = household_ratio * beta_community
  beta_school = school_ratio * beta_community
  beta_workplace = workplace_ratio * beta_community
  beta_leisure = leisure_ratio * beta_community

  # Combine the betas into a dataframe:
  betas <- data.frame(
    beta_household = beta_household,
    beta_school = beta_school,
    beta_workplace = beta_workplace,
    beta_leisure = beta_leisure,
    beta_community = beta_community)

  # Append columns giving the proportion of the total beta accounted for in each setting:
  betas %>%
    mutate(beta_total = beta_household + beta_school + beta_workplace + beta_leisure + beta_community) %>%
    mutate(prop_household = beta_household / beta_total,
           prop_school = beta_school / beta_total,
           prop_workplace = beta_workplace / beta_total,
           prop_leisure = beta_leisure / beta_total,
           prop_community = beta_community / beta_total) -> betas

  # Return the data frame of betas:
  return(betas)

}

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

# Set up infrastructure to run in parallel
num_cores <- 2
cl <- makeCluster(num_cores)
clusterExport(cl, list("simulations_to_run", "simulation_timesteps"))
clusterEvalQ(cl, library(helios))

# Run through the simulations in simulations_to_run:
results <- parLapply(cl, 1:nrow(simulations_to_run), function(i) {

  # Set up the parameters list:
  temp_parameter_list <- get_parameters(overrides = list(
    human_population = 10000,
    beta_household = simulations_to_run$beta_household[i],
    beta_school = simulations_to_run$beta_school[i],
    beta_workplace = simulations_to_run$beta_workplace[i],
    beta_leisure = simulations_to_run$beta_leisure[i],
    beta_community = simulations_to_run$beta_community[i],
    endemic_or_epidemic = "epidemic",
    simulation_time = 10 # simulation_timesteps
  ))

  # If coverage and efficacy are greater than 0, append the far UVC parameters:
  if(simulations_to_run$coverage[i] > 0 | simulations_to_run$efficacy[i] > 0) {
    temp_parameter_list %>%
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

  # Running the model
  temp <- run_simulation(parameters_list = temp_parameter_list)
  temp$ID <- simulations_to_run$ID[i]
  temp$r0 <- simulations_to_run$r0[i]
  temp$coverage <- simulations_to_run$coverage[i]
  temp$efficacy <- simulations_to_run$efficacy[i]
  temp$coverage_type <- simulations_to_run$coverage_type[i]
  temp$beta_community <- simulations_to_run$beta_community[i]
  temp$iteration <- simulations_to_run$iteration[i]
  return(temp)
})

#----- 4) Simulation Post-Processing ---------------------------------------------------------------

# Combine the simulation outputs into a combined data frame:
combined_parameter_sweep_outputs <- data.frame()
for(i in 1:length(results)) {
  combined_parameter_sweep_outputs <- bind_rows(combined_parameter_sweep_outputs, results[[i]])
}

# Convert the dataframe to long form
combined_parameter_sweep_outputs2 <- combined_parameter_sweep_outputs %>%
  mutate(total_count = S_count + E_count + I_count + R_count) %>%
  mutate(S = S_count/total_count,
         E = E_count/total_count,
         I = I_count/total_count,
         R = R_count/total_count)

#----- 5) Visualisation ----------------------------------------------------------------------------


