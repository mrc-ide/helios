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
library(helios)
# library(ggplot2)
# library(tidyverse)
# library(grid)
# library(gridExtra)
library(parallel)
library(tictoc)

#----- 2) Parameter Sweep Set-Up -------------------------------------------------------------------

# Calculate the simulation_time required to simulate a 2 year period:
years_to_simulate <- 10
simulation_time_days <- (365 * years_to_simulate)

# Set the human population size:
human_population <- 100000

# Specify a duration of immunity following infection:
duration_of_immunity <- 365

# Set a probability of infection from an external source (1 person per timestep on average):
external_infection_probability <- 1 / human_population

# Archetypes to simulate for:
archetypes <- c("flu", "sars_cov_2")

# Riskiness setting:
# riskiness <- c("uniform_riskiness", "setting_specific_riskiness")
riskiness <- c("setting_specific_riskiness")

# Set up a vector of far-UVC coverages to simulate:
far_uvc_coverage <- c(0.1, 0.25, 0.5)

# Set up a vector of far-UVC efficacies to simulate
far_uvc_efficacy <- seq(0.4, 0.8, 0.2)

# Number of iterations to simulate for each parameterisation:
iterations <- 1:10

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
  if (simulations_to_run$archetype[i] == "sars_cov_2") {

    ## Setting up initial conditions (approx endemic equilibrium solution for R0 2.5 pathogen)
    initial_S_SC2 <- round(0.4 * human_population)
    initial_E_SC2 <- round(0.01 * human_population)
    initial_I_SC2 <- round(0.02 * human_population)
    initial_R_SC2 <- human_population - initial_S_SC2 - initial_E_SC2 - initial_I_SC2
    parameter_lists[[i]] <- get_parameters(archetype = simulations_to_run$archetype[i],
                                           overrides = list(
                                             human_population = human_population,
                                             number_initial_S = initial_S_SC2,
                                             number_initial_E = initial_E_SC2,
                                             number_initial_I = initial_I_SC2,
                                             number_initial_R = initial_R_SC2,
                                             endemic_or_epidemic = "endemic",
                                             duration_immune = duration_of_immunity,
                                             prob_inf_external = external_infection_probability,
                                             simulation_time = simulation_time_days))

  } else if (simulations_to_run$archetype[i] == "flu") {

    ## Setting up initial conditions (approx endemic equilibrium solution for R0 1.5 pathogen)
    initial_S_flu <- round(0.67 * human_population)
    initial_E_flu <- round(0.006 * human_population)
    initial_I_flu <- round(0.012 * human_population)
    initial_R_flu <- human_population - initial_S_flu - initial_E_flu - initial_I_flu
    parameter_lists[[i]] <- get_parameters(archetype = simulations_to_run$archetype[i],
                                           overrides = list(
                                             human_population = human_population,
                                             number_initial_S = initial_S_flu,
                                             number_initial_E = initial_E_flu,
                                             number_initial_I = initial_I_flu,
                                             number_initial_R = initial_R_flu,
                                             endemic_or_epidemic = "endemic",
                                             duration_immune = duration_of_immunity,
                                             prob_inf_external = external_infection_probability,
                                             simulation_time = simulation_time_days))
  } else {
    stop("something's gone wrong withspecifying archetype")
  }



  # If coverage is greater than 0 append the far UVC parameters:
  timestep_uvc_on <- round(((years_to_simulate - 2) * 365) / parameter_lists[[i]]$dt) # note this is in timesteps not days
  if(simulations_to_run$coverage[i] > 0) {
    parameter_lists[[i]] |>
      set_uvc(setting = "school",
              coverage = simulations_to_run$coverage[i],
              coverage_target = "buildings",
              coverage_type = "random",
              efficacy = simulations_to_run$efficacy[i],
              timestep = timestep_uvc_on) |>
      set_uvc(setting = "workplace",
              coverage = simulations_to_run$coverage[i],
              coverage_target = "buildings",
              coverage_type = "random",
              efficacy = simulations_to_run$efficacy[i],
              timestep = timestep_uvc_on) |>
      set_uvc(setting = "leisure",
              coverage = simulations_to_run$coverage[i],
              coverage_target = "buildings",
              coverage_type = "random",
              efficacy = simulations_to_run$efficacy[i],
              timestep = timestep_uvc_on) -> parameter_lists[[i]]
  }

  # If setting-specific-riskiness is parameterised, append the setting-specific riskiness parameters:
  if(simulations_to_run$riskiness[i] == "setting_specific_riskiness") {
    parameter_lists[[i]] |>
      set_setting_specific_riskiness(setting = "school",
                                     mean = 0,
                                     sd = 0.3544,
                                     min = 1/sqrt(4.75),
                                     max = sqrt(4.75)) |>
      set_setting_specific_riskiness(setting = "workplace",
                                     mean = 0,
                                     sd = 0.5072,
                                     min = 1/sqrt(6.35),
                                     max = sqrt(6.35)) |>
      set_setting_specific_riskiness(setting = "household",
                                     mean = 0,
                                     sd = 0.0871,
                                     min = 1/sqrt(2.5),
                                     max = sqrt(2.5)) |>
      set_setting_specific_riskiness(setting = "leisure",
                                     mean = 0,
                                     sd = 0.4278,
                                     min = 1/sqrt(5.5),
                                     max = sqrt(5.5)) -> parameter_lists[[i]]
  }
}

saveRDS(simulations_to_run, file = "./inst/blueprint_output_2_July2024/simulations_to_run.rds")
saveRDS(parameter_lists, file = "./inst/blueprint_output_2_July2024/parameter_lists.rds")

#----- 3) Simulation Runs --------------------------------------------------------------------------

# Set up a list to store the simulation_outputs:
# tic()
# simulation_outputs <- list()
#
# # Run through the simulations in simulations_to_run:
# for(i in 1:length(parameter_lists)) {
#   simulation_outputs[[i]] <- run_simulation(parameters_list = parameter_lists[[i]])
#   simulation_outputs[[i]]$ID <- simulations_to_run$ID[i]
#   simulation_outputs[[i]]$riskiness_setting <- simulations_to_run$riskiness[i]
#   simulation_outputs[[i]]$archetype <- simulations_to_run$archetype[i]
#   simulation_outputs[[i]]$coverage <- simulations_to_run$coverage[i]
#   simulation_outputs[[i]]$efficacy <- simulations_to_run$efficacy[i]
#   simulation_outputs[[i]]$iteration <- simulations_to_run$iteration[i]
#   # print(paste0(i, "th simulation complete (", (i/length(parameter_lists))*100, "% complete)"))
# }
# toc()

# Set up infrastructure to run in parallel
# num_cores <- 40
# cl <- makeCluster(num_cores)
# clusterExport(cl, list("simulations_to_run", "simulation_timesteps", "parameter_lists"))
# clusterEvalQ(cl, library(helios))
#
# tic()
# # Run through the simulations in simulations_to_run:
# results <- parLapply(cl, 1:nrow(simulations_to_run), function(i) {
#
#   # Running the model
#   temp <- run_simulation(parameters_list = parameter_lists[[i]])
#   temp$ID <- simulations_to_run$ID[i]
#   temp$riskiness_setting <- simulations_to_run$riskiness[i]
#   temp$archetype <- simulations_to_run$archetype[i]
#   temp$coverage <- simulations_to_run$coverage[i]
#   temp$efficacy <- simulations_to_run$efficacy[i]
#   temp$iteration <- simulations_to_run$iteration[i]
#
#   return(temp)
# })
# parallel::stopCluster(cl)
# toc()

num_cores <- 40
tic()
results1 <- mclapply(1:40, mc.cores = num_cores, function(i) {
  temp <- run_simulation(parameters_list = parameter_lists[[i]])
  temp$ID <- simulations_to_run$ID[i]
  return(temp)
})
toc()
Sys.sleep(45)
saveRDS(object = results1, file = "./inst/blueprint_output_2_July2024/raw_outputs_results1.rds")
Sys.sleep(15)

tic()
results2 <- mclapply(41:80, mc.cores = num_cores, function(i) {
  temp <- run_simulation(parameters_list = parameter_lists[[i]])
  temp$ID <- simulations_to_run$ID[i]
  return(temp)
})
toc()
Sys.sleep(45)
saveRDS(object = results1, file = "./inst/blueprint_output_2_July2024/raw_outputs_results2.rds")
Sys.sleep(15)

tic()
results3 <- mclapply(81:90, mc.cores = num_cores, function(i) {
  temp <- run_simulation(parameters_list = parameter_lists[[i]])
  temp$ID <- simulations_to_run$ID[i]
  return(temp)
})
toc()
Sys.sleep(45)
saveRDS(object = results1, file = "./inst/blueprint_output_2_July2024/raw_outputs_results3.rds")
Sys.sleep(15)

## make sure we also save and return the dt and years to simulate and when UVC is started somehow

results[[1]]
x <- results[[2]]
plot(x$timestep, x$S_count, type = "l", ylim = c(0, human_population))
lines(x$timestep, x$E_count, col = "red")
lines(x$timestep, x$I_count, col = "green")
lines(x$timestep, x$R_count, col = "purple")

## do a left join here

# Save the simulation outputs:
#saveRDS(object = simulation_outputs, file = "./inst/blueprint_output_2_July2024/example_raw_sim_outputs.rds")

#----- 4) Simulation Post-Processing ---------------------------------------------------------------

# Combine the simulation outputs into a combined data frame:
combined_parameter_sweep_outputs <- data.frame()
for(i in 1:length(simulation_outputs)) {
  combined_parameter_sweep_outputs <- dplyr::bind_rows(combined_parameter_sweep_outputs, results[[i]])
}

# Convert the dataframe to long form
combined_parameter_sweep_outputs %>%
  mutate(total_count = S_count + E_count + I_count + R_count) %>%
  mutate(S = S_count/total_count,
         E = E_count/total_count,
         I = I_count/total_count,
         R = R_count/total_count) %>%
  pivot_longer(cols = c(S, E, I, R),
               names_to = "Disease_State",
               values_to = "Proportion") -> combined_parameter_sweep_outputs_long

# Save the long-form dataframe:
#saveRDS(object = combined_parameter_sweep_outputs_long, file = "./inst/blueprint_output_2_July2024/example_longform_output.rds")

#----- 5) Visualisation ----------------------------------------------------------------------------
