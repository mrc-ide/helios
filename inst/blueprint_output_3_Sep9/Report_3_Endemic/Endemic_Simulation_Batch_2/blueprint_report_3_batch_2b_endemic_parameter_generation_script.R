#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+++++ Blueprint Report 3.a: Far UVC under Endemic Pathogens +++++#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

#+++ Introduction +++#
#++++++++++++++++++++#



#----- 1) Preamble ---------------------------------------------------------------------------------

# Load in the requisite packages:
library(helios)
library(tidyverse)
library(tictoc)
library(individual)

#----- 2) Parameter Sweep Set-Up -------------------------------------------------------------------

# Number of iterations to simulate for each parameterisation:
iterations <- seq(10)

# Calculate the simulation_time required to simulate a 2 year period:
years_to_simulate <- 24
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
  mutate(scenario = "endemic") |>
  arrange(archetype, coverage_type, coverage, efficacy, iteration) |>
  mutate(ID = 1:nrow(simulations_to_run)) -> simulations_to_run

# View the simulations_to_run dataframe:
nrow(simulations_to_run)

# Set up a list to store the parameter_lists:
parameter_lists <- list()

# Set up the simulation parameter lists for each row of the simulations_to_run dataframe:
for(i in 1:nrow(simulations_to_run)) {

  #+++ SARS-CoV-2 +++#
  #++++++++++++++++++#
  if (simulations_to_run$archetype[i] == "sars_cov_2") {

    ## Setting up initial conditions (approx endemic equilibrium solution for R0 2.5 pathogen)
    initial_S_SC2 <- round(0.4 * human_population)
    initial_E_SC2 <- round(0.01 * human_population)
    initial_I_SC2 <- round(0.02 * human_population)
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
                                             endemic_or_epidemic = "endemic",
                                             duration_immune = duration_of_immunity,
                                             prob_inf_external = external_infection_probability,

                                             # Specify the simulation duration
                                             simulation_time = simulation_time_days))

  } else if (simulations_to_run$archetype[i] == "flu") {

    #+++ Flu +++#
    #+++++++++++#
    # Setting up initial conditions (approx endemic equilibrium solution for R0 1.5 pathogen)
    initial_S_flu <- round(0.67 * human_population)
    initial_E_flu <- round(0.006 * human_population)
    initial_I_flu <- round(0.012 * human_population)
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
                                             endemic_or_epidemic = "endemic",
                                             duration_immune = duration_of_immunity,
                                             prob_inf_external = external_infection_probability,

                                             # Specify the simulation duration
                                             simulation_time = simulation_time_days))

  } else {

    # If the archetype is neither fly nor SARS-CoV-2 then halt the parameter generation process:
    stop("something's gone wrong withspecifying archetype")

  }

  # Calculate the timestep on which to switch Far UVC on:
  timestep_uvc_on <- round(((years_to_simulate - 2) * 365) / parameter_lists[[i]]$dt)

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

    # Append the simulation parameters
    parameter_lists[[i]]$ID <- simulations_to_run$ID[i]
    parameter_lists[[i]]$iteration <- simulations_to_run$iteration[i]
    parameter_lists[[i]]$pathogen <- simulations_to_run$archetype[i]

  }
}

# Save the parameter lists and the simulations dataframe:
saveRDS(simulations_to_run, file = "endemic_simulations_table_batch_2_b.rds")
saveRDS(parameter_lists, file = "endemic_simulations_parameter_lists_batch_2_b.rds")

#----- 3) Batch Saving -----------------------------------------------------------------------------

##' I'm going to batch these simulations up into groups of 64 simulations so that each core runs two
##' run in parallel.

# Calculate the number of simulations
num_sims <- length(parameter_lists)

# View the upper and lower bounds of the parameter batches:
lower_bounds <- c(1, seq(1, 20, 1) * 64 + 1)
upper_bounds <- seq(1, 20, 1) * 64

# Manually save the parameter lists into 21 batches (20 full, 1 remainder)
endemic_simulation_parameter_list_1_64 <- parameter_lists[1:64]
endemic_simulation_parameter_list_65_128 <- parameter_lists[65:128]
endemic_simulation_parameter_list_129_192 <- parameter_lists[129:192]
endemic_simulation_parameter_list_193_256 <- parameter_lists[193:256]
endemic_simulation_parameter_list_257_320 <- parameter_lists[257:320]
endemic_simulation_parameter_list_321_384 <- parameter_lists[321:384]
endemic_simulation_parameter_list_385_448 <- parameter_lists[385:448]
endemic_simulation_parameter_list_449_512 <- parameter_lists[449:512]
endemic_simulation_parameter_list_513_576 <- parameter_lists[513:576]
endemic_simulation_parameter_list_577_640 <- parameter_lists[577:640]
endemic_simulation_parameter_list_641_704 <- parameter_lists[641:704]
endemic_simulation_parameter_list_705_768 <- parameter_lists[705:768]
endemic_simulation_parameter_list_769_832 <- parameter_lists[769:832]
endemic_simulation_parameter_list_833_896 <- parameter_lists[833:896]
endemic_simulation_parameter_list_897_960 <- parameter_lists[897:960]
endemic_simulation_parameter_list_961_1024 <- parameter_lists[961:1024]
endemic_simulation_parameter_list_1025_1088 <- parameter_lists[1025:1088]
endemic_simulation_parameter_list_1089_1152 <- parameter_lists[1089:1152]
endemic_simulation_parameter_list_1153_1216 <- parameter_lists[1153:1216]
endemic_simulation_parameter_list_1217_1280 <- parameter_lists[1217:1280]
endemic_simulation_parameter_list_1281_1320 <- parameter_lists[1281:1320]

# Save the independent simulation lists:
saveRDS(object = endemic_simulation_parameter_list_1_64, file = "endemic_simulation_parameter_list_1_64.rds")
saveRDS(object = endemic_simulation_parameter_list_65_128, file = "endemic_simulation_parameter_list_65_128.rds")
saveRDS(object = endemic_simulation_parameter_list_129_192, file = "endemic_simulation_parameter_list_129_192.rds")
saveRDS(object = endemic_simulation_parameter_list_193_256, file = "endemic_simulation_parameter_list_193_256.rds")
saveRDS(object = endemic_simulation_parameter_list_257_320, file = "endemic_simulation_parameter_list_257_320.rds")
saveRDS(object = endemic_simulation_parameter_list_321_384, file = "endemic_simulation_parameter_list_321_384.rds")
saveRDS(object = endemic_simulation_parameter_list_385_448, file = "endemic_simulation_parameter_list_385_448.rds")
saveRDS(object = endemic_simulation_parameter_list_449_512, file = "endemic_simulation_parameter_list_449_512.rds")
saveRDS(object = endemic_simulation_parameter_list_513_576, file = "endemic_simulation_parameter_list_513_576.rds")
saveRDS(object = endemic_simulation_parameter_list_577_640, file = "endemic_simulation_parameter_list_577_640.rds")
saveRDS(object = endemic_simulation_parameter_list_641_704, file = "endemic_simulation_parameter_list_641_704.rds")
saveRDS(object = endemic_simulation_parameter_list_705_768, file = "endemic_simulation_parameter_list_705_768.rds")
saveRDS(object = endemic_simulation_parameter_list_769_832, file = "endemic_simulation_parameter_list_769_832.rds")
saveRDS(object = endemic_simulation_parameter_list_833_896, file = "endemic_simulation_parameter_list_833_896.rds")
saveRDS(object = endemic_simulation_parameter_list_897_960, file = "endemic_simulation_parameter_list_897_960.rds")
saveRDS(object = endemic_simulation_parameter_list_961_1024, file = "endemic_simulation_parameter_list_961_1024.rds")
saveRDS(object = endemic_simulation_parameter_list_1025_1088, file = "endemic_simulation_parameter_list_1025_1088.rds")
saveRDS(object = endemic_simulation_parameter_list_1089_1152, file = "endemic_simulation_parameter_list_1089_1152.rds")
saveRDS(object = endemic_simulation_parameter_list_1153_1216, file = "endemic_simulation_parameter_list_1153_1216.rds")
saveRDS(object = endemic_simulation_parameter_list_1217_1280, file = "endemic_simulation_parameter_list_1217_1280.rds")
saveRDS(object = endemic_simulation_parameter_list_1281_1320, file = "endemic_simulation_parameter_list_1281_1320.rds")

#----- 4) Charlie's Sanity Checks ------------------------------------------------------------------

x <- results1[[1]]
indices_prev_UVC <- round(((years_to_simulate - 4) * 365) / parameter_lists[[i]]$dt) : round(((years_to_simulate - 2) * 365) / parameter_lists[[i]]$dt)
indices_post_UVC <- round(((years_to_simulate - 2) * 365) / parameter_lists[[i]]$dt):(simulation_time_days / parameter_lists[[i]]$dt)

proc_outputs <- lapply(results1, function(x) {
  df <- data.frame(ID = unique(x$ID),
                   avg_pre_UVC = sum(x$E_new[indices_prev_UVC]) / 3,
                   avg_post_UVC = sum(x$E_new[indices_post_UVC]) / 3,
                   avg_prev_pre_UVC = mean(x$I_count[indices_prev_UVC]),
                   avg_prev_post_UVC = mean(x$I_count[indices_post_UVC])) %>%
    mutate(reduction_incidence = 1 - avg_post_UVC / avg_pre_UVC) %>%
    mutate(reduction_prevalence = 1 - avg_prev_post_UVC / avg_prev_pre_UVC)
})
proc_outputs2 <- bind_rows(proc_outputs)
proc_outputs3 <- proc_outputs2 %>%
  left_join(simulations_to_run, by = "ID") %>%
  group_by(coverage, coverage_type, archetype, efficacy)

head(proc_outputs3)
ggplot(proc_outputs3, aes(x = coverage, y = reduction_incidence, colour = coverage_type)) +
  geom_line() +
  facet_grid(archetype ~ efficacy)
ggplot(proc_outputs3, aes(x = coverage, y = reduction_prevalence, colour = coverage_type)) +
  geom_line() +
  facet_grid(archetype ~ efficacy)


num_cores <- 30
tic()
results1 <- mclapply(1:length(parameter_lists), mc.cores = num_cores, function(i) {
  temp <- run_simulation(parameters_list = parameter_lists[[i]])
  temp$ID <- simulations_to_run$ID[i]
  return(temp)
})
toc()
Sys.sleep(45)
saveRDS(object = results1, file = "./inst/blueprint_output_3_Sep9/Report_3_Epidemic/Report3_EpidemicSimulation_Outputs/full_epidemic_outputs.rds")
Sys.sleep(15)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
