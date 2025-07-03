#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+++++ Hipercow Demo +++++#
#+++++++++++++++++++++++++#

##'
##' In this demo scipt we:
##' 1. Configure hipercow
##' 2. Set up some simulations to run in parallel
##' 3. Run the simulations on the DIDE HPC using hipercow
##'

#----- 1) Preamble ---------------------------------------------------------------------------------

# Set your working directory:
setwd("P:/Tom/helios/")

# Install the appropriate branch of the individual package (if required):
#install_github('mrc-ide/individual@feat/logi_size')

# Load helios:
devtools::load_all()

# Load in the requisite packages:
library(hipercow)
library(tidyverse)
library(individual)
library(parallel)

#----- 2) Generate some parameter sets to simulate -------------------------------------------------

# Number of iterations to simulate for each parameterisation:
iterations <- seq(5)

# Calculate the simulation_time required to simulate a 2 year period:
years_to_simulate <- 1
simulation_time_days <- floor(365 * years_to_simulate)

# Specify the timestep on which to switch far_UVC on:
timestep_uvc_on <- 1

# Set the human population size:
human_population <- 10000

# Archetypes to simulate for:
archetypes <- c("flu")

# Riskiness setting:
riskiness <- c("setting_specific_riskiness")

# Set up a vector of far-UVC efficacies to simulate
far_uvc_efficacy <- c(0.8)

# Set up a vector of far-UVC coverages to simulate:
far_uvc_joint_coverage <- c(0.5)

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

    # Append the simulation parameters
    parameter_lists[[i]]$ID <- simulations_to_run$ID[i]
    parameter_lists[[i]]$iteration <- simulations_to_run$iteration[i]
    parameter_lists[[i]]$pathogen <- simulations_to_run$archetype[i]
    parameter_lists[[i]]$years_to_simulate <- years_to_simulate
    parameter_lists[[i]]$timestep_uvc_on <- timestep_uvc_on

  }
}

# Check the number of parameter lists is sensible:
length(parameter_lists)

#----- 3) hipercow Set-up --------------------------------------------------------------------------

## Prepare for cluster use
## see https://mrc-ide.github.io/hipercow/
hipercow::hipercow_init(driver = 'dide-windows')

# Configure hipercow for dide-windows:
hipercow_configure(driver = "dide-windows")

# Check the configuration:
hipercow::hipercow_configuration()

## Provision packages required on the cluster (hipercow looks for provision.R by default)
## see https://mrc-ide.github.io/hipercow/articles/packages.html
hipercow::hipercow_provision()

#----- 5) Run simulations on cluster using hipercow ------------------------------------------------

# Create the environment for hipercow
hipercow::hipercow_environment_create(packages = c("individual",
                                                   "helios",
                                                   "tidyverse",
                                                   "dqrng",
                                                   "parallel",
                                                   "EnvStats"),
                                      sources = "./inst/hipercow_demo/run_simulation_hipercow.R")

# Run the simulations using the hipercow function task_create_expr()
# https://mrc-ide.github.io/hipercow/reference/task_create_expr.html
task_id <- hipercow::task_create_expr(
  expr = parallel::clusterApply(
    NULL,
    parameter_lists,
    function(p) run_simulation_hipercow(p, file_save = TRUE)
  ),
  parallel = hipercow::hipercow_parallel("parallel"),
  resources = hipercow::hipercow_resources(cores = 10))

# Use this to track the status of your job(s):
x <- sapply(task_id, hipercow::task_status); table(x)

# View the job logs:
hipercow::task_log_show(task_id)

# View the job result:
outputs <- hipercow::task_result(task_id)

# Example plot from single simulation:
outputs[[1]][[2]] |>
  tidyr::pivot_longer(cols = c(S_count, E_count, I_count, R_count),
                      names_to = "State",
                      values_to = "Population") |>
  ggplot(aes(x = timestep, y = Population, colour = State)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(x = "Time", y = "Population", colour = "Disease State")

# Combine the ouputs into a single dataframe:
simulations_df <- data.frame()
for(i in 1:length(outputs)) {
  simulations_df <- dplyr::bind_rows(simulations_df, outputs[[i]][[2]])
}

# Plot the incidence of new cases through time:
simulations_df |>
  ggplot(aes(x = timestep,
             y = E_new,
             colour = coverage_type,
             group = as.factor(iteration))) +
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  labs(x = "Time", y = "Incidence", colour = "Coverage Type") +
  facet_grid(~iteration)











