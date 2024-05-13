#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+++++ Blueprint: Exemplar Model Outputs and the Parameter Sweeps +++++#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

#+++ Brief +++#
#+++++++++++++#
#
# For the exemplar model outputs, pick an R0 e.g. R0 = 2, with a beta split such that you have 30%
# transmission in the household, 20% in schools, 20% in workplaces, 20% in leisure settings, and 10%
# in community.
#
# Run three simulations - one of which is without farUVC, and the other of which is with farUVC,
# installed at a coverage of let's say 50%, and do this for both random and targeted, across all
# structures except households to begin with. Do this for the epidemic scenario setting to begin
# with.
#
# For the parameter sweeps, do:
#       R0 = {1.25, 1.5, 2, 2.5, 3}
#       farUVC coverage do it between 0 and 1, increments of 0.2
#       farUVC efficacy between 0 and 1, increments of 0.2
#       targeted and random coverage types.
#
# In all cases, run 5 replicates
#
# calculate the total number of individuals infected.
#
# Calculate infections averted as total_without_farUVC - total_with_farUVC.
#

#----- 1) Preamble ---------------------------------------------------------------------------------

# Load the finalsize package:
library(finalsize)
library(tidyverse)
library(grid)
library(gridExtra)

# Open a function that automatically generates betas in the requried ratios. The function can take a
# single value, or a vector of values for beta_community and will return the remaining setting-specific
# beta values in the ratio

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

#----- 2) Matching Setting Betas to R0 Using Final Epidemic Size: 3:2:2:2:1 ------------------------

##'
##' This section mapped to R0s via the final_size() function using the following beta ratios:
##'
##' beta_household = 0.3
##' beta_workplace = 0.2
##' beta_school = 0.2
##' beta_leisure = 0.2
##' beta_community = 0.1
##'

# Use generate_betas() to create a dataframe of betas for all settings:
betas <- generate_betas(beta_community = 0.123,
                        household_ratio = 3,
                        school_ratio = 2,
                        workplace_ratio = 2,
                        leisure_ratio = 2)

# Select a beta index to use:
beta_index <- 1

# View the betas associated with the index
betas[beta_index,]

# Generate a list of model parameters:
parameters_list <- get_parameters(overrides = list(
  human_population = 10000,
  beta_household = betas$beta_household[beta_index],
  beta_school = betas$beta_school[beta_index],
  beta_workplace = betas$beta_workplace[beta_index],
  beta_leisure = betas$beta_leisure[beta_index],
  beta_community = betas$beta_community[beta_index],
  endemic_or_epidemic = "epidemic",
  simulation_time = 150
))

# Run the example of the final size function:
R0s <- c(1.25, 1.5, 2, 2.5, 3)

# Set up a vector to store the final epidemic sizes in:
matched_final_sizes <- c()

# Use finalsize to calculate the final size for each of the R0s:
for(i in 1:length(R0s)) {

  # Prepare the contact matrix
  contact_matrix <- matrix(1.0) / parameters_list$human_population

  # Determine what proportion of the population are susceptible:
  initially_susceptible <- (parameters_list$human_population - parameters_list$number_initially_exposed)/
    parameters_list$human_population

  # Store the susceptibility:
  susceptibility <- matrix(1)

  # Store p_susceptibility:
  p_susceptibility <- matrix(1)

  # Generate the final sizes expected to be seen for a given R0:
  matched_final_sizes[i] <- final_size(r0 = R0s[i],
                                       contact_matrix = contact_matrix,
                                       susceptibility = susceptibility,
                                       demography_vector = parameters_list$human_population,
                                       p_susceptibility = p_susceptibility)$p_infected
}

# Create a dataframe of R0s and matched final sizes:
epidemic_sizes <- data.frame(R0 = R0s, size = round(matched_final_sizes, digits = 2))

# Run the simulation:
simulation_output <- run_simulation(parameters_list = parameters_list)

# Calculate the final proportion of individuals in each compartment:
simulation_output %>%
  mutate(S_prop = S_count / parameters_list$human_population) %>%
  mutate(E_prop = E_count / parameters_list$human_population) %>%
  mutate(I_prop = I_count / parameters_list$human_population) %>%
  mutate(R_prop = R_count / parameters_list$human_population) -> simulation_output_processed

# Check the maximum proportion recovered:
max(simulation_output_processed$R_prop)

# Plot the dynamics:
simulation_output %>%
  mutate(S_prop = S_count / parameters_list$human_population,
         E_prop = E_count / parameters_list$human_population,
         I_prop = I_count / parameters_list$human_population,
         R_prop = R_count / parameters_list$human_population) %>%
  pivot_longer(cols = c(S_prop, E_prop, I_prop, R_prop), names_to = "State", values_to = "Proportion") %>%
  ggplot(aes(x = timestep, y = Proportion, colour = State)) + geom_line(linewidth = 1.2) +
  theme_bw() +
  labs(x = "Time", y = "Count", colour = "State") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0))

##'
##' Table of beta_community values matched to the final, actual proportion of infected individuals
##'
##' beta_community      Actual Prop.
##' 0.045               0.3525
##' 0.0455              0.3383
##' 0.0458              0.3425
##' 0.046               0.392
##' 0.05                0.107
##' 0.056               0.5694
##' 0.07                0.7426
##' 0.077               0.7989
##' 0.08                0.8187
##' 0.12                0.9325
##' 0.13                0.9435
##'

##' Betas that match R0s of interest:
##'
##' R0      size      beta_community    Actual Prop.
##' 1.25    0.37      0.046             0.3678
##' 1.5     0.58      0.056             0.5823
##' 2       0.8       0.077             0.7989
##' 2.5     0.89      0.1               0.8937
##' 3       0.94      0.123             0.9394
##'
##'


#----- 4) Exemplar Model Runs: 3:2:2:2:1 -----------------------------------------------------------

#+++ Introduction +++#
#++++++++++++++++++++#
##'
##' For the model exemplar we are simulating a scenario with an R0 of 2 and the following
##' setting-specific transmission coefficients: household (30%), school (20%), workplace (20%),
##' leisure (20%), and community (10%).
##'
##' We run three simulations. The first is a non-intervention baseline. The second simulates the
##' effect of far UVC at a coverage of 50% in all settings, except for households, with far UVC
##' devices distributed randomly in each setting. The third replicates the second scenario, but
##' targets the distribution of far UVC devices to the most populous settings (e.g. the 50% of
##' schools with the most children and teachers).
##'
##' All simulations are run with in the model's endemic setting.
##'
##' Meeting with Charlie
##' - take max of recovered
##' - For same parameter set,run with/without far UVC: - max(recovered_far_vuc) - max(recovered_baseline)
##' - Endemic Stuff: Render Daily Incidence / Render Incidence per timestep (difference between It and It-1)
##'
##'

##' For the exemplar model runs, we want an R0 of 2, for which the corresponding setting-specific
##' betas when the ratios are 3:2:2:2:1 are:
##'
##' beta_household: 0.231
##' beta_school: 0.154
##' beta_workplace: 0.154
##' beta_leisure: 0.154
##' beta_community: 0.077

# Generate the betas required to achieve the target R0 of 2:
exemplar_run_betas <- generate_betas(beta_community = 0.077,
                                     household_ratio = 2,
                                     school_ratio = 2,
                                     workplace_ratio = 2,
                                     leisure_ratio = 1)

# Calculate the simulation_time required to simulate a 2 year period:
years_to_simulate <- 3
simulation_timesteps <- (365 * years_to_simulate)

# Generate the list of model parameters for the baseline, no-intervention run:
parameters_baseline <- get_parameters(overrides = list(
  human_population = 10000,
  beta_household = exemplar_run_betas$beta_household,
  beta_school = exemplar_run_betas$beta_school,
  beta_workplace = exemplar_run_betas$beta_workplace,
  beta_leisure = exemplar_run_betas$beta_leisure,
  beta_community = exemplar_run_betas$beta_community,
  endemic_or_epidemic = "epidemic",
  simulation_time = simulation_timesteps
))

# Set up model parameters for random far UVC coverage type
parameters_baseline %>%
  set_uvc(setting = "school",
          coverage = 0.5,
          coverage_type = "random",
          efficacy = 0.75,
          timestep = 1) %>%
  set_uvc(setting = "workplace",
          coverage = 0.5,
          coverage_type = "random",
          efficacy = 0.75,
          timestep = 1) %>%
  set_uvc(setting = "leisure",
          coverage = 0.5,
          coverage_type = "random",
          efficacy = 0.75,
          timestep = 1) -> parameters_uvc_random

# Set up model parameters for targeted far UVC coverage type
parameters_baseline %>%
  set_uvc(setting = "school",
          coverage = 0.5,
          coverage_type = "targeted",
          efficacy = 0.75,
          timestep = 1) %>%
  set_uvc(setting = "workplace",
          coverage = 0.5,
          coverage_type = "targeted",
          efficacy = 0.75,
          timestep = 1) %>%
  set_uvc(setting = "leisure",
          coverage = 0.5,
          coverage_type = "targeted",
          efficacy = 0.75,
          timestep = 1) -> parameters_uvc_targeted

# Run the baseline, no intervention scenario (approx. 599 seconds):
tictoc::tic()
baseline_output_raw <- run_simulation(parameters_list = parameters_baseline)
tictoc::toc()

# Save the output:
#saveRDS(baseline_output_raw, "C:/Users/trb216/OneDrive - Imperial College London/Documents/Research_Projects/RP4_FarUPV/Code/Blueprint_Milestone_1/Exemplar_data/exemplar_baseline_output_raw.rds")

# Run the randomly assigned far UVC simulation:
tictoc::tic()
random_uvc_output_raw <- run_simulation(parameters_list = parameters_uvc_random)
tictoc::toc()

# Save the output
#saveRDS(random_uvc_output_raw, "C:/Users/trb216/OneDrive - Imperial College London/Documents/Research_Projects/RP4_FarUPV/Code/Blueprint_Milestone_1/Exemplar_data/exemplar_uvc_random_output_raw.rds")

# Run the targeted far UVC simulation:
tictoc::tic()
targeted_uvc_output_raw <- run_simulation(parameters_list = parameters_uvc_targeted)
tictoc::toc()

# Set up the parameter list for the far UVC with randomised coverage:
#saveRDS(targeted_uvc_output_raw, "C:/Users/trb216/OneDrive - Imperial College London/Documents/Research_Projects/RP4_FarUPV/Code/Blueprint_Milestone_1/Exemplar_data/exemplar_uvc_targeted_output_raw.rds")

#----- 6) Exemplar Run Visualisation ---------------------------------------------------------------

# Load the raw outputs for the first version:
# baseline_output_raw <- readRDS("C:/Users/trb216/OneDrive - Imperial College London/Documents/Research_Projects/RP4_FarUPV/Code/Blueprint_Milestone_1/Exemplar_data/exemplar_baseline_output_raw.rds")
# random_uvc_output_raw <- readRDS("C:/Users/trb216/OneDrive - Imperial College London/Documents/Research_Projects/RP4_FarUPV/Code/Blueprint_Milestone_1/Exemplar_data/exemplar_uvc_random_output_raw.rds")
# targeted_uvc_output_raw  <- readRDS("C:/Users/trb216/OneDrive - Imperial College London/Documents/Research_Projects/RP4_FarUPV/Code/Blueprint_Milestone_1/Exemplar_data/exemplar_uvc_targeted_output_raw.rds")
#
# # Load the outputs for the second version:
# baseline_output_raw_2 <- readRDS("C:/Users/trb216/OneDrive - Imperial College London/Documents/Research_Projects/RP4_FarUPV/Code/Blueprint_Milestone_1/Exemplar_data/exemplar_baseline_output_raw_2.rds")
# random_uvc_output_raw_2 <- readRDS("C:/Users/trb216/OneDrive - Imperial College London/Documents/Research_Projects/RP4_FarUPV/Code/Blueprint_Milestone_1/Exemplar_data/exemplar_uvc_random_output_raw_2.rds")
# targeted_uvc_output_raw_2  <- readRDS("C:/Users/trb216/OneDrive - Imperial College London/Documents/Research_Projects/RP4_FarUPV/Code/Blueprint_Milestone_1/Exemplar_data/exemplar_uvc_targeted_output_raw_2.rds")


# Store colours for plotting:
disease_state_colours <- c("#4cd8ff", "#f8ed5b", "brown2", "#a633ff")

# Set a plotting window for the x-axis:
plotting_window <- 900

# Multiplot showing the disease state dynamics in each simulation:
grid.arrange(

  #+++ 3:2:2:2:1 +++#

  # Plot the disease state dynamics in the baseline scenario:
  baseline_output_raw %>%
    filter(timestep <= plotting_window) %>%
    rename("S" = S_count,
           "E" = E_count,
           "I" = I_count,
           "R" = R_count) %>%
    mutate(Total_count = S + E + I + R) %>%
    mutate(S = S / Total_count,
           E = E / Total_count,
           I = I / Total_count,
           R = R / Total_count) %>%
    select(timestep, S, E, I, R) %>%
    pivot_longer(cols = c(S, E, I, R), names_to = "State", values_to = "Proportion") %>%
    mutate(State = factor(State, levels = c("S", "E", "I", "R"))) %>%
    ggplot(aes(x = timestep, y = Proportion, colour = State)) +
    geom_hline(yintercept = max(baseline_output_raw$R_count)/parameters_baseline$human_population,
               linetype = "dashed",
               linewidth = 1) +
    geom_line(linewidth = 1.5) +
    theme_bw() +
    labs(x = "Time", y = "Proportion of Population", colour = "Disease State") +
    scale_colour_discrete(type = disease_state_colours) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    theme(legend.position = "none",
          axis.title.x = element_text(colour = "white")),

  # Plot the disease state dynamics for randomly targeted far UVC:
  random_uvc_output_raw %>%
    filter(timestep <= plotting_window) %>%
    rename("S" = S_count,
           "E" = E_count,
           "I" = I_count,
           "R" = R_count) %>%
    mutate(Total_count = S + E + I + R) %>%
    mutate(S = S / Total_count,
           E = E / Total_count,
           I = I / Total_count,
           R = R / Total_count) %>%
    select(timestep, S, E, I, R) %>%
    pivot_longer(cols = c(S, E, I, R), names_to = "State", values_to = "Proportion") %>%
    mutate(State = factor(State, levels = c("S", "E", "I", "R"))) %>%
    ggplot(aes(x = timestep, y = Proportion, colour = State)) +
    geom_hline(yintercept = max(baseline_output_raw$R_count)/parameters_baseline$human_population,
               linetype = "dashed",
               linewidth = 1) +
    geom_line(linewidth = 1.5) +
    theme_bw() +
    labs(x = "Time", y = "Proportion of Population", colour = "Disease State") +
    scale_colour_discrete(type = disease_state_colours) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    theme(legend.position = "none",
          axis.title.y = element_text(colour = "white")),

  # Plot the disease state dynamics for randomly targeted far UVC:
  targeted_uvc_output_raw %>%
    filter(timestep <= plotting_window) %>%
    rename("S" = S_count,
           "E" = E_count,
           "I" = I_count,
           "R" = R_count) %>%
    mutate(Total_count = S + E + I + R) %>%
    mutate(S = S / Total_count,
           E = E / Total_count,
           I = I / Total_count,
           R = R / Total_count) %>%
    select(timestep, S, E, I, R) %>%
    pivot_longer(cols = c(S, E, I, R), names_to = "State", values_to = "Proportion") %>%
    mutate(State = factor(State, levels = c("S", "E", "I", "R"))) %>%
    ggplot(aes(x = timestep, y = Proportion, colour = State)) +
    geom_hline(yintercept = max(baseline_output_raw$R_count)/parameters_baseline$human_population,
               linetype = "dashed",
               linewidth = 1) +
    geom_line(linewidth = 1.5) +
    theme_bw() +
    labs(x = "Time", y = "Proportion of Population", colour = "Disease State") +
    scale_colour_discrete(type = disease_state_colours) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    theme(legend.position = "none",
          axis.title.y = element_text(colour = "white"),
          axis.title.x = element_text(colour = "white")),

  #+++ 3:3:3:3:1 +++#

  baseline_output_raw_2 %>%
    filter(timestep <= plotting_window) %>%
    rename("S" = S_count,
           "E" = E_count,
           "I" = I_count,
           "R" = R_count) %>%
    mutate(Total_count = S + E + I + R) %>%
    mutate(S = S / Total_count,
           E = E / Total_count,
           I = I / Total_count,
           R = R / Total_count) %>%
    select(timestep, S, E, I, R) %>%
    pivot_longer(cols = c(S, E, I, R), names_to = "State", values_to = "Proportion") %>%
    mutate(State = factor(State, levels = c("S", "E", "I", "R"))) %>%
    ggplot(aes(x = timestep, y = Proportion, colour = State)) +
    geom_hline(yintercept = max(baseline_output_raw_2$R_count)/parameters_baseline_2$human_population,
               linetype = "dashed",
               linewidth = 1) +
    geom_line(linewidth = 1.5) +
    theme_bw() +
    labs(x = "Time", y = "Proportion of Population", colour = "Disease State") +
    scale_colour_discrete(type = disease_state_colours) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    theme(legend.position = "none",
          axis.title.x = element_text(colour = "white")),

  # Plot the disease state dynamics for randomly targeted far UVC:
  random_uvc_output_raw_2 %>%
    filter(timestep <= plotting_window) %>%
    rename("S" = S_count,
           "E" = E_count,
           "I" = I_count,
           "R" = R_count) %>%
    mutate(Total_count = S + E + I + R) %>%
    mutate(S = S / Total_count,
           E = E / Total_count,
           I = I / Total_count,
           R = R / Total_count) %>%
    select(timestep, S, E, I, R) %>%
    pivot_longer(cols = c(S, E, I, R), names_to = "State", values_to = "Proportion") %>%
    mutate(State = factor(State, levels = c("S", "E", "I", "R"))) %>%
    ggplot(aes(x = timestep, y = Proportion, colour = State)) +
    geom_hline(yintercept = max(baseline_output_raw_2$R_count)/parameters_baseline_2$human_population,
               linetype = "dashed",
               linewidth = 1) +
    geom_line(linewidth = 1.5) +
    theme_bw() +
    labs(x = "Time", y = "Proportion of Population", colour = "Disease State") +
    scale_colour_discrete(type = disease_state_colours) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    theme(legend.position = "none",
          axis.title.y = element_text(colour = "white")),

  # Plot the disease state dynamics for randomly targeted far UVC:
  targeted_uvc_output_raw_2 %>%
    filter(timestep <= plotting_window) %>%
    rename("S" = S_count,
           "E" = E_count,
           "I" = I_count,
           "R" = R_count) %>%
    mutate(Total_count = S + E + I + R) %>%
    mutate(S = S / Total_count,
           E = E / Total_count,
           I = I / Total_count,
           R = R / Total_count) %>%
    select(timestep, S, E, I, R) %>%
    pivot_longer(cols = c(S, E, I, R), names_to = "State", values_to = "Proportion") %>%
    mutate(State = factor(State, levels = c("S", "E", "I", "R"))) %>%
    ggplot(aes(x = timestep, y = Proportion, colour = State)) +
    geom_hline(yintercept = max(baseline_output_raw_2$R_count)/parameters_baseline_2$human_population,
               linetype = "dashed",
               linewidth = 1) +
    geom_line(linewidth = 1.5) +
    theme_bw() +
    labs(x = "Time", y = "Proportion of Population", colour = "Disease State") +
    scale_colour_discrete(type = disease_state_colours) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    theme(legend.position = "none",
          axis.title.y = element_text(colour = "white"),
          axis.title.x = element_text(colour = "white")),

  nrow = 2

)

#----- 7) Iterating Random Far-UVC Simulations -----------------------------------------------------

# Generate the betas required to achieve the target R0 of 2:
exemplar_run_betas <- generate_betas(beta_community = c(0.062),
                                     household_ratio = 3,
                                     school_ratio = 3,
                                     workplace_ratio = 3,
                                     leisure_ratio = 3)

# Calculate the simulation_time required to simulate a 2 year period:
years_to_simulate <- 3
simulation_timesteps <- (365 * years_to_simulate)

# Generate the list of model parameters for the baseline, no-intervention run:
get_parameters(overrides = list(
  human_population = 10000,
  beta_household = exemplar_run_betas$beta_household,
  beta_school = exemplar_run_betas$beta_school,
  beta_workplace = exemplar_run_betas$beta_workplace,
  beta_leisure = exemplar_run_betas$beta_leisure,
  beta_community = exemplar_run_betas$beta_community,
  endemic_or_epidemic = "epidemic",
  simulation_time = simulation_timesteps)) %>%
  set_uvc(setting = "school",
          coverage = 0.5,
          coverage_type = "random",
          efficacy = 0.75,
          timestep = 1) %>%
  set_uvc(setting = "workplace",
          coverage = 0.5,
          coverage_type = "random",
          efficacy = 0.75,
          timestep = 1) %>%
  set_uvc(setting = "leisure",
          coverage = 0.5,
          coverage_type = "random",
          efficacy = 0.75,
          timestep = 1) -> parameters_uvc_random_2

# Select a number of iterations to run:
iterations <- 20

# Open a list to store the simulations:
uvc_random_outputs <- list()

# Run the simulations:
for(i in 1:iterations) {
  tictoc::tic()
  uvc_random_outputs[[i]] <- run_simulation(parameters_list = parameters_uvc_random_2)
  tictoc::toc()
}







