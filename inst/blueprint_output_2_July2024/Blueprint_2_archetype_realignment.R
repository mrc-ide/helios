#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+++++ Blueprint Report 2: Archetype Re-Alignment +++++#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++#

#+++ README +++#

##' In this script, we check that the beta values estimated to produce R0's reflective of the pathogen
##' archetypes we calculated prior to the USA data integration development. Specifically, we check that
##' the beta values for influenza, SARS-CoV-2 and measles.

#' The original parameter sets for the archetypes are as follows:
#'
#' Parameter              Influenza       SARS-CoV-2      Measles
#' duration_exposed           1               2               8
#' duration_infectious        2               4               5
#' beta_household             0.132           0.24            1.26
#' beta_workplace             0.132           0.24            1.26
#' beta_school                0.132           0.24            1.26
#' beta_leisure               0.132           0.24            1.26
#' beta_community             0.044           0.08            0.42
#' R0                         1.5             2.5             9
#' Final Size                 0.583           0.893           0.999
#'
#' Where the beta_community value is used to generate the rest of the location-specific betas in a
#' ratio of 3:3:3:3:1 (using the generate_betas() function).
#'

#----- 1) Preamble ---------------------------------------------------------------------------------

# Load the finalsize package:
library(finalsize)
library(tidyverse)

#----- 2) Determine the final sizes ------------------------------------------------------------------------

# Load the base set of parameters:
parameters_list <- parameters_list <- get_parameters(overrides = list(
  human_population = 10000,
  endemic_or_epidemic = "epidemic",
  simulation_time = 500
))

# Run the example of the final size function:
R0s <- c(1.5, 2.5, 9)

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

# The final sizes to match:
matched_final_sizes

#----- 3) Influenza Testing ------------------------------------------------------------------------

# Use generate_betas() to create a dataframe of betas for all settings:
betas <- generate_betas(beta_community = 0.044,
                        household_ratio = 3,
                        school_ratio = 3,
                        workplace_ratio = 3,
                        leisure_ratio = 3)

# Generate a list of model parameters:
parameters_list <- get_parameters(overrides = list(
  human_population = 10000,
  beta_household = betas$beta_household,
  beta_school = betas$beta_school,
  beta_workplace = betas$beta_workplace,
  beta_leisure = betas$beta_leisure,
  beta_community = betas$beta_community,
  endemic_or_epidemic = "epidemic",
  simulation_time = 225
))

# Run the simulation:
simulation_output <- run_simulation(parameters_list = parameters_list); beep(1)

# Check the final size:
(simulation_output$R_count / 10000)[390:400]

# Calculate the final proportion of individuals in each compartment:
simulation_output %>%
  mutate(S_prop = S_count / parameters_list$human_population) %>%
  mutate(E_prop = E_count / parameters_list$human_population) %>%
  mutate(I_prop = I_count / parameters_list$human_population) %>%
  mutate(R_prop = R_count / parameters_list$human_population) -> simulation_output_processed

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
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  geom_hline(yintercept = 0.583, linetype = "dashed")

##' This generates an epideic with a final size of 0.5829.

#----- 4) SARS-CoV-2 Testing -----------------------------------------------------------------------

# Use generate_betas() to create a dataframe of betas for all settings:
betas <- generate_betas(beta_community = 0.08,
                        household_ratio = 3,
                        school_ratio = 3,
                        workplace_ratio = 3,
                        leisure_ratio = 3)

# Generate a list of model parameters:
parameters_list <- get_parameters(overrides = list(
  human_population = 10000,
  beta_household = betas$beta_household,
  beta_school = betas$beta_school,
  beta_workplace = betas$beta_workplace,
  beta_leisure = betas$beta_leisure,
  beta_community = betas$beta_community,
  endemic_or_epidemic = "epidemic",
  simulation_time = 200
))

# Run the simulation:
simulation_output <- run_simulation(parameters_list = parameters_list); beep(1)

# Check the final size:
(simulation_output$R_count / 10000)[390:400]

# Calculate the final proportion of individuals in each compartment:
simulation_output %>%
  mutate(S_prop = S_count / parameters_list$human_population) %>%
  mutate(E_prop = E_count / parameters_list$human_population) %>%
  mutate(I_prop = I_count / parameters_list$human_population) %>%
  mutate(R_prop = R_count / parameters_list$human_population) -> simulation_output_processed

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
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  geom_hline(yintercept = 0.893, linetype = "dashed")

##' This generates an epideic with a final size of 0.8947.

#----- 5) Measles Testing --------------------------------------------------------------------------

# Use generate_betas() to create a dataframe of betas for all settings:
betas <- generate_betas(beta_community = 0.42,
                        household_ratio = 3,
                        school_ratio = 3,
                        workplace_ratio = 3,
                        leisure_ratio = 3)

# Generate a list of model parameters:
parameters_list <- get_parameters(overrides = list(
  human_population = 10000,
  beta_household = betas$beta_household,
  beta_school = betas$beta_school,
  beta_workplace = betas$beta_workplace,
  beta_leisure = betas$beta_leisure,
  beta_community = betas$beta_community,
  endemic_or_epidemic = "epidemic",
  simulation_time = 200
))

# Run the simulation:
simulation_output <- run_simulation(parameters_list = parameters_list); beep(1)

# Check the final size:
(simulation_output$R_count / 10000)[390:400]

# Calculate the final proportion of individuals in each compartment:
simulation_output %>%
  mutate(S_prop = S_count / parameters_list$human_population) %>%
  mutate(E_prop = E_count / parameters_list$human_population) %>%
  mutate(I_prop = I_count / parameters_list$human_population) %>%
  mutate(R_prop = R_count / parameters_list$human_population) -> simulation_output_processed

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
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  geom_hline(yintercept = 0.999, linetype = "dashed")

##' This generates an epidemic with a final size of
