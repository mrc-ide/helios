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

#----- 2) Parameterise the endemic model exemplar runs for the 3:3:3:3:1 beta ratios ---------------

##'
##' We have previously used the finalsize::final_size() function to work out which beta_community value
##' we need, given a ratio of 3:3:3:3:beta_community for the remaining setting-specific betas, to
##' achieve the following R0s of interest:
##'
##' R0      size      beta_community    Actual Prop.  Approx. time to Eq
##' 1.25    0.37      0.36              0.3607        t = 700
##' 1.5     0.58      0.044             0.588         t = 500
##' 2       0.8       0.062             0.8001        t = 250
##' 2.5     0.89      0.08              0.893         t = 200
##' 3       0.94      0.097             0.9415        t = 175
##'
##' For the exemplar model runs, we are matching our beta parameters to simulate a pathogen with an
##' R0 of approximately 2, which yields the following setting-specific beta values:
##'
##' beta_household: 0.186
##' beta_school: 0.186
##' beta_workplace: 0.186
##' beta_leisure: 0.186
##' beta_community: 0.062
##'

# Generate the betas required to achieve the target R0 of 2:
exemplar_run_betas <- generate_betas(beta_community = 0.062,
                                     household_ratio = 3,
                                     school_ratio = 3,
                                     workplace_ratio = 3,
                                     leisure_ratio = 3)

# Calculate the simulation_time required to simulate a 2 year period: 5 years took about 19 minutes
years_to_simulate <- 10
simulation_timesteps <- (365 * years_to_simulate)

# Specify the duration of immunity:
#duration_of_immunity <- (3 * 30)/0.5
duration_of_immunity <- 14

# Set the intervention time step:
uvc_timestep <- 500

# Generate the list of model parameters for the baseline, no-intervention run:
parameters_endemic_exemplar_baseline <- get_parameters(overrides = list(
  human_population = 10000,
  beta_household = exemplar_run_betas$beta_household,
  beta_school = exemplar_run_betas$beta_school,
  beta_workplace = exemplar_run_betas$beta_workplace,
  beta_leisure = exemplar_run_betas$beta_leisure,
  beta_community = exemplar_run_betas$beta_community,
  endemic_or_epidemic = "endemic",
  duration_immune = duration_of_immunity,
  simulation_time = simulation_timesteps
))

# Set up model parameters for random far UVC coverage type
parameters_endemic_exemplar_baseline %>%
  set_uvc(setting = "school",
          coverage = 0.5,
          coverage_type = "random",
          efficacy = 0.75,
          timestep = uvc_timestep) %>%
  set_uvc(setting = "workplace",
          coverage = 0.5,
          coverage_type = "random",
          efficacy = 0.75,
          timestep = uvc_timestep) %>%
  set_uvc(setting = "leisure",
          coverage = 0.5,
          coverage_type = "random",
          efficacy = 0.75,
          timestep = uvc_timestep) -> parameters_endemic_exemplar_uvc_random

# Set up model parameters for targeted far UVC coverage type
parameters_endemic_exemplar_baseline %>%
  set_uvc(setting = "school",
          coverage = 0.5,
          coverage_type = "targeted",
          efficacy = 0.75,
          timestep = uvc_timestep) %>%
  set_uvc(setting = "workplace",
          coverage = 0.5,
          coverage_type = "targeted",
          efficacy = 0.75,
          timestep = uvc_timestep) %>%
  set_uvc(setting = "leisure",
          coverage = 0.5,
          coverage_type = "targeted",
          efficacy = 0.75,
          timestep = uvc_timestep) -> parameters_endemic_exemplar_uvc_targeted

#----- 3) Simulations ------------------------------------------------------------------------------

# Run the baseline, no intervention scenario (approx. 599 seconds):
tictoc::tic()
endemic_baseline_output_raw <- run_simulation(parameters_list = parameters_endemic_exemplar_baseline)
tictoc::toc()

# Save the output:
saveRDS(endemic_baseline_output_raw, "C:/Users/trb216/OneDrive - Imperial College London/Desktop/temp_endemic_results/endemic_baseline_2_week_immunity_10_years.rds")

# Run the randomly assigned far UVC simulation:
tictoc::tic()
endemic_random_uvc_output_raw <- run_simulation(parameters_list = parameters_endemic_exemplar_uvc_random)
tictoc::toc()

# Save the output
saveRDS(endemic_random_uvc_output_raw, "C:/Users/trb216/OneDrive - Imperial College London/Desktop/temp_endemic_results/endemic_random_uvc_2_week_immunity_10_years.rds")
#
# # Run the targeted far UVC simulation:
tictoc::tic()
endemic_targeted_uvc_output_raw <- run_simulation(parameters_list = parameters_endemic_exemplar_uvc_targeted)
tictoc::toc()

# # Set up the parameter list for the far UVC with randomised coverage:
saveRDS(endemic_targeted_uvc_output_raw, "C:/Users/trb216/OneDrive - Imperial College London/Desktop/temp_endemic_results/endemic_targeted_uvc_2_week_immunity_10_years.rds")

#----- 4) Exemplar Run Visualisation 1: Individual SEIR Plots --------------------------------------

# Load the simulation outputs:
endemic_baseline_output_raw <- readRDS("C:/Users/trb216/OneDrive - Imperial College London/Desktop/temp_endemic_results/endemic_baseline_2_week_immunity_10_years.rds")
endemic_random_uvc_output_raw
endemic_targeted_uvc_output_raw

# Store colours for plotting:
disease_state_colours <- c("#4cd8ff", "#f8ed5b", "brown2", "#a633ff")

# Set a plotting window for the x-axis:
plotting_window <- 900

# Plot the dynamics in the endemic baseline simulation:
endemic_baseline_output_raw %>%
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
  filter(timestep <= 1500) %>%
  pivot_longer(cols = c(S, E, I, R), names_to = "State", values_to = "Proportion") %>%
  mutate(State = factor(State, levels = c("S", "E", "I", "R"))) %>%
  ggplot(aes(x = timestep, y = Proportion, colour = State)) +
  geom_hline(yintercept = max(endemic_baseline_output_raw$R_count)/parameters_endemic_exemplar_baseline$human_population,
             linetype = "dashed",
             linewidth = 1) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  labs(x = "Time", y = "Proportion of Population", colour = "Disease State") +
  scale_colour_discrete(type = disease_state_colours) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_vline(xintercept = 300)

# Plot the dynamics in the endemic random far-UVC simulation:
endemic_random_uvc_output_raw %>%
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
  filter(timestep <= 1500) %>%
  pivot_longer(cols = c(S, E, I, R), names_to = "State", values_to = "Proportion") %>%
  mutate(State = factor(State, levels = c("S", "E", "I", "R"))) %>%
  ggplot(aes(x = timestep, y = Proportion, colour = State)) +
  geom_hline(yintercept = max(endemic_baseline_output_raw$R_count)/parameters_endemic_exemplar_baseline$human_population,
             linetype = "dashed",
             linewidth = 1) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  labs(x = "Time", y = "Proportion of Population", colour = "Disease State") +
  scale_colour_discrete(type = disease_state_colours) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_vline(xintercept = uvc_timestep)

# Plot the dynamics in the endemic targeted far-UVC simulation:
endemic_targeted_uvc_output_raw %>%
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
  filter(timestep <= 1500) %>%
  pivot_longer(cols = c(S, E, I, R), names_to = "State", values_to = "Proportion") %>%
  mutate(State = factor(State, levels = c("S", "E", "I", "R"))) %>%
  ggplot(aes(x = timestep, y = Proportion, colour = State)) +
  geom_hline(yintercept = max(endemic_baseline_output_raw$R_count)/parameters_endemic_exemplar_baseline$human_population,
             linetype = "dashed",
             linewidth = 1) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  labs(x = "Time", y = "Proportion of Population", colour = "Disease State") +
  scale_colour_discrete(type = disease_state_colours) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_vline(xintercept = uvc_timestep)

#----- 5) Exemplar Run Visualisation 2: Combined SEIR Plots ----------------------------------------

# Combined plot:
grid.arrange(

  # a) No-Intervention Baseline:
  endemic_baseline_output_raw %>%
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
    filter(timestep %in% 400:1000) %>%
    pivot_longer(cols = c(S, E, I, R), names_to = "State", values_to = "Proportion") %>%
    mutate(State = factor(State, levels = c("S", "E", "I", "R"))) %>%
    #filter(State %in% c("I", "R")) %>%
    ggplot(aes(x = timestep, y = Proportion, colour = State)) +
    # geom_hline(yintercept = max(endemic_baseline_output_raw$R_count)/parameters_endemic_exemplar_baseline$human_population,
    #            linetype = "dashed",
    #            linewidth = 1) +
    geom_line(linewidth = 1.5) +
    geom_vline(xintercept = uvc_timestep, linetype = "dashed") +
    theme_bw() +
    theme(legend.position = "none",
          axis.title.x = element_text(colour = "white")) +
    labs(x = "Time", y = "Proportion of Population", colour = "Disease State") +
    scale_colour_discrete(type = disease_state_colours) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    scale_x_continuous(expand = c(0, 0)),

  # b) Randomly Assigned Far-UVC
  endemic_random_uvc_output_raw %>%
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
    filter(timestep %in% 400:1000) %>%
    pivot_longer(cols = c(S, E, I, R), names_to = "State", values_to = "Proportion") %>%
    mutate(State = factor(State, levels = c("S", "E", "I", "R"))) %>%
    #filter(State %in% c("I", "R")) %>%
    ggplot(aes(x = timestep, y = Proportion, colour = State)) +
    # geom_hline(yintercept = max(endemic_baseline_output_raw$R_count)/parameters_endemic_exemplar_baseline$human_population,
    #            linetype = "dashed",
    #            linewidth = 1) +
    geom_line(linewidth = 1.5) +
    geom_vline(xintercept = uvc_timestep, linetype = "dashed") +
    theme_bw() +
    theme(legend.position = "none",
          axis.title.y = element_text(colour = "white")) +
    labs(x = "Time", y = "Proportion of Population", colour = "Disease State") +
    scale_colour_discrete(type = disease_state_colours) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    scale_x_continuous(expand = c(0, 0)),

  # c) Targeted Assigned Far-UVC
  endemic_targeted_uvc_output_raw %>%
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
    filter(timestep %in% 400:1000) %>%
    pivot_longer(cols = c(S, E, I, R), names_to = "State", values_to = "Proportion") %>%
    mutate(State = factor(State, levels = c("S", "E", "I", "R"))) %>%
    #filter(State %in% c("I", "R")) %>%
    ggplot(aes(x = timestep, y = Proportion, colour = State)) +
    # geom_hline(yintercept = max(endemic_baseline_output_raw$R_count)/parameters_endemic_exemplar_baseline$human_population,
    #            linetype = "dashed",
    #            linewidth = 1) +
    geom_line(linewidth = 1.5) +
    geom_vline(xintercept = uvc_timestep, linetype = "dashed") +
    theme_bw() +
    theme(legend.position = "none",
          axis.title.y = element_text(colour = "white"),
          axis.title.x = element_text(colour = "white")) +
    labs(x = "Time", y = "Proportion of Population", colour = "Disease State") +
    scale_colour_discrete(type = disease_state_colours) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    scale_x_continuous(expand = c(0, 0)),

  nrow = 1
)
