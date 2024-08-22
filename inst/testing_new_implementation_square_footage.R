#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+++ Square-Footage Implementation Testing +++#
#+++++++++++++++++++++++++++++++++++++++++++++#

#----- 1) Establish Base Parameter List ------------------------------------------------------------

# Loading library
library(helios)

## Setting up the parameters list and variables list
parameters_list <- get_parameters(archetype = "sars_cov_2",
                                  overrides = list(human_population = 15000,
                                                   number_initial_S = 7000,
                                                   number_initial_E = 500,
                                                   number_initial_I = 500,
                                                   number_initial_R = 7000,
                                                   size_per_individual_workplace = 5,
                                                   size_per_individual_school = 2,
                                                   size_per_individual_leisure = 15,
                                                   size_per_individual_household = 10,
                                                   setting_specific_riskiness_workplace = TRUE,
                                                   setting_specific_riskiness_workplace_meanlog = 0,
                                                   setting_specific_riskiness_workplace_sdlog = 0.3544,
                                                   setting_specific_riskiness_workplace_min = 1/sqrt(4.75),
                                                   setting_specific_riskiness_workplace_max = sqrt(4.75),
                                                   setting_specific_riskiness_school = TRUE,
                                                   setting_specific_riskiness_school_meanlog = 0,
                                                   setting_specific_riskiness_school_sdlog = 0.3544,
                                                   setting_specific_riskiness_school_min = 1/sqrt(4.75),
                                                   setting_specific_riskiness_school_max = sqrt(4.75),
                                                   setting_specific_riskiness_leisure = TRUE,
                                                   setting_specific_riskiness_leisure_meanlog = 0,
                                                   setting_specific_riskiness_leisure_sdlog = 0.3544,
                                                   setting_specific_riskiness_leisure_min = 1/sqrt(4.75),
                                                   setting_specific_riskiness_leisure_max = sqrt(4.75),
                                                   setting_specific_riskiness_household = TRUE,
                                                   setting_specific_riskiness_household_meanlog = 0,
                                                   setting_specific_riskiness_household_sdlog = 0.3544,
                                                   setting_specific_riskiness_household_min = 1/sqrt(4.75),
                                                   setting_specific_riskiness_household_max = sqrt(4.75),
                                                   endemic_or_epidemic = "endemic",
                                                   duration_immune = 10,
                                                   prob_inf_external = 1 / 50000,
                                                   simulation_time = 500))

# Generate the list of model variables:
variables_list <- create_variables(parameters_list)
parameters <- variables_list$parameters_list
variables_list <- variables_list$variables_list

# Specify some dummy far UVC timesteps:
timestep_far_UVC <- 250
efficacy_far_UVC <- 0.9

#----- 2) Setting: Joint, Target = Individuals, Type = Random --------------------------------------

#+++++++++++++++++++++++++++++++++++++++++++++++#
#+++ Testing setting = "joint" +++++++++++++++++#
#+           coverage_target = "individuals" +++#
#++++++++++  coverage_type = "random" ++++++++++#
#+++++++++++++++++++++++++++++++++++++++++++++++#

# Parameterise far UVC for jointly targeting settings at random to cover a target number of individuals:
parameters_individuals <- set_uvc(parameters_list = parameters,
                                  setting = "joint",
                                  coverage = 0.5,
                                  coverage_target = "individuals",
                                  coverage_type = "random",
                                  efficacy = efficacy_far_UVC,
                                  timestep = timestep_far_UVC)

# Generate the far UVC switches for the coverage_target = "individuals" run:
switches_individuals <- generate_far_uvc_switches(parameters_list = parameters_individuals,
                                                  variables_list = variables_list)

# Calculate the number of individuals covered by far UVC in each setting type:
size_covered_individuals <- list(
  "workplace" = sum(switches_individuals$setting_sizes$workplace * switches_individuals$uvc_workplace),
  "school" = sum(switches_individuals$setting_sizes$school * switches_individuals$uvc_school),
  "household" = sum(switches_individuals$setting_sizes$household * switches_individuals$uvc_household),
  "leisure" = sum(switches_individuals$setting_sizes$leisure * switches_individuals$uvc_leisure)
)

# Calculate the total number of individuals acfross each setting type (e.g. total occupancy?):
total_size_individuals <- sum(switches_individuals$setting_sizes$workplace) +
                          sum(switches_individuals$setting_sizes$school) +
                          sum(switches_individuals$setting_sizes$household) +
                          sum(switches_individuals$setting_sizes$leisure)

# Check that the total occupancy x far UVC joint coverage (50%) is apporximately equal to the number of covered individuals:
(total_size_individuals * switches_individuals$far_uvc_joint_coverage) / sum(unlist(size_covered_individuals))

# Plot the UVC switches against the specific-riskinesses of each setting type:
plot(switches_individuals$uvc_household, parameters_individuals$household_specific_riskiness, cex = 0.01)
plot(switches_individuals$uvc_school, parameters_individuals$school_specific_riskiness, cex = 0.01)
plot(switches_individuals$uvc_workplace, parameters_individuals$workplace_specific_riskiness, cex = 0.01)
plot(switches_individuals$uvc_leisure, parameters_individuals$leisure_specific_riskiness, cex = 0.01)

#----- 3) Setting: Joint, Target = Square Footage, Type = Random -----------------------------------

#++++++++++++++++++++++++++++++++++++++++++++++++++#
#+++ Testing setting = "joint" ++++++++++++++++++++#
#+           coverage_target = "square_footage" +++#
#++++++++++  coverage_type = "random" +++++++++++++#
#++++++++++++++++++++++++++++++++++++++++++++++++++#

# Parameterise far UVC for jointly targeting settings at random to cover a target square footage:
parameters_square_footage <- set_uvc(parameters_list = parameters,
                                     setting = "joint",
                                     coverage = 0.5,
                                     coverage_target = "square_footage",
                                     coverage_type = "random",
                                     efficacy = efficacy_far_UVC,
                                     timestep = timestep_far_UVC)

# Generate the far UVC switches:
switches_square_footage <- generate_far_uvc_switches(parameters_list = parameters_square_footage,
                                                     variables_list = variables_list)

# Calculate the square footage covered in each setting type
size_covered_square_footage <- list(
  "workplace" = sum(switches_square_footage$setting_sizes$workplace * switches_square_footage$uvc_workplace *  switches_square_footage$size_per_individual_workplace),
  "school" = sum(switches_square_footage$setting_sizes$school * switches_square_footage$uvc_school *  switches_square_footage$size_per_individual_school),
  "household" = sum(switches_square_footage$setting_sizes$household * switches_square_footage$uvc_household) *  switches_square_footage$size_per_individual_household ,
  "leisure" = sum(switches_square_footage$setting_sizes$leisure * switches_square_footage$uvc_leisure *  switches_square_footage$size_per_individual_leisure)
)


total_size_square_footage <- sum(switches_square_footage$setting_sizes$workplace) *  switches_square_footage$size_per_individual_workplace +
  sum(switches_square_footage$setting_sizes$school) *  switches_square_footage$size_per_individual_school  +
  sum(switches_square_footage$setting_sizes$household) *  switches_square_footage$size_per_individual_household  +
  sum(switches_square_footage$setting_sizes$leisure) *  switches_square_footage$size_per_individual_leisure
(total_size_square_footage * switches_square_footage$far_uvc_joint_coverage) / sum(unlist(size_covered_square_footage)) # checking coverages match up
plot(switches_square_footage$uvc_household, parameters_square_footage$household_specific_riskiness, cex = 0.01)
plot(switches_square_footage$uvc_school, parameters_square_footage$school_specific_riskiness, cex = 0.01)
plot(switches_square_footage$uvc_workplace, parameters_square_footage$workplace_specific_riskiness, cex = 0.01)
plot(switches_square_footage$uvc_leisure, parameters_square_footage$leisure_specific_riskiness, cex = 0.01)

#----- 4) Setting: Joint, Target = Individuals, Type = Target-Riskiness ----------------------------

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+++ Testing setting = "joint" ++++++++++++++++++++++++++++++++#
#+           coverage_target = "individuals"+++++++++++++++++++#
#++++++++++  coverage_type = "targeted_riskiness" +++++++++++++#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

#
parameters_individuals_riskiness <- set_uvc(parameters_list = parameters,
                                            setting = "joint",
                                            coverage = 0.5,
                                            coverage_target = "individuals",
                                            coverage_type = "targeted_riskiness",
                                            efficacy = efficacy_far_UVC,
                                            timestep = timestep_far_UVC)


switches_individuals_riskiness <- generate_far_uvc_switches(parameters_list = parameters_individuals_riskiness, variables_list = variables_list)
size_covered_individuals_riskiness <- list(
  "workplace" = sum(switches_individuals_riskiness$setting_sizes$workplace * switches_individuals_riskiness$uvc_workplace),
  "school" = sum(switches_individuals_riskiness$setting_sizes$school * switches_individuals_riskiness$uvc_school),
  "household" = sum(switches_individuals_riskiness$setting_sizes$household * switches_individuals_riskiness$uvc_household),
  "leisure" = sum(switches_individuals_riskiness$setting_sizes$leisure * switches_individuals_riskiness$uvc_leisure)
)
total_size_individuals_riskiness <- sum(switches_individuals_riskiness$setting_sizes$workplace) +
  sum(switches_individuals_riskiness$setting_sizes$school) +
  sum(switches_individuals_riskiness$setting_sizes$household) +
  sum(switches_individuals_riskiness$setting_sizes$leisure)
(total_size_individuals_riskiness * switches_individuals_riskiness$far_uvc_joint_coverage) / sum(unlist(size_covered_individuals_riskiness)) # checking coverages match up
plot(switches_individuals_riskiness$uvc_household, parameters_individuals_riskiness$household_specific_riskiness, cex = 0.01)
plot(switches_individuals_riskiness$uvc_school, parameters_individuals_riskiness$school_specific_riskiness, cex = 0.01)
plot(switches_individuals_riskiness$uvc_workplace, parameters_individuals_riskiness$workplace_specific_riskiness, cex = 0.01)
plot(switches_individuals_riskiness$uvc_leisure, parameters_individuals_riskiness$leisure_specific_riskiness, cex = 0.01)

#----- 4) Setting: Joint, Target = Square-Footage, Type = Target-Riskiness -------------------------

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+++ Testing setting = "joint" ++++++++++++++++++++++++++++++++#
#+           coverage_target = "square_footage" +++++++++++++++#
#++++++++++  coverage_type = "targeted_riskiness" +++++++++++++#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
parameters_square_footage_riskiness <- set_uvc(parameters_list = parameters,
                                               setting = "joint",
                                               coverage = 0.5,
                                               coverage_target = "square_footage",
                                               coverage_type = "targeted_riskiness",
                                               efficacy = efficacy_far_UVC,
                                               timestep = timestep_far_UVC)

switches_square_footage_riskiness <- generate_far_uvc_switches(parameters_list = parameters_square_footage_riskiness, variables_list = variables_list)
size_covered_square_footage_riskiness <- list(
  "workplace" = sum(switches_square_footage_riskiness$setting_sizes$workplace * switches_square_footage_riskiness$uvc_workplace *  switches_square_footage_riskiness$size_per_individual_workplace),
  "school" = sum(switches_square_footage_riskiness$setting_sizes$school * switches_square_footage_riskiness$uvc_school *  switches_square_footage_riskiness$size_per_individual_school),
  "household" = sum(switches_square_footage_riskiness$setting_sizes$household * switches_square_footage_riskiness$uvc_household) *  switches_square_footage_riskiness$size_per_individual_household ,
  "leisure" = sum(switches_square_footage_riskiness$setting_sizes$leisure * switches_square_footage_riskiness$uvc_leisure *  switches_square_footage_riskiness$size_per_individual_leisure)
)
total_size_square_footage_riskiness <- sum(switches_square_footage_riskiness$setting_sizes$workplace) *  switches_square_footage_riskiness$size_per_individual_workplace +
  sum(switches_square_footage_riskiness$setting_sizes$school) *  switches_square_footage_riskiness$size_per_individual_school  +
  sum(switches_square_footage_riskiness$setting_sizes$household) *  switches_square_footage_riskiness$size_per_individual_household  +
  sum(switches_square_footage_riskiness$setting_sizes$leisure) *  switches_square_footage_riskiness$size_per_individual_leisure
(total_size_square_footage_riskiness * switches_square_footage_riskiness$far_uvc_joint_coverage) / sum(unlist(size_covered_square_footage_riskiness)) # checking coverages match up
plot(switches_square_footage_riskiness$uvc_household, parameters_square_footage_riskiness$household_specific_riskiness, cex = 0.01)
plot(switches_square_footage_riskiness$uvc_school, parameters_square_footage_riskiness$school_specific_riskiness, cex = 0.01)
plot(switches_square_footage_riskiness$uvc_workplace, parameters_square_footage_riskiness$workplace_specific_riskiness, cex = 0.01)
plot(switches_square_footage_riskiness$uvc_leisure, parameters_square_footage_riskiness$leisure_specific_riskiness, cex = 0.01)

## Running the model with and without far UVC etc etc
model_run_individual <- run_simulation(parameters_individuals)
model_run_square_footage <- run_simulation(parameters_square_footage)
model_run_individual_riskiness <- run_simulation(parameters_individuals_riskiness)
model_run_square_footage_riskiness <- run_simulation(parameters_square_footage_riskiness)

### needs to be completed
plot(model_run_individual$timestep, model_run_individual$E_new, type = "l", col = "black")
lines(model_run_individual_riskiness$timestep, model_run_individual_riskiness$E_new, type = "l", col = "darkorchid")
lines(model_run_square_footage$timestep, model_run_square_footage$E_new, type = "l", col = "blue")
lines(model_run_individual_riskiness$timestep, model_run_square_footage_riskiness$E_new, type = "l", col = "orange")
