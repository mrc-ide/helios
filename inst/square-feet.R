# Loading library
library(helios)

# * [x] Pass in size per person for each setting type
# * [x] Pass in joint UVC parameters
# * [x] New "joint_allocation" function within generate_far_uvc_switches
# * [ ] Appropriate handling of defaults and making life easy for users
# * [ ] Get rid of buildings version from other part of tree

parameters_list <- get_parameters(archetype = "sars_cov_2")
variables_list <- create_variables(parameters_list)

parameters <- variables_list$parameters_list
variables_list <- variables_list$variables_list

parameters <- set_uvc(parameters_list = parameters, setting = "joint", coverage = 0.5, coverage_target = "individuals", coverage_type = "random", efficacy = 0.5, timestep = 10)

x <- generate_far_uvc_switches(parameters, variables_list)

size_covered <- list(
  "workplace" = sum(x$setting_sizes$workplace * x$size_per_individual_workplace * x$uvc_workplace),
  "school" = sum(x$setting_sizes$school * x$size_per_individual_school * x$uvc_school),
  "household" = sum(x$setting_sizes$household * x$size_per_individual_household * x$uvc_household),
  "leisure" = sum(x$setting_sizes$leisure * x$size_per_individual_leisure * x$uvc_leisure)
)

total_size <- sum(x$setting_sizes$workplace * x$size_per_individual_workplace) +
  sum(x$setting_sizes$school * x$size_per_individual_school) +
  sum(x$setting_sizes$household * x$size_per_individual_household) +
  sum(x$setting_sizes$leisure * x$size_per_individual_leisure)

total_size * x$far_uvc_joint_coverage

sum(unlist(size_covered))
