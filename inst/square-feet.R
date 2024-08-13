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

# Check this is geq to coverage times population
sum(x$setting_sizes$household * x$uvc_household)
