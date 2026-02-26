library(helios)
devtools::load_all()

parameters_list <- get_parameters(
  overrides = list(
    human_population = 1000,
    number_initial_S = 995,
    number_initial_E = 5,
    number_initial_I = 0,
    number_initial_R = 0,
    simulation_time = 100
  ),
  archetype = "sars_cov_2"
)


parameters_list <- parameters_list %>%
  set_setting_specific_ach("workplace", mean = 4.8, sd = 2.0) %>%
  set_setting_specific_ach("school",    mean = 3.5, sd = 1.5) %>%
  set_setting_specific_ach("leisure",   mean = 5.0, sd = 2.0) %>%
  set_setting_specific_ach("household", mean = 2.0, sd = 1.0)

# workplace sigmoid
parameters_list <- set_uvc_ach(
  parameters_list,
  setting          = "workplace",
  coverage         = 0.5,
  coverage_target  = "individuals",
  coverage_type    = "random",
  timestep         = 10,
  relationship_type = "sigmoid",
  max_efficacy     = 0.9,
  sigmoid_k        = 0.5,
  sigmoid_x0       = 5
)

#School sigmoid
parameters_list <- set_uvc_ach(
  parameters_list,
  setting          = "school",
  coverage         = 0.5,
  coverage_target  = "individuals",
  coverage_type    = "random",
  timestep         = 10,
  relationship_type = "sigmoid",
  max_efficacy     = 0.8,
  sigmoid_k        = 0.6,
  sigmoid_x0       = 4
)

#leisure constant efficacy
parameters_list <- set_uvc_ach(
  parameters_list,
  setting          = "leisure",
  coverage         = 0.5,
  coverage_target  = "individuals",
  coverage_type    = "random",
  timestep         = 10,
  relationship_type = "constant",
  max_efficacy     = NULL,   # not used for constant
  sigmoid_k        = NULL,
  sigmoid_x0       = NULL
)
parameters_list$far_uvc_leisure_efficacy <- 0.7  # fixed efficacy for constant

# Household: CONSTANT efficacy
parameters_list <- set_uvc_ach(
  parameters_list,
  setting          = "household",
  coverage         = 0.5,
  coverage_target  = "individuals",
  coverage_type    = "random",
  timestep         = 10,
  relationship_type = "constant",
  max_efficacy     = NULL,
  sigmoid_k        = NULL,
  sigmoid_x0       = NULL
)
parameters_list$far_uvc_household_efficacy <- 0.6

output <- run_simulation(parameters_list)

par(mfrow = c(1, 1))
plot(output$timestep, output$I_count,
     type = "l", col = "red",
     xlab = "Timestep", ylab = "Count",
     main = "Infectious over time")
