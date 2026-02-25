#Test Run with workplace sigmoid efficacy

library(helios)
devtools::load_all()

parameters_list <- get_parameters(
  overrides = list(
    human_population = 1000,
    number_initial_S = 995,
    number_initial_E = 5,
    number_initial_I = 0,
    number_initial_R = 0,
    simulation_time = 50
  ),
  archetype = "sars_cov_2"
)

parameters_list <- set_setting_specific_ach(parameters_list, setting = "workplace", mean = 4.8, sd = 2.0)
parameters_list <- set_setting_specific_ach(parameters_list, setting = "school",    mean = 3.5, sd = 1.5)
parameters_list <- set_setting_specific_ach(parameters_list, setting = "leisure",   mean = 5.0, sd = 2.0)
parameters_list <- set_setting_specific_ach(parameters_list, setting = "household", mean = 2.0, sd = 1.0)

parameters_list <- set_uvc_ach(
  parameters_list,
  setting = "workplace",
  coverage = 0.5,
  coverage_target = "square_footage",
  coverage_type = "random",
  timestep = 10,
  relationship_type = "sigmoid",
  max_efficacy = 0.9,
  sigmoid_k = 0.5,
  sigmoid_x0 = 5
)

output <- run_simulation(parameters_list)


plot(output$timestep, output$I_count, type = "l", col = "red",
     main = "Infectious counts over time",
     xlab = "Timestep", ylab = "Count")
