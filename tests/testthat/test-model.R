test_that("run_simulations_from_table() errors when the parameter_table input contains unrecognised column names", {

  # Set up example parameter table input:
  parameter_table <- data.frame("simulation_time" = c(100, 100), "gar_uvc" = c("TRUE", "FALSE"))

  expect_error(object = run_simulations_from_table(parameters_table = parameter_table, output_type = "both"),
               regexp = "Error: Parameter name in parameter_table not a recognised helios parameter")

})

test_that("run_simulations_from_table() errors when the parameter_table input is not a data frame", {

  # Set up example parameter table input:
  parameter_table <- numeric(length = 10)

  expect_error(object = run_simulations_from_table(parameters_table = parameter_table, output_type = "both"),
               regexp = "Error: parameters_table is not a data.frame - please reformat")

})

# test_that("run_simulations_from_table() run_simulations_from_table() returns list of parameters when output_type set to parameters", {

  # Set up example parameter table input:
  parameter_table <- data.frame("simulation_time" = c(10))

  # Run the function using the parameters setting:
  simulation_output <- run_simulations_from_table(parameters_table = parameter_table, output_type = "parameters")

})

##' TODO: run_simulations_from_table() returns list of simulations when output_type = "simulations"

##' TODO: run_simulations_from_table() returns list of simulation outputs and parameters when output_type = "both"
