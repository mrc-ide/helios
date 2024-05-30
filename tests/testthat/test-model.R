test_that("run_simulations_from_table() errors when the parameter_table input contains unrecognised column names", {

  # Set up example parameter table input:
  parameter_table <- data.frame("simulation_time" = c(100, 100), "gar_uvc" = c("TRUE", "FALSE"))

  expect_error(object = run_simulations_from_table(parameters_table = parameter_table),
               regexp = "Error: Parameter name in parameter_table not a recognised helios parameter")

})



