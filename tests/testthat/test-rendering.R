test_that("run_simulations() correctly fails to render diagnostic outputs when render_diagnostics is switched off", {

  # Set a seed:
  set.seed(seed = 12345)

  # Open a parameters list with render_diagnostics switched on:
  parameters <- get_parameters(overrides = list(simulation_time = 10))

  # Run the simulation:
  output <- run_simulation(parameters_list = parameters)

  # Set up a vector of expected column names:
  expected_columns_names <- c("timestep",
                              "S_count",
                              "E_count",
                              "I_count",
                              "R_count")

  # Store the output column names:
  observed_column_names <- names(output)

  # Check that the column names match those expected:
  expect_identical(object = observed_column_names, expected = expected_columns_names)

})

test_that("run_simulations() correctly renders diagnostic outputs when render_diagnostics is switched on", {

  # Set a seed:
  set.seed(seed = 12345)

  # Open a parameters list with render_diagnostics switched on:
  parameters <- get_parameters(overrides = list(simulation_time = 5,
                                                render_diagnostics = TRUE))

  # Run the simulation:
  output <- run_simulation(parameters_list = parameters)

  # Set up a vector of expected column names:
  expected_columns_names <- c("timestep",
                              "FOI_household",
                              "FOI_workplace",
                              "FOI_school",
                              "FOI_leisure",
                              "FOI_community",
                              "FOI_total",
                              "S_count",
                              "E_count",
                              "I_count",
                              "R_count")

  # Store the output column names:
  observed_column_names <- names(output)

  # Check that the column names match those expected:
  expect_identical(object = observed_column_names, expected = expected_columns_names)

})

test_that("Disease state counts sum to parameters$human population", {

  # Get a list of model parameters:
  parameters <- get_parameters(overrides = list(human_population = 137,
                                                simulation_time = 10))

  # Run the simulation:
  output <- run_simulation(parameters_list = parameters)

  # Sum the disease states in each time step:
  for(i in 1:nrow(output)) {
    output$total_pop[i] <- sum(output$S_count[i] + output$E_count[i] + output$I_count[i] + output$R_count[i])
  }

  # Check that all summed disease states sum to the parameterised human population:
  expect_true(all(output$total_pop == parameters$human_population))

})

#test_that("Renderer renders the number of externall sourced infections when endemic switched on", {

})
