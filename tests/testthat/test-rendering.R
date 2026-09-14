test_that("run_simulations() correctly fails to render diagnostic outputs when render_diagnostics is switched off", {
  # Set a seed:
  set.seed(seed = 12345)

  # Open a parameters list with render_diagnostics switched on:
  parameters <- with_default_ach(get_parameters(overrides = list(simulation_time = 10)))

  # Run the simulation:
  output <- run_simulation(parameters_list = parameters)$result

  # Set up a vector of expected column names:
  expected_columns_names <- c(
    "timestep",
    "D_new",
    "E_new",
    "H_new",
    "S_count",
    "E_count",
    "I_mild_count",
    "I_hosp_count",
    "R_count",
    "D_count"
  )

  # Store the output column names:
  observed_column_names <- names(output)

  # Check that the column names match those expected:
  expect_identical(
    object = observed_column_names,
    expected = expected_columns_names
  )
})

test_that("run_simulations() correctly renders diagnostic outputs when render_diagnostics is switched on", {
  # Set a seed:
  set.seed(seed = 12345)

  # Open a parameters list with render_diagnostics switched on:
  parameters <- with_default_ach(get_parameters(
    overrides = list(simulation_time = 5, render_diagnostics = TRUE)
  ))

  # Run the simulation:
  output <- run_simulation(parameters_list = parameters)$result

  # Set up a vector of expected column names:
  expected_columns_names <- c(
    "timestep",
    "D_new",
    "FOI_household",
    "FOI_workplace",
    "FOI_school",
    "FOI_leisure",
    "FOI_community",
    "FOI_total",
    "E_new",
    "H_new",
    "S_count",
    "E_count",
    "I_mild_count",
    "I_hosp_count",
    "R_count",
    "D_count"
  )

  # Store the output column names:
  observed_column_names <- names(output)

  # Check that the column names match those expected:
  expect_identical(
    object = observed_column_names,
    expected = expected_columns_names
  )
})

test_that("Disease state counts sum to parameters$human population", {
  # Get a list of model parameters (initial states must sum to human_population):
  parameters <- with_default_ach(get_parameters(
    overrides = list(human_population = 137, number_initial_S = 132, simulation_time = 10)
  ))

  # Run the simulation:
  output <- run_simulation(parameters_list = parameters)$result

  # Sum the disease states in each time step:
  output$total_pop <- output$S_count +
    output$E_count +
    output$I_mild_count +
    output$I_hosp_count +
    output$R_count +
    output$D_count

  # Check that all summed disease states sum to the parameterised human population:
  expect_true(all(output$total_pop == parameters$human_population))
})

test_that("Renderer renders the number of externally sourced infections when endemic switched on", {
  # Generate the model variables:
  parameters_list <- with_default_ach(get_parameters(
    overrides = list(
      human_population = 1000,
      number_initial_S = 995,
      endemic_or_epidemic = 'endemic',
      duration_immune = 14,
      prob_inf_external = 0.05,
      simulation_time = 10
    )
  ))

  # Run the simulation:
  simulation_render_test <- run_simulation(parameters_list = parameters_list)$result

  # Check that run_simulation() has rendered a data frame with a column for n_external_infections:
  expect_true("n_external_infections" %in% names(simulation_render_test))
})

test_that("Renderer does not render the number of externally sourced infections when endemic switched off", {
  # Generate the model variables:
  parameters_list <- with_default_ach(get_parameters(
    overrides = list(human_population = 1000, number_initial_S = 995, simulation_time = 10)
  ))

  # Run the simulation:
  simulation_render_test <- run_simulation(parameters_list = parameters_list)$result

  # Check that run_simulation() has rendered a data frame with a column for n_external_infections:
  expect_false("n_external_infections" %in% names(simulation_render_test))
})
