test_that("get_parameters() errors when a setting-specific beta has length greater than 1", {
  expect_error(
    object = parameters <- get_parameters(
      overrides = list(
        beta_household = c(0.23),
        beta_school = c(0.45),
        beta_workplace = c(0.01),
        beta_leisure = c(0.455, 0.3),
        beta_community = c(0.4)
      )
    ),
    regexp = "ERROR: A setting-specific beta has length not equal to 1. All setting-specific betas must be of length 1"
  )
})

test_that("get_parameters() errors when the endemic setting is switched on without parameterising duration_immune", {
  expect_error(
    object = get_parameters(
      overrides = list(
        endemic_or_epidemic = 'endemic',
        prob_inf_external = 0.01
      )
    ),
    regexp = "duration_immune must be specified if endemic_or_epidemic is set to endemic"
  )
})

test_that("get_parameters() errors when the endemic setting is switched on without parameterising duration_immune", {
  expect_error(
    object = get_parameters(
      overrides = list(
        endemic_or_epidemic = 'endemic',
        duration_immune = 14
      )
    ),
    regexp = "prob_inf_external must be specified if endemic_or_epidemic is set to endemic"
  )
})

test_that("get_parameters() successfully creates parameter list for endemic setting when all required parameters are parsed", {
  expect_no_error(
    pl <- get_parameters(
      overrides = list(
        endemic_or_epidemic = 'endemic',
        duration_immune = 14,
        prob_inf_external = 0.1
      )
    )
  )
})

test_that("get_parameters() errors when an unrecognised archetype is input", {
  expect_error(get_parameters(archetype = "floo"), "archetype not recognised")
})

test_that("get_parameters() assigns correct parameters for flu archetype", {
  # Generate the parameter list for the fly archetype:
  pl <- get_parameters(archetype = "flu")

  # Check that the archetype-specific parameters match the expected values:
  expect_identical(object = pl$duration_exposed, expected = 1)
  expect_identical(object = pl$duration_infectious, expected = 2)
  expect_identical(object = pl$beta_household, expected = 0.207)
  expect_identical(object = pl$beta_school, expected = 0.207)
  expect_identical(object = pl$beta_workplace, expected = 0.207)
  expect_identical(object = pl$beta_leisure, expected = 0.207)
  expect_identical(object = pl$beta_community, expected = 0.069)
})

test_that("get_parameters() assigns correct parameters for SARS-CoV-2 archetype", {
  # Generate the parameter list for the fly archetype:
  pl <- get_parameters(archetype = "sars_cov_2")

  # Check that the archetype-specific parameters match the expected values:
  expect_identical(object = pl$duration_exposed, expected = 2)
  expect_identical(object = pl$duration_infectious, expected = 4)
  expect_identical(object = pl$beta_household, expected = 0.24)
  expect_identical(object = pl$beta_school, expected = 0.24)
  expect_identical(object = pl$beta_workplace, expected = 0.24)
  expect_identical(object = pl$beta_leisure, expected = 0.24)
  expect_identical(object = pl$beta_community, expected = 0.08)
})

test_that("get_parameters() assigns correct parameters for measles archetype", {
  # Generate the parameter list for the fly archetype:
  pl <- get_parameters(archetype = "measles")

  # Check that the archetype-specific parameters match the expected values:
  expect_identical(object = pl$duration_exposed, expected = 8)
  expect_identical(object = pl$duration_infectious, expected = 5)
  expect_identical(object = pl$beta_household, expected = 1.26)
  expect_identical(object = pl$beta_school, expected = 1.26)
  expect_identical(object = pl$beta_workplace, expected = 1.26)
  expect_identical(object = pl$beta_leisure, expected = 1.26)
  expect_identical(object = pl$beta_community, expected = 0.42)
})

test_that("run_simulation() works when a parameter archetype specified", {
  # Generate the parameter list for the fly archetype:
  pl <- get_parameters(
    overrides = list(simulation_time = 3),
    archetype = "flu"
  )

  # Run the simulation:
  simulation_example <- run_simulation(parameters_list = pl)

  # Check that the output contains some expected column names and that it is a data.frame:
  expect_true(all(
    c("S_count", "E_count", "I_count", "R_count") %in% names(simulation_example)
  ))
  expect_true(object = is.data.frame(simulation_example))
})
