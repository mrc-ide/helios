test_that("get_parameters() errors when a setting-specific beta has length greater than 1", {

  expect_error(object = parameters <- get_parameters(overrides = list(beta_household = c(0.23),
                                                                      beta_school = c(0.45),
                                                                      beta_workplace = c(0.01),
                                                                      beta_leisure = c(0.455, 0.3),
                                                                      beta_community = c(0.4))),
               regexp = "ERROR: A setting-specific beta has length not equal to 1. All setting-specific betas must be of length 1")

})

test_that("get_parameters() errors when the endemic setting is switched on without parameterising duration_immune", {

  expect_error(object = get_parameters(overrides = list(
    endemic_or_epidemic = 'endemic',
    prob_inf_external = 0.01)
  ),
  regexp = "duration_immune must be specified if endemic_or_epidemic is set to endemic")

})

test_that("get_parameters() errors when the endemic setting is switched on without parameterising duration_immune", {

  expect_error(object = get_parameters(overrides = list(
    endemic_or_epidemic = 'endemic',
    duration_immune = 14)
  ),
  regexp = "prob_inf_external must be specified if endemic_or_epidemic is set to endemic")

})

test_that("get_parameters() successfully creates parameter list for endemic setting when all required parameters are parsed", {

  expect_no_error(parameters_list <- get_parameters(overrides = list(
    endemic_or_epidemic = 'endemic',
    duration_immune = 14,
    prob_inf_external = 0.1)
    )
  )

})






