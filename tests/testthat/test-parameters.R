test_that("get_parameters() errors when a setting-specific beta has length greater than 1", {

  expect_error(object = parameters <- get_parameters(overrides = list(beta_household = c(0.23),
                                                                      beta_school = c(0.45),
                                                                      beta_workplace = c(0.01),
                                                                      beta_leisure = c(0.455, 0.3),
                                                                      beta_community = c(0.4))),
               regexp = "ERROR: A setting-specific beta has length not equal to 1. All setting-specific betas must be of length 1")

})
