test_that("set_uvc() errors if function given multiple settings in a single call", {

  # Establish the list of model parameters:
  parameters <- get_parameters()

  # Check that set_uvc() errors when the setting input not either "workplace",
  # "school", or "leisure"
  expect_error(object = set_uvc(parameters_list = parameters,
                                setting = c("workplace", "school"),
                                coverage = c(0.8),
                                coverage_target = "buildings",
                                coverage_type = "random",
                                efficacy = c(0.8),
                                timestep = c(1)),
               regexp = "Error: Number of settings input greater than 1, parameterise for one setting at a time")


})

test_that("set_uvc() errors if function given multiple coverage types in a single call", {

  # Establish the list of model parameters:
  parameters <- get_parameters()

  # Check that set_uvc() errors when there are multiple coverage types
  expect_error(object = set_uvc(parameters_list = parameters,
                                setting = "workplace",
                                coverage = c(0.8),
                                coverage_target = "buildings",
                                coverage_type = c("random", "targeted"),
                                efficacy = c(0.8),
                                timestep = c(1)),
               regexp = "Error: Number of coverage types input greater than 1, parameterise for one coverage type at a time")


})

test_that("set_uvc() errors when setting input not from allowed list of Far UVC settings", {

  # Establish the list of model parameters:
  parameters <- get_parameters()

  # Check that set_uvc() errors when the setting input not either "workplace",
  # "school", or "leisure"
  expect_error(object = set_uvc(parameters_list = parameters,
                                setting = "hospital",
                                coverage = c(0.8),
                                coverage_target = "buildings",
                                coverage_type = "random",
                                efficacy = c(0.8),
                                timestep = c(1)),
               regexp = "Error: Input setting invalid - far UVC only deployable in workplace, school, leisure, or household settings")
})

test_that("set_uvc() errors when coverage_type input not from allowed list of Far UVC settings", {

  # Establish the list of model parameters:
  parameters <- get_parameters()

  # Check that set_uvc() errors when the setting input not either "workplace",
  # "school", or "leisure"
  expect_error(object = set_uvc(parameters_list = parameters,
                                setting = "household",
                                coverage = c(0.8),
                                coverage_target = "buildings",
                                coverage_type = "weak",
                                efficacy = c(0.8),
                                timestep = c(1)),
               regexp = "Error: Input setting invalid - far UVC only deployable in random or targeted coverage types")
})

test_that("set_uvc() errors if coverage not between 0 and 1", {

  # Establish the list of model parameters:
  parameters <- get_parameters()

  # Check that set_uvc() errors when the setting input not either "workplace",
  # "school", or "leisure"
  expect_error(object = set_uvc(parameters_list = parameters,
                                setting = "workplace",
                                coverage = c(-0.8),
                                coverage_target = "buildings",
                                coverage_type = c("targeted"),
                                efficacy = c(0.8),
                                timestep = c(1)),
               regexp = "Error: coverage must take a value between 0 and 1")


})

test_that("set_uvc() errors if efficacy not between 0 and 1", {

  # Establish the list of model parameters:
  parameters <- get_parameters()

  # Check that set_uvc() errors when the setting input not either "workplace",
  # "school", or "leisure"
  expect_error(object = set_uvc(parameters_list = parameters,
                                setting = "workplace",
                                coverage = c(0.8),
                                coverage_target = "buildings",
                                coverage_type = c("targeted"),
                                efficacy = c(1.01),
                                timestep = c(1)),
               regexp = "Error: efficacy must take a value between 0 and 1")


})

test_that("set_uvc() correctly assigns Far-UVC parameters for all settings and coverage types", {

  # Workplace

  # Establish a parameter list:
  parameters_workplace <- get_parameters()

  #Use set_uvc to parameterise far UVC in workplaces with random coverage:
  parameters_workplace <- set_uvc(parameters_list = parameters_workplace,
                                  setting = "workplace",
                                  coverage = 0.7,
                                  coverage_target = "buildings",
                                  coverage_type = "random",
                                  efficacy = 0.6,
                                  timestep = 100)

  # Manually create the parameters list you would expect to see after calling set_uvc()
  test_parameters_workplace <- get_parameters()
  test_parameters_workplace$far_uvc_workplace <- TRUE
  test_parameters_workplace$far_uvc_workplace_coverage_target <- "buildings"
  test_parameters_workplace$far_uvc_workplace_coverage_type <- "random"
  test_parameters_workplace$far_uvc_workplace_coverage <- 0.7
  test_parameters_workplace$far_uvc_workplace_efficacy <- 0.6
  test_parameters_workplace$far_uvc_workplace_timestep <- 100

  # Check that the two parameter lists are identical:
  expect_identical(object = parameters_workplace, expected = test_parameters_workplace)

  # School

  # Establish a parameter list:
  parameters_school <- get_parameters()

  #Use set_uvc to parameterise far UVC in workplaces with targeted coverage:
  parameters_school <- set_uvc(parameters_list = parameters_school,
                               setting = "school",
                               coverage = 0.7,
                               coverage_target = "buildings",
                               coverage_type = "targeted",
                               efficacy = 0.6,
                               timestep = 100)

  # Manually create the parameters list you would expect to see after calling set_uvc()
  test_parameters_school <- get_parameters()
  test_parameters_school$far_uvc_school <- TRUE
  test_parameters_school$far_uvc_school_coverage_target <- "buildings"
  test_parameters_school$far_uvc_school_coverage_type <- "targeted"
  test_parameters_school$far_uvc_school_coverage <- 0.7
  test_parameters_school$far_uvc_school_efficacy <- 0.6
  test_parameters_school$far_uvc_school_timestep <- 100

  # Check that the two parameter lists are identical:
  expect_identical(object = parameters_school, expected = test_parameters_school)

  # Leisure

  # Establish a parameter list:
  parameters_leisure <- get_parameters()

  # Use set_uvc to parameterise far UVC in workplaces with random coverage:
  parameters_leisure <- set_uvc(parameters_list = parameters_leisure,
                                setting = "leisure",
                                coverage = 0,
                                coverage_target = "buildings",
                                coverage_type = "random",
                                efficacy = 0.32,
                                timestep = 41)

  # Manually create the parameters list you would expect to see after calling set_uvc()
  test_parameters_leisure <- get_parameters()
  test_parameters_leisure$far_uvc_leisure <- TRUE
  test_parameters_leisure$far_uvc_leisure_coverage_target <- "buildings"
  test_parameters_leisure$far_uvc_leisure_coverage_type <- "random"
  test_parameters_leisure$far_uvc_leisure_coverage <- 0
  test_parameters_leisure$far_uvc_leisure_efficacy <- 0.32
  test_parameters_leisure$far_uvc_leisure_timestep <- 41

  # Check that the two parameter lists are identical:
  expect_identical(object = parameters_leisure, expected = test_parameters_leisure)

  # Household

  # Establish a parameter list:
  parameters_household <- get_parameters()

  #Use sset_uvc to parameterise far UVC in workplaces with targeted coverage:
  parameters_household <- set_uvc(parameters_list = parameters_household,
                                  setting = "household",
                                  coverage = 1,
                                  coverage_target = "buildings",
                                  coverage_type = "targeted",
                                  efficacy = 0.5,
                                  timestep = 1)

  # Manually create the parameters list you would expect to see after calling set_uvc()
  test_parameters_household <- get_parameters()
  test_parameters_household$far_uvc_household <- TRUE
  test_parameters_household$far_uvc_household_coverage_target <- "buildings"
  test_parameters_household$far_uvc_household_coverage_type <- "targeted"
  test_parameters_household$far_uvc_household_coverage <- 1
  test_parameters_household$far_uvc_household_efficacy <- 0.5
  test_parameters_household$far_uvc_household_timestep <- 1

  # Check that the two parameter lists are identical:
  expect_identical(object = parameters_household, expected = test_parameters_household)

})

test_that("set_uvc() errors coverage_target not from allowed options", {

  # Establish the list of model parameters:
  parameters <- get_parameters()

  # Check that set_uvc() errors when the setting input not either "workplace",
  # "school", or "leisure"
  expect_error(object = set_uvc(parameters_list = parameters,
                                setting = "workplace",
                                coverage = c(0.8),
                                coverage_target = "individual",
                                coverage_type = "targeted",
                                efficacy = c(0.5),
                                timestep = c(1)),
               regexp = "Error: Input setting invalid - far UVC coverage only applicable to individuals or buildings")

})

test_generate_far_uvc_switches <- function(setting, target, type, tol = 1) {
  coverage <- 0.7
  parameters_list <- get_parameters()

  parameters_list <- set_uvc(
    parameters_list = parameters_list,
    setting = setting,
    coverage = coverage,
    coverage_target = target,
    coverage_type = type,
    efficacy = 0.6,
    timestep = 100
  )

  x <- create_variables(parameters_list)
  x <- generate_far_uvc_switches(x$parameters_list, x$variables_list)

  testthat::expect_vector(x[[paste0("uvc_", setting)]], ptype = double(), size = length(x$setting_sizes[[setting]]))
  testthat::expect_equal(x$human_population * coverage, sum(x$setting_sizes[[setting]] * x[[paste0("uvc_", setting)]]), tolerance = tol)
}

test_that("generate_far_uvc_switches() with coverage_target as individuals and coverage_type as random can be used to set UVC switches for households, and the total coverage sums to the expected input", {
  test_generate_far_uvc_switches(setting = "household", target = "individuals", type = "random")
})

test_that("generate_far_uvc_switches() with coverage_target as buildings and coverage_type as random can be used to set UVC switches for households, and the total coverage sums to the expected input", {
  test_generate_far_uvc_switches(setting = "household", target = "buildings", type = "random")
})

test_that("generate_far_uvc_switches() with coverage_target as individuals and coverage_type as targeted can be used to set UVC switches for households, and the total coverage sums to the expected input", {
  test_generate_far_uvc_switches(setting = "household", target = "buildings", type = "targeted", tol = 10)
})

test_that("generate_far_uvc_switches() with coverage_target as individuals and coverage_type as targeted can be used to set UVC switches for households, and the total coverage sums to the expected input", {
  test_generate_far_uvc_switches(setting = "household", target = "buildings", type = "targeted")
})
