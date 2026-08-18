test_that("get_setting_size() returns integer vectors for setting as workplace, school, and household", {
  parameters_list <- get_parameters()
  x <- create_variables(parameters_list)
  testthat::expect_vector(
    get_setting_size(x$variables_list, setting = "workplace"),
    ptype = integer()
  )
  testthat::expect_vector(
    get_setting_size(x$variables_list, setting = "school"),
    ptype = integer()
  )
  testthat::expect_vector(
    get_setting_size(x$variables_list, setting = "household"),
    ptype = integer()
  )
})

test_that("get_setting_size() returns integer vectors for setting as workplace, school, and household", {
  parameters_list <- get_parameters()
  x <- create_variables(parameters_list)
  testthat::expect_error(get_setting_size(
    x$variables_list,
    setting = "leisure"
  ))
  testthat::expect_error(get_setting_size(x$variables_list, setting = 1))
})

test_that("timestep_to_day() maps both timesteps of a day to the same day (dt = 0.5)", {
  expect_equal(timestep_to_day(1, 0.5), 1)
  expect_equal(timestep_to_day(2, 0.5), 1)
  expect_equal(timestep_to_day(3, 0.5), 2)
  expect_equal(timestep_to_day(4, 0.5), 2)
})

test_that("timestep_to_day() maps one timestep per day when dt = 1", {
  expect_equal(timestep_to_day(1, 1), 1)
  expect_equal(timestep_to_day(365, 1), 365)
})

test_that("timestep_to_day() does not recycle and keeps counting past day 365", {
  expect_equal(timestep_to_day(366, 1), 366)
  expect_equal(timestep_to_day(730, 1), 730)
  expect_equal(timestep_to_day(731, 0.5), 366)
})

test_that("timestep_to_day() errors when t is not an integer", {
  expect_error(timestep_to_day(1.5, 0.5), "t must be an integer value")
})

test_that("timestep_to_day() errors when dt is zero or negative", {
  expect_error(timestep_to_day(1, 0),  "dt must be a positive numeric value")
  expect_error(timestep_to_day(1, -1), "dt must be a positive numeric value")
})
