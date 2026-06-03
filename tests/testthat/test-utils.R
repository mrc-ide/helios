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

test_that("timestep_to_day_of_year() maps both timesteps of a day to the same day (dt = 0.5)", {
  expect_equal(timestep_to_day_of_year(1, 0.5), 1)
  expect_equal(timestep_to_day_of_year(2, 0.5), 1)
  expect_equal(timestep_to_day_of_year(3, 0.5), 2)
  expect_equal(timestep_to_day_of_year(4, 0.5), 2)
})

test_that("timestep_to_day_of_year() maps one timestep per day when dt = 1", {
  expect_equal(timestep_to_day_of_year(1, 1), 1)
  expect_equal(timestep_to_day_of_year(365, 1), 365)
})

test_that("timestep_to_day_of_year() recycles annually back to day 1", {
  # dt = 1: timestep 366 should wrap to day 1, 730 to day 365, 731 to day 1
  expect_equal(timestep_to_day_of_year(366, 1), 1)
  expect_equal(timestep_to_day_of_year(730, 1), 365)
  expect_equal(timestep_to_day_of_year(731, 1), 1)
  # dt = 0.5: last timestep of year (730) is day 365, first of next year (731) wraps to 1
  expect_equal(timestep_to_day_of_year(730, 0.5), 365)
  expect_equal(timestep_to_day_of_year(731, 0.5), 1)
})
