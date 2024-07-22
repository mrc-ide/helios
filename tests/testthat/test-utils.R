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
  testthat::expect_error(get_setting_size(x$variables_list, setting = "leisure"))
  testthat::expect_error(get_setting_size(x$variables_list, setting = 1))
})
