check_population_invariant <- function(parameters) {
  population_data <- generate_population_data(parameters)

  expect_type(population_data$initial_disease_states, "character")
  expect_in(population_data$initial_disease_states, DISEASE_STATES)
  expect_length(population_data$initial_disease_states, parameters$human_population)

  expect_type(population_data$age_classes, "character")
  expect_in(population_data$age_classes, AGE_CLASSES)
  expect_length(population_data$age_classes, parameters$human_population)

  num_households <- length(population_data$household_specific_riskiness)
  num_workplaces <- length(population_data$workplace_specific_riskiness)
  num_schools <- length(population_data$school_specific_riskiness)
  num_leisure <- length(population_data$leisure_specific_riskiness)

  # Having these specifically as "integer" rather than double is desirable as it
  # has more predictable behaviour, eg. `as.character(100000L)` is "100000",
  # not "1e+05".
  expect_type(population_data$initial_household_settings, "integer")
  expect_length(population_data$initial_household_settings, parameters$human_population)
  expect_true(all(population_data$initial_household_settings != 0))
  expect_true(all(population_data$initial_household_settings <= num_households))

  expect_type(population_data$initial_school_settings, "integer")
  expect_length(population_data$initial_school_settings, parameters$human_population)
  expect_true(all(population_data$initial_school_settings <= num_schools))

  expect_type(population_data$initial_workplace_settings, "integer")
  expect_length(population_data$initial_workplace_settings, parameters$human_population)
  expect_true(all(population_data$initial_workplace_settings <= num_workplaces))

  expect_type(population_data$initial_leisure_settings, "list")
  expect_length(population_data$initial_leisure_settings, parameters$human_population)
  expect_true(all(sapply(population_data$initial_leisure_settings, typeof) == "integer"))

  # Leisure indices is the vector of non-empty leisure locations (including the
  # null location 0).
  expect_type(population_data$leisure_indices, "integer")
  expect_in(unlist(population_data$initial_leisure_settings), population_data$leisure_indices)
  expect_length(population_data$leisure_indices[population_data$leisure_indices != 0],
                num_leisure)

  expect_type(population_data$household_specific_riskiness, "double")
  expect_type(population_data$school_specific_riskiness, "double")
  expect_type(population_data$workplace_specific_riskiness, "double")
  expect_type(population_data$leisure_specific_riskiness, "double")

  # There's a 1:1 mapping between households and individuals. That is not true
  # of other locations, eg. not everyone visits a school or a workplace.
  expect_equal(sum(population_data$setting_sizes$household), parameters$human_population)
  expect_equal(sum(population_data$setting_sizes$workplace), sum(population_data$initial_workplace_settings != 0))
  expect_equal(sum(population_data$setting_sizes$school), sum(population_data$initial_school_settings != 0))

  # Total of workplace and school does adds up to the size of the non-elderly
  # population though.
  total_activity <- sum(population_data$setting_sizes$workplace) + sum(population_data$setting_sizes$school)
  expect_equal(total_activity, sum(population_data$age_classes != "elderly"))
}

test_that("population data invariants", {
  check_population_invariant(get_parameters())
  check_population_invariant(get_parameters(list(household_distribution_country = "custom")))
  check_population_invariant(get_parameters(list(school_distribution_country = "custom")))
})
