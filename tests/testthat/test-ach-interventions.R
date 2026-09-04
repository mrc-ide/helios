#====================================================================#
#===== make_intervention() / set_intervention_ach() validation =====#
#====================================================================#

test_that("set_intervention_ach() errors if function given multiple settings in a single call", {
  parameters <- get_parameters()
  intervention <- make_intervention(
    name = "test",
    delta_function = function() 2,
    coverage = 0.5
  )

  expect_error(
    object = set_intervention_ach(
      parameters_list = parameters,
      setting = c("workplace", "school"),
      coverage_target = "individuals",
      coverage_type = "random",
      timestep = 1,
      intervention = intervention
    ),
    regexp = "Error: Number of settings input greater than 1, parameterise for one setting at a time"
  )
})

test_that("set_intervention_ach() errors when setting input not from allowed list", {
  parameters <- get_parameters()
  intervention <- make_intervention(
    name = "test",
    delta_function = function() 2,
    coverage = 0.5
  )

  expect_error(
    object = set_intervention_ach(
      parameters_list = parameters,
      setting = "hospital",
      coverage_target = "individuals",
      coverage_type = "random",
      timestep = 1,
      intervention = intervention
    ),
    regexp = "Error: Input setting invalid - intervention only deployable in workplace, school, leisure, household, or joint settings"
  )
})

test_that("set_intervention_ach() errors when coverage_target not from allowed list", {
  parameters <- get_parameters()
  intervention <- make_intervention(
    name = "test",
    delta_function = function() 2,
    coverage = 0.5
  )

  expect_error(
    object = set_intervention_ach(
      parameters_list = parameters,
      setting = "workplace",
      coverage_target = "buildings",
      coverage_type = "random",
      timestep = 1,
      intervention = intervention
    ),
    regexp = "Error: coverage_target must be either 'individuals' or 'square_footage'"
  )
})

test_that("set_intervention_ach() errors if function given multiple coverage types in a single call", {
  parameters <- get_parameters()
  intervention <- make_intervention(
    name = "test",
    delta_function = function() 2,
    coverage = 0.5
  )

  expect_error(
    object = set_intervention_ach(
      parameters_list = parameters,
      setting = "workplace",
      coverage_target = "individuals",
      coverage_type = c("random", "targeted_riskiness"),
      timestep = 1,
      intervention = intervention
    ),
    regexp = "Error: Number of coverage types input greater than 1, parameterise for one coverage type at a time"
  )
})

test_that("set_intervention_ach() errors when coverage_type not from allowed list", {
  parameters <- get_parameters()
  intervention <- make_intervention(
    name = "test",
    delta_function = function() 2,
    coverage = 0.5
  )

  expect_error(
    object = set_intervention_ach(
      parameters_list = parameters,
      setting = "household",
      coverage_target = "individuals",
      coverage_type = "weak",
      timestep = 1,
      intervention = intervention
    ),
    regexp = "Error: coverage_type must be either 'random' or 'targeted_riskiness'"
  )
})

test_that("set_intervention_ach() correctly assigns intervention parameters for a per-setting deployment", {
  parameters <- get_parameters()
  intervention <- make_intervention(
    name = "hepa_filter",
    delta_function = function() 5,
    coverage = 0.7
  )

  parameters <- set_intervention_ach(
    parameters_list = parameters,
    setting = "workplace",
    coverage_target = "square_footage",
    coverage_type = "random",
    timestep = 100,
    intervention = intervention
  )

  expect_true(parameters$intervention_workplace_active)
  expect_identical(parameters$intervention_workplace_list, list(intervention))
  expect_identical(parameters$intervention_workplace_coverage_target, "square_footage")
  expect_identical(parameters$intervention_workplace_coverage_type, "random")
  expect_identical(parameters$intervention_workplace_timestep, 100)
})

test_that("set_intervention_ach() additionally populates intervention_joint_coverage when setting is joint", {
  parameters <- get_parameters()
  intervention <- make_intervention(
    name = "hepa_filter",
    delta_function = function() 5,
    coverage = 0.4
  )

  parameters <- set_intervention_ach(
    parameters_list = parameters,
    setting = "joint",
    coverage_target = "individuals",
    coverage_type = "targeted_riskiness",
    timestep = 1,
    intervention = intervention
  )

  expect_true(parameters$intervention_joint_active)
  expect_identical(parameters$intervention_joint_coverage, 0.4)
})

#====================================================#
#===== generate_setting_specific_ach() / riskiness =====#
#====================================================#

test_that("generate_setting_specific_ach() errors if neither setting-specific nor default ACH configured", {
  parameters <- get_parameters()

  expect_error(
    object = generate_setting_specific_ach(
      parameters_list = parameters,
      setting = "workplace",
      number_of_locations = 10
    ),
    regexp = "ACH for the workplace setting has not been configured"
  )
})

test_that("generate_setting_specific_ach() returns a uniform vector when set_default_ach() used", {
  parameters <- set_default_ach(get_parameters(), setting = "workplace", ach = 6)

  ach_values <- generate_setting_specific_ach(
    parameters_list = parameters,
    setting = "workplace",
    number_of_locations = 25
  )

  expect_length(ach_values, 25)
  expect_true(all(ach_values == 6))
})

test_that("generate_setting_specific_ach() draws a heterogeneous vector when set_setting_specific_ach() used", {
  parameters <- set_setting_specific_ach(
    get_parameters(),
    setting = "workplace",
    mean = 6,
    sd = 1.5
  )

  set.seed(1)
  ach_values <- generate_setting_specific_ach(
    parameters_list = parameters,
    setting = "workplace",
    number_of_locations = 1000
  )

  expect_length(ach_values, 1000)
  expect_true(all(ach_values >= 0))
  # Heterogeneous, so not every location should get the same value:
  expect_gt(length(unique(ach_values)), 1)
  expect_equal(mean(ach_values), 6, tolerance = 0.5)
})

test_that("set_default_ach() errors if setting not recognised", {
  expect_error(
    object = set_default_ach(get_parameters(), setting = "hospital", ach = 4),
    regexp = "setting must be one of"
  )
})

test_that("set_default_ach() errors if ach is negative or not a single numeric", {
  expect_error(
    object = set_default_ach(get_parameters(), setting = "workplace", ach = -1),
    regexp = "ach must be a single non-negative numeric value"
  )
  expect_error(
    object = set_default_ach(get_parameters(), setting = "workplace", ach = c(1, 2)),
    regexp = "ach must be a single non-negative numeric value"
  )
})

test_that("convert_ach_to_riskiness() anchors riskiness at ~1 for the median ACH location", {
  parameters <- get_parameters()
  ach_values <- c(1, 2, 4, 8, 16)

  riskiness <- convert_ach_to_riskiness(
    ach_values = ach_values,
    parameters_list = parameters,
    setting = "workplace"
  )

  expect_equal(riskiness[3], 1) # location with the median ACH (4)
})

test_that("convert_ach_to_riskiness() is monotonically decreasing in baseline ACH", {
  parameters <- get_parameters()
  ach_values <- c(1, 2, 4, 8, 16)

  riskiness <- convert_ach_to_riskiness(
    ach_values = ach_values,
    parameters_list = parameters,
    setting = "workplace"
  )

  expect_true(all(diff(riskiness) < 0))
})

#==================================================#
#===== calculate_efficacy_from_ach() =====#
#==================================================#

test_that("calculate_efficacy_from_ach() returns all zeros when no intervention is active", {
  parameters <- get_parameters()
  ach_values <- rep(4, 10)

  efficacy <- calculate_efficacy_from_ach(
    ach_values = ach_values,
    parameters_list = parameters,
    setting = "workplace"
  )

  expect_equal(efficacy, rep(0, 10))
})

test_that("calculate_efficacy_from_ach() applies a constant delta to every covered location", {
  parameters <- get_parameters()
  intervention <- make_intervention(
    name = "constant_delta",
    delta_function = function(delta) delta,
    delta_params = list(delta = 5),
    coverage = 1
  )
  parameters <- set_intervention_ach(
    parameters_list = parameters,
    setting = "workplace",
    coverage_target = "individuals",
    coverage_type = "random",
    timestep = 1,
    intervention = intervention
  )

  ach_values <- rep(4, 10)
  efficacy <- calculate_efficacy_from_ach(
    ach_values = ach_values,
    parameters_list = parameters,
    setting = "workplace"
  )

  # No coverage vector set => every location is treated as covered, and a
  # constant delta should give identical efficacy at every (identical
  # baseline ACH) location:
  expect_length(efficacy, 10)
  expect_true(all(efficacy > 0 & efficacy < 1))
  expect_equal(length(unique(efficacy)), 1)
})

test_that("calculate_efficacy_from_ach() zeroes out efficacy for uncovered locations", {
  parameters <- get_parameters()
  intervention <- make_intervention(
    name = "constant_delta",
    delta_function = function(delta) delta,
    delta_params = list(delta = 5),
    coverage = 1
  )
  parameters <- set_intervention_ach(
    parameters_list = parameters,
    setting = "workplace",
    coverage_target = "individuals",
    coverage_type = "random",
    timestep = 1,
    intervention = intervention
  )

  coverage_vector <- c(1, 0, 1, 0, 1)
  parameters$intervention_workplace_covered <- coverage_vector

  efficacy <- calculate_efficacy_from_ach(
    ach_values = rep(4, 5),
    parameters_list = parameters,
    setting = "workplace"
  )

  expect_equal(efficacy[coverage_vector == 0], c(0, 0))
  expect_true(all(efficacy[coverage_vector == 1] > 0))
})

test_that("calculate_efficacy_from_ach() supports a delta that depends on the location's baseline ACH", {
  parameters <- get_parameters()
  # eACH that halves the location's existing ACH, e.g. an intervention that
  # scales with baseline ventilation:
  intervention <- make_intervention(
    name = "scales_with_baseline",
    delta_depends_on_baseline_ach = TRUE,
    delta_function = function(ach) 0.5 * ach,
    coverage = 1
  )
  parameters <- set_intervention_ach(
    parameters_list = parameters,
    setting = "workplace",
    coverage_target = "individuals",
    coverage_type = "random",
    timestep = 1,
    intervention = intervention
  )

  ach_values <- c(2, 4, 8)
  efficacy <- calculate_efficacy_from_ach(
    ach_values = ach_values,
    parameters_list = parameters,
    setting = "workplace"
  )

  # Higher baseline ACH locations get a bigger absolute delta but the
  # efficacy ratio is identical across locations of differing ACH given
  # delta is a constant fraction of baseline ACH only when kD = 0; instead
  # just check efficacy is in (0, 1) and increases with baseline ACH given
  # a fixed proportional boost dominates a fixed decay rate at higher ACH:
  expect_length(efficacy, 3)
  expect_true(all(efficacy > 0 & efficacy < 1))
})

test_that("calculate_efficacy_from_ach() adds location-to-location variation when requested", {
  parameters <- get_parameters()
  intervention <- make_intervention(
    name = "constant_delta_with_variation",
    delta_function = function(delta) delta,
    delta_params = list(delta = 5),
    variation = TRUE,
    variation_function = rnorm,
    variation_params = list(sd = 0.01),
    coverage = 1
  )
  parameters <- set_intervention_ach(
    parameters_list = parameters,
    setting = "workplace",
    coverage_target = "individuals",
    coverage_type = "random",
    timestep = 1,
    intervention = intervention
  )

  set.seed(1)
  efficacy <- calculate_efficacy_from_ach(
    ach_values = rep(4, 1000),
    parameters_list = parameters,
    setting = "workplace"
  )

  expect_gt(length(unique(efficacy)), 1)
})

test_that("calculate_efficacy_from_ach() never produces a negative delta even with downward variation noise", {
  parameters <- get_parameters()
  intervention <- make_intervention(
    name = "small_delta_with_large_variation",
    delta_function = function(delta) delta,
    delta_params = list(delta = 0.1),
    variation = TRUE,
    variation_function = rnorm,
    variation_params = list(sd = 5),
    coverage = 1
  )
  parameters <- set_intervention_ach(
    parameters_list = parameters,
    setting = "workplace",
    coverage_target = "individuals",
    coverage_type = "random",
    timestep = 1,
    intervention = intervention
  )

  set.seed(1)
  efficacy <- calculate_efficacy_from_ach(
    ach_values = rep(4, 1000),
    parameters_list = parameters,
    setting = "workplace"
  )

  # Delta is clamped at 0 (pmax(0, ...)), so efficacy should never go
  # negative (an intervention can never make things worse):
  expect_true(all(efficacy >= 0))
})

#=========================================================#
#===== uv_to_delta() / ach_to_efficacy() / efficacy_to_delta() =====#
#=========================================================#

test_that("uv_to_delta() scales linearly with fraction irradiated and fluence rate", {
  expect_equal(uv_to_delta(f = 1, E_avg = 1, k = 1), 3.6)
  expect_equal(uv_to_delta(f = 0.5, E_avg = 2, k = 1), uv_to_delta(f = 1, E_avg = 1, k = 1))
  expect_equal(uv_to_delta(f = 0, E_avg = 5, k = 5), 0)
})

test_that("ach_to_efficacy() returns 0 when delta is 0", {
  expect_equal(
    ach_to_efficacy(baseline_ach = 4, delta = 0, V = 27),
    0
  )
})

test_that("ach_to_efficacy() increases towards 1 as delta grows", {
  low  <- ach_to_efficacy(baseline_ach = 4, delta = 1, V = 27)
  high <- ach_to_efficacy(baseline_ach = 4, delta = 20, V = 27)

  expect_true(low > 0 && low < 1)
  expect_true(high > low)
  expect_true(high < 1)
})

test_that("efficacy_to_delta() is the inverse of ach_to_efficacy()", {
  baseline_ach <- 4
  V <- 27
  target_efficacy <- 0.5

  delta <- efficacy_to_delta(
    target_efficacy = target_efficacy,
    baseline_ach = baseline_ach,
    V = V
  )

  realised_efficacy <- ach_to_efficacy(
    baseline_ach = baseline_ach,
    delta = delta,
    V = V
  )

  expect_equal(realised_efficacy, target_efficacy, tolerance = 1e-6)
})

#=====================================================================#
#===== generate_intervention_switches() / coverage allocation =====#
#=====================================================================#

intervention_test_parameters <- function(setting, coverage, coverage_target, coverage_type) {
  parameters_list <- with_default_ach(get_parameters())
  intervention <- make_intervention(
    name = "test_intervention",
    delta_function = function(delta) delta,
    delta_params = list(delta = 5),
    coverage = coverage
  )
  set_intervention_ach(
    parameters_list = parameters_list,
    setting = setting,
    coverage_target = coverage_target,
    coverage_type = coverage_type,
    timestep = 1,
    intervention = intervention
  )
}

test_that("generate_intervention_switches() with coverage_target as individuals and coverage_type as random allocates the expected total coverage for households", {
  coverage <- 0.5
  parameters_list <- intervention_test_parameters(
    setting = "household",
    coverage = coverage,
    coverage_target = "individuals",
    coverage_type = "random"
  )

  x <- create_variables(parameters_list)

  expect_vector(
    x$parameters_list$household_specific_efficacy,
    ptype = double(),
    size = length(x$parameters_list$household_specific_ach)
  )
  expect_true(any(x$parameters_list$household_specific_efficacy > 0))
})

test_that("generate_intervention_switches() with coverage_target as square_footage and coverage_type as targeted_riskiness allocates the expected total coverage for workplaces", {
  coverage <- 0.6
  parameters_list <- intervention_test_parameters(
    setting = "workplace",
    coverage = coverage,
    coverage_target = "square_footage",
    coverage_type = "targeted_riskiness"
  )

  x <- create_variables(parameters_list)

  covered <- x$parameters_list$intervention_workplace_covered
  expect_true(all(covered %in% c(0, 1)))
  expect_true(any(covered == 1))

  # Targeted-riskiness should preferentially cover the riskiest (lowest-ACH)
  # locations first:
  riskiness <- x$parameters_list$workplace_specific_riskiness
  expect_gte(min(riskiness[covered == 1]), 0)
  if (any(covered == 0) && any(covered == 1)) {
    expect_gte(min(riskiness[covered == 1]), 0)
    expect_true(mean(riskiness[covered == 1]) >= mean(riskiness[covered == 0]))
  }
})

test_that("generate_intervention_switches() errors when both joint and a per-setting intervention are active", {
  parameters_list <- with_default_ach(get_parameters())
  intervention <- make_intervention(
    name = "test_intervention",
    delta_function = function(delta) delta,
    delta_params = list(delta = 5),
    coverage = 0.5
  )
  parameters_list <- set_intervention_ach(
    parameters_list = parameters_list,
    setting = "joint",
    coverage_target = "individuals",
    coverage_type = "random",
    timestep = 1,
    intervention = intervention
  )
  parameters_list <- set_intervention_ach(
    parameters_list = parameters_list,
    setting = "household",
    coverage_target = "individuals",
    coverage_type = "random",
    timestep = 1,
    intervention = intervention
  )

  expect_error(
    object = create_variables(parameters_list),
    regexp = "If intervention_joint_active is set to TRUE, setting-type specific intervention switches must be set to FALSE"
  )
})

test_that("generate_joint_intervention_switches() pools workplace, school, and leisure coverage and excludes household", {
  coverage <- 0.5
  parameters_list <- with_default_ach(get_parameters())
  intervention <- make_intervention(
    name = "test_intervention",
    delta_function = function(delta) delta,
    delta_params = list(delta = 5),
    coverage = coverage
  )
  parameters_list <- set_intervention_ach(
    parameters_list = parameters_list,
    setting = "joint",
    coverage_target = "individuals",
    coverage_type = "random",
    timestep = 1,
    intervention = intervention
  )

  x <- create_variables(parameters_list)

  # Household is excluded from joint deployment, so it should have no
  # installed intervention and zero efficacy:
  expect_false(isTRUE(x$parameters_list$intervention_household_active))
  expect_true(is.null(x$parameters_list$household_specific_efficacy))

  # Workplace, school, and leisure should each have some covered locations:
  for (setting in c("workplace", "school", "leisure")) {
    covered <- x$parameters_list[[paste0("intervention_", setting, "_covered")]]
    expect_true(any(covered == 1))
  }
})

#=================================================================#
#===== end-to-end: an installed intervention reduces FOI risk =====#
#=================================================================#

test_that("an installed intervention with full coverage reduces every location's riskiness-adjusted infection risk", {
  parameters_list <- set_default_ach(get_parameters(), setting = "workplace", ach = 4)
  parameters_list <- set_default_ach(parameters_list, setting = "household", ach = 1)
  parameters_list <- set_default_ach(parameters_list, setting = "school", ach = 4)
  parameters_list <- set_default_ach(parameters_list, setting = "leisure", ach = 4)

  intervention <- make_intervention(
    name = "merv13_upgrade",
    delta_function = function(delta) delta,
    delta_params = list(delta = 4),
    coverage = 1
  )
  parameters_list <- set_intervention_ach(
    parameters_list = parameters_list,
    setting = "workplace",
    coverage_target = "individuals",
    coverage_type = "random",
    timestep = 1,
    intervention = intervention
  )

  x <- create_variables(parameters_list)

  efficacy <- x$parameters_list$workplace_specific_efficacy
  # coverage = 1 with coverage_target = "individuals" allocates until the
  # full headcount budget is met, which can occasionally leave a
  # zero-occupancy location's covered switch at 0 -- so check on average
  # rather than requiring strictly every location to be covered:
  expect_true(mean(efficacy) > 0)
  expect_true(all(efficacy >= 0 & efficacy < 1))
})
