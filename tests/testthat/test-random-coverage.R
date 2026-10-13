#=========================================================#
#===== select_random_locations (random coverage rule) =====#
#=========================================================#

test_that("select_random_locations picks the same locations, errors and RNG state as the original loop", {
  random_state <- function() get(".Random.seed", envir = globalenv())
  run <- function(fun, sizes, target, seed) {
    set.seed(seed)
    result <- tryCatch(fun(sizes, target, "not enough"), error = function(e) conditionMessage(e))
    list(result = result, state = random_state())
  }

  set.seed(99)
  for (num_locations in c(1, 2, 3, 10, 200, 1000)) {
    sizes <- sample(1:8, num_locations, replace = TRUE)
    for (coverage in c(0, 0.1, 0.5, 0.9, 1)) {
      target <- floor(coverage * sum(sizes))
      for (seed in 1:6) {
        info <- paste("locations", num_locations, "coverage", coverage, "seed", seed)
        expect_identical(
          run(select_random_locations, sizes, target, seed),
          run(reference_select_random_locations, sizes, target, seed),
          info = info
        )
      }
    }
  }
  # a target that cannot be reached, and lots of tied sizes
  for (seed in 1:3) {
    expect_identical(
      run(select_random_locations, 5, 10, seed),
      run(reference_select_random_locations, 5, 10, seed)
    )
    expect_identical(
      run(select_random_locations, rep(1, 50), 50, seed),
      run(reference_select_random_locations, rep(1, 50), 50, seed)
    )
  }
})

test_that("select_random_locations stops as soon as the target is reached, without repeats", {
  set.seed(5)
  sizes <- sample(1:10, 500, replace = TRUE)
  target <- floor(0.4 * sum(sizes))
  picked <- select_random_locations(sizes, target, "not enough")

  expect_false(anyNA(picked))
  expect_false(anyDuplicated(picked) > 0)
  expect_gte(sum(sizes[picked]), target)
  expect_lt(sum(sizes[picked[-length(picked)]]), target)
})

test_that("select_random_locations returns nothing for a zero target and errors when the target cannot be met", {
  expect_null(select_random_locations(c(3, 4, 5), 0, "not enough"))
  expect_error(select_random_locations(c(3, 4, 5), 100, "not enough"), regexp = "not enough")
})

test_that("random intervention coverage covers at least the requested number of individuals", {
  parameters_list <- with_default_ach(get_parameters(list(
    human_population = 3000,
    number_initial_S = 2995,
    number_initial_E = 5,
    seed = 13
  )))
  intervention <- make_intervention(
    "constant",
    delta_function = function() 2,
    coverage = 0.4
  )
  for (setting in c("workplace", "school", "household")) {
    parameters_list <- set_intervention_ach(
      parameters_list, setting, "individuals", "random", 5, intervention
    )
  }
  x <- suppressMessages(create_variables(parameters_list))

  for (setting in c("workplace", "school", "household")) {
    covered <- x$parameters_list[[paste0("intervention_", setting, "_covered")]]
    sizes <- get_setting_size(x$variables_list, setting = setting)
    expect_true(all(covered %in% c(0, 1)), info = setting)
    expect_gte(sum(sizes[covered == 1]), floor(0.4 * sum(sizes)))
  }
})
