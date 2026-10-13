# Tests for the S -> E process (create_SE_process()).
#
# The per-person force of infection (FOI) is pinned down three ways:
#  1. against values recorded from the ORIGINAL per-location-loop implementation
#     (tests/testthat/fixtures/se_foi_golden.rds, regenerated as described in
#     helper-se-fixture.R),
#  2. against hand-derived numbers for individual people,
#  3. against a deliberately naive per-person oracle (reference_se_foi()), which is
#     also used to check the daily random leisure redraw.
# The population and scenarios are defined in helper-se-fixture.R.

golden_foi <- readRDS(test_path("fixtures", "se_foi_golden.rds"))

expect_foi_equal <- function(object, expected, label) {
  result <- all.equal(object, expected, tolerance = 1e-12)
  expect_true(isTRUE(result), info = paste(label, paste(result, collapse = "; ")))
}

# ---- 1. golden values from the original implementation -----------------------

test_that("per-person FOI matches the values recorded from the original loop implementation", {
  for (scenario in names(se_golden_scenarios())) {
    current <- capture_se_foi(se_golden_scenarios()[[scenario]])
    expect_identical(names(current), names(golden_foi[[scenario]]))
    for (step in names(current)) {
      expect_foi_equal(
        current[[step]], golden_foi[[scenario]][[step]],
        label = paste(scenario, step)
      )
    }
  }
})

test_that("golden scenarios cover the requested edge cases", {
  expect_setequal(
    names(golden_foi),
    c("baseline", "riskiness_off_explicit", "riskiness_on", "uvc_before_switch_on",
      "uvc_after_switch_on", "uvc_no_riskiness", "uvc_single_setting", "no_infectious",
      "only_hospitalised_and_single", "everyone_infectious", "time_varying_betas",
      "distinct_betas", "dt_quarter")
  )
  # Interventions switch on strictly after their timestep (t > timestep):
  uvc <- se_golden_scenarios()$uvc_after_switch_on$uvc
  expect_equal(uvc$household$timestep, 2)
})

# ---- 2. hand-derived values -------------------------------------------------

test_that("FOI of specific people equals the hand-calculated sum of setting terms", {
  # Default fixture: all betas 0.5 except community 0.2, riskiness 1, 7 infectious.
  community <- 0.2 * 7 / 36
  at_t2 <- capture_se_foi(list(), steps = 2)$t2 # t = 2: first day boundary, leisure drawn

  # person 5: household {5, 6} 1/2 infectious; workplace {2, 5, 6, 8, 9} 2/5; leisure venue 2 {1, 2, 3, 5, 6} 2/5
  expect_equal(at_t2[5], 0.5 * 1 / 2 + 0.5 * 2 / 5 + 0.5 * 2 / 5 + community, tolerance = 1e-12)
  # person 7: elderly, single-person household, no school/workplace/leisure -> community only
  expect_equal(at_t2[7], community, tolerance = 1e-12)
  # person 17: infectious, elderly, single-person household -> household term must be 0
  expect_equal(at_t2[17], community, tolerance = 1e-12)
  # person 19: infectious. Household {18..22}: 1/5 (themselves); single-person workplace: 1/1
  expect_equal(at_t2[19], 0.5 * 1 / 5 + 0.5 * 1 / 1 + community, tolerance = 1e-12)
  # person 11: household's only other case (12) is hospitalised, so no household term;
  # workplace {11, 12, 15} has no I_mild; leisure venue 5 {8, 9, 11, 12, 14} has one (8)
  expect_equal(at_t2[11], 0.5 * 1 / 5 + community, tolerance = 1e-12)
  # person 3: child. household {1..4}: 1/4; school {3, 4, 10, 13, 1}: 1/5; leisure venue 2: 2/5
  expect_equal(at_t2[3], 0.5 * 1 / 4 + 0.5 * 1 / 5 + 0.5 * 2 / 5 + community, tolerance = 1e-12)
  # person 21: school {20, 21, 22, 18} holds no I_mild (22 is hospitalised): school term 0;
  # household {18..22} holds one case (19)
  expect_equal(at_t2[21], 0.5 * 1 / 5 + community, tolerance = 1e-12)
  # person 31: household {27..32} holds case 29 (1/6); workplace 5 holds case 33 (1/8);
  # leisure venue 7 {27, 31} has no infectious visitor, so a zero-infectious location adds 0
  expect_equal(at_t2[31], 0.5 * 1 / 6 + 0.5 * 1 / 8 + community, tolerance = 1e-12)
})

test_that("leisure FOI is zero until the first daily redraw (t * dt a whole number)", {
  by_step <- capture_se_foi(list(), steps = 1:3)
  community <- 0.2 * 7 / 36
  # t = 1 -> t * dt = 0.5: nobody has been assigned a venue yet, so person 5 has no leisure term
  expect_equal(by_step$t1[5], 0.5 * 1 / 2 + 0.5 * 2 / 5 + community, tolerance = 1e-12)
  # t = 2 -> day boundary: assignment drawn. t = 3: assignment kept from t = 2.
  expect_equal(by_step$t2[5], by_step$t3[5], tolerance = 1e-12)
  expect_gt(by_step$t2[5], by_step$t1[5])
})

test_that("interventions apply strictly after the switch-on timestep and only to switched-on locations", {
  uvc <- list(
    workplace = list(switch = c(1, 0, 0, 0, 0), efficacy = 0.5, timestep = 2)
  )
  fixture <- make_se_fixture(uvc = uvc, riskiness = FALSE)
  probe <- se_foi_probe(fixture)
  dqrng::dqset.seed(1)
  community <- 0.2 * 7 / 36
  at_t2 <- probe(2) # t == timestep -> not yet active
  at_t3 <- probe(3) # t > timestep -> workplace 1 halved
  expect_equal(at_t2[5], 0.5 * 1 / 2 + 0.5 * 2 / 5 + 0.5 * 2 / 5 + community, tolerance = 1e-12)
  expect_equal(at_t3[5], 0.5 * 1 / 2 + 0.5 * (1 - 0.5) * 2 / 5 + 0.5 * 2 / 5 + community, tolerance = 1e-12)
  # person 11 sits in workplace 2, which has no intervention
  expect_equal(at_t3[11], at_t2[11], tolerance = 1e-12)
})

test_that("riskiness multiplies the setting term and is neutral when switched off", {
  riskier <- make_se_fixture(riskiness = TRUE)
  probe <- se_foi_probe(riskier)
  dqrng::dqset.seed(1)
  foi <- probe(2)
  community <- 0.2 * 7 / 36
  p <- riskier$parameters_list
  # person 7: community only, unaffected by any riskiness
  expect_equal(foi[7], community, tolerance = 1e-12)
  # person 5: household 2, workplace 1, leisure position 1 (venue id 2)
  expect_equal(
    foi[5],
    p$household_specific_riskiness[2] * 0.5 * 1 / 2 +
      p$workplace_specific_riskiness[1] * 0.5 * 2 / 5 +
      p$leisure_specific_riskiness[1] * 0.5 * 2 / 5 + community,
    tolerance = 1e-12
  )
})

# ---- 3. naive oracle, including the random daily leisure draw ---------------

# One person at a time, no vectorisation, no bitsets: for every person, look up
# their household / workplace / school / leisure venue and count infectious
# members directly. `visit` is the leisure venue id each person goes to today.
reference_se_foi <- function(fixture, t, visit) {
  p <- fixture$parameters_list
  state <- fixture$variables_list$disease_state$get_values()
  infectious <- state == "I_mild"
  ids <- se_fixture_ids()
  leisure_ids <- p$leisure_indices[p$leisure_indices != 0]
  beta <- function(x) if (p$time_varying_transmission_on) x[ceiling(t * p$dt)] else x

  term <- function(location, riskiness, beta_t, on, timestep, efficacy, skip_single = FALSE) {
    vapply(seq_along(location), function(i) {
      l <- location[i]
      if (l == 0) return(0)
      members <- which(location == l)
      if (skip_single && length(members) == 1) return(0)
      base <- beta_t * sum(infectious[members]) / length(members)
      if (on && t > timestep) (1 - efficacy[l]) * riskiness[l] * base else riskiness[l] * base
    }, numeric(1))
  }
  setting <- function(name, location) {
    on <- isTRUE(p[[paste0("intervention_", name, "_active")]])
    term(location, p[[paste0(name, "_specific_riskiness")]], beta(p[[paste0("beta_", name)]]),
         on, p[[paste0("intervention_", name, "_timestep")]],
         p[[paste0(name, "_specific_efficacy")]], skip_single = (name == "household"))
  }
  leisure_pos <- match(visit, leisure_ids, nomatch = 0)
  community <- beta(p$beta_community) * sum(infectious) / p$human_population
  setting("household", ids$household) + setting("workplace", ids$workplace) +
    setting("school", ids$school) + setting("leisure", leisure_pos) + community
}

test_that("random daily leisure draws reproduce the per-person draws of the original loop", {
  mixed <- se_fixture_leisure()
  mixed[[1]] <- c(0, 2, 2, 5, 7, 0, 5)
  mixed[[6]] <- c(7, 7, 0, 2, 5, 0, 2)
  mixed[[8]] <- c(5, 0, 0, 0, 2, 7, 7)
  mixed[[26]] <- c(2, 5, 7, 2, 5, 7, 0)
  mixed[[33]] <- c(0, 0, 0, 0, 0, 0, 7)
  scenario <- list(
    leisure_visits = mixed, riskiness = TRUE,
    uvc = list(leisure = list(switch = c(1, 0, 1), efficacy = 0.6, timestep = 3))
  )
  for (seed in c(11, 12, 13)) {
    fixture <- do.call(make_se_fixture, scenario)
    probe <- se_foi_probe(fixture)
    # What the original per-person loop draws, from the same dqrng state:
    dqrng::dqset.seed(seed)
    visits_by_day <- list()
    for (day in 1:3) {
      visits_by_day[[day]] <- vapply(seq_len(se_fixture_n), function(i) {
        mixed[[i]][dqrng::dqsample.int(n = 7, size = 1)]
      }, numeric(1))
    }
    dqrng::dqset.seed(seed)
    for (t in 1:6) { # t = 2, 4, 6 are day boundaries (dt = 0.5)
      foi <- probe(t)
      day <- floor(t * 0.5) # the day whose draw is active at step t
      visit <- if (day == 0) rep(0, se_fixture_n) else visits_by_day[[day]]
      expect_foi_equal(foi, reference_se_foi(fixture, t, visit), label = paste("seed", seed, "t", t))
    }
  }
})

test_that("a process created mid-day (a resumed run) picks up the leisure assignment saved in specific_leisure", {
  # When a run is resumed from a saved state, create_SE_process() is called afresh and
  # the first resumed step is usually NOT a redraw step, so the assignment drawn on the
  # last step of the first leg has to come back from the specific_leisure variable.
  fixture <- make_se_fixture(riskiness = TRUE)
  first_leg <- se_foi_probe(fixture)
  dqrng::dqset.seed(5)
  first_leg(1)
  first_leg(2) # day boundary: today's venues are drawn and queued
  fixture$variables_list$specific_leisure$.update() # what the simulation loop does at the end of the step
  saved <- fixture$variables_list$specific_leisure$get_values()
  expect_gt(sum(saved > 0), 0)
  expect_true(all(saved %in% c(0, 2, 5, 7)))

  expected_next_step <- first_leg(3) # same process, no redraw
  resumed <- se_foi_probe(fixture) # a new process on the same (restored) variables
  expect_foi_equal(resumed(3), expected_next_step, label = "first resumed step")
})

test_that("the naive oracle agrees with the golden scenarios (so it is a fair referee)", {
  # All golden scenarios use deterministic leisure lists, so the venue visited is
  # the single non-zero id in each person's list (or none before the first draw).
  leisure <- se_fixture_leisure()
  venue <- vapply(leisure, function(v) v[1], numeric(1))
  for (scenario in c("riskiness_on", "uvc_after_switch_on", "time_varying_betas", "distinct_betas")) {
    fixture <- do.call(make_se_fixture, se_golden_scenarios()[[scenario]])
    for (t in c(1, 2, 3)) { # before t = 4 the infectious set is the initial one
      visit <- if (floor(t * fixture$parameters_list$dt) == 0) rep(0, se_fixture_n) else venue
      expect_foi_equal(
        reference_se_foi(fixture, t, visit), golden_foi[[scenario]][[paste0("t", t)]],
        label = paste("oracle vs golden", scenario, t)
      )
    }
  }
})

# ---- 4. whole-process behaviour ---------------------------------------------

test_that("the S -> E process infects nobody when nobody is infectious and there is no force of infection", {
  fixture <- make_se_fixture(infectious = integer(0), hospitalised = integer(0))
  process <- create_SE_process(
    fixture$variables_list, list(), fixture$parameters_list, individual::Render$new(5)
  )
  dqrng::dqset.seed(3)
  process(2)
  ds <- fixture$variables_list$disease_state
  ds$.update()
  expect_equal(ds$get_size_of("E"), 1) # only the person who was already exposed
})

test_that("a seeded simulation (R and dqrng seeds) is reproducible", {
  params <- get_parameters(list(
    human_population = 600, number_initial_S = 595, number_initial_E = 5,
    simulation_time = 12, seed = 77
  ))
  for (setting in c("household", "workplace", "school", "leisure")) {
    params <- set_default_ach(params, setting, ach = 2)
  }
  run <- function() {
    dqrng::dqset.seed(77)
    run_simulation(params)$result
  }
  expect_identical(run(), run())
})
