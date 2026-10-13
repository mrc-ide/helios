# Fixed, hand-built population used to pin down the S -> E force of infection (FOI).
#
# 36 people, deliberately small so that every edge case is visible by eye:
#
#   households   1: 1-4 | 2: 5-6 | 3: 7 (single, elderly) | 4: 8-10 | 5: 11-14
#                6: 15-16 | 7: 17 (single, elderly) | 8: 18-22 | 9: EMPTY
#                10: 23-26 | 11: 27-32 | 12: 33-36
#   schools      1: kids 3,4,10,13 + staff 1 | 2: kids 20,21,22 + staff 18
#                3: EMPTY | 4: kids 29,30,34,35 + staff 36 | 0: everybody else
#   workplaces   1: 2,5,6,8,9 | 2: 11,12,15 | 3: EMPTY | 4: 19 (single)
#                5: 23-27,31,32,33 | 0: elderly, children, school staff
#   leisure      location ids 2, 5, 7 (ids 1, 3, 4, 6 were never assigned, so the
#                valid ids are c(0, 2, 5, 7)); 0 = stays home. Everyone has the
#                same venue (or none) on all seven days, so the daily redraw is
#                deterministic and cannot disturb the expected FOI.
#
# Disease states of the "default" infectious set:
#   I_mild (infectious): 1, 6, 8, 17, 19, 29, 33
#   I_hosp (hospitalised, not counted as infectious by the process): 12, 22
#   D: 25, R: 26, E: 2, everyone else S
#
# So the fixture contains: elderly people with no school/workplace (7, 14, 16,
# 17, 28), single-person households (3, 7), a single-person workplace (19) that
# contains an infectious person, locations with zero infectious people, empty
# locations in every setting, people who never visit a leisure venue, a venue
# (id 7) with visitors but no infectious visitor, and hospitalised people.

se_fixture_n <- 36L

se_fixture_ids <- function() {
  household <- c(
    rep(1, 4), rep(2, 2), 3, rep(4, 3), rep(5, 4), rep(6, 2), 7,
    rep(8, 5), rep(10, 4), rep(11, 6), rep(12, 4)
  )
  school <- integer(se_fixture_n)
  school[c(3, 4, 10, 13, 1)] <- 1
  school[c(20, 21, 22, 18)] <- 2
  school[c(29, 30, 34, 35, 36)] <- 4
  workplace <- integer(se_fixture_n)
  workplace[c(2, 5, 6, 8, 9)] <- 1
  workplace[c(11, 12, 15)] <- 2
  workplace[19] <- 4
  workplace[c(23:27, 31, 32, 33)] <- 5
  list(household = household, school = school, workplace = workplace)
}

se_fixture_leisure <- function() {
  visits <- replicate(se_fixture_n, rep(0, 7), simplify = FALSE)
  for (i in c(1, 2, 3, 5, 6)) visits[[i]] <- rep(2, 7)
  for (i in c(8, 9, 11, 12, 14)) visits[[i]] <- rep(5, 7)
  for (i in c(27, 31)) visits[[i]] <- rep(7, 7)
  visits
}

# Build `variables_list` and `parameters_list` for the S -> E process.
#
# @param infectious,hospitalised integer vectors of person indices.
# @param riskiness TRUE to use non-trivial per-location riskiness.
# @param uvc named list with elements household/workplace/school/leisure; each
#   is NULL (intervention off) or list(switch = <0/1 vector>, efficacy, timestep).
# @param betas list of beta_* overrides (scalars, or day-vectors if time varying).
# @param leisure_visits list of 7-vectors (possible leisure venue per weekday).
make_se_fixture <- function(
  infectious = c(1, 6, 8, 17, 19, 29, 33),
  hospitalised = c(12, 22),
  riskiness = FALSE,
  uvc = list(),
  betas = list(),
  time_varying = FALSE,
  leisure_visits = se_fixture_leisure(),
  dt = 0.5
) {
  n <- se_fixture_n
  ids <- se_fixture_ids()
  leisure_indices <- c(0, 2, 5, 7)

  state <- rep("S", n)
  state[2] <- "E"
  state[25] <- "D"
  state[26] <- "R"
  state[hospitalised] <- "I_hosp"
  state[infectious] <- "I_mild"

  variables_list <- list(
    disease_state = individual::CategoricalVariable$new(
      c("S", "E", "I_mild", "I_hosp", "R", "D"), state
    ),
    household = individual::CategoricalVariable$new(
      as.character(1:12), as.character(ids$household)
    ),
    school = individual::CategoricalVariable$new(
      as.character(0:4), as.character(ids$school)
    ),
    workplace = individual::CategoricalVariable$new(
      as.character(0:5), as.character(ids$workplace)
    ),
    leisure = individual::RaggedInteger$new(leisure_visits),
    specific_leisure = individual::CategoricalVariable$new(
      as.character(leisure_indices), rep("0", n)
    )
  )

  risk <- function(k, offset) if (riskiness) 0.6 + 0.07 * ((seq_len(k) * 3 + offset) %% 11) else rep(1, k)
  overrides <- c(
    list(
      human_population = n, number_initial_S = n, number_initial_E = 0,
      number_initial_I = 0, number_initial_R = 0, dt = dt, simulation_time = 20,
      time_varying_transmission_on = time_varying
    ),
    betas
  )
  parameters_list <- get_parameters(overrides)
  parameters_list$household_specific_riskiness <- risk(12, 1)
  parameters_list$workplace_specific_riskiness <- risk(5, 2)
  parameters_list$school_specific_riskiness <- risk(4, 3)
  parameters_list$leisure_specific_riskiness <- risk(3, 4)
  parameters_list$leisure_indices <- leisure_indices
  parameters_list$setting_sizes <- list(leisure = c(30, 20, 10))

  for (setting in names(uvc)) {
    u <- uvc[[setting]]
    if (is.null(u)) next
    # PR #142 (ach_efficacy_update): every location of an active setting gets its own
    # efficacy (0 = uncovered), there is no separate 0/1 switch vector.
    parameters_list[[paste0("intervention_", setting, "_active")]] <- TRUE
    parameters_list[[paste0(setting, "_specific_efficacy")]] <- u$switch * u$efficacy
    parameters_list[[paste0("intervention_", setting, "_timestep")]] <- u$timestep
  }
  list(variables_list = variables_list, parameters_list = parameters_list)
}

# Return the S -> E process closure truncated so that it returns the per-person
# total FOI (`total_FOI`) instead of sampling infections. The probe keeps the
# closure environment, so any state held between steps (e.g. the daily leisure
# draw) behaves exactly as it does in a real run. It works on the process before
# and after vectorisation as long as `total_FOI` is a top-level assignment.
se_foi_probe <- function(fixture) {
  process <- create_SE_process(
    variables_list = fixture$variables_list,
    events_list = list(),
    parameters_list = fixture$parameters_list,
    renderer = individual::Render$new(50)
  )
  statements <- as.list(body(process))
  is_total <- vapply(statements, function(e) {
    is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("total_FOI"))
  }, logical(1))
  stopifnot(sum(is_total) == 1)
  body(process) <- as.call(c(statements[seq_len(which(is_total))], quote(total_FOI)))
  process
}

# The scenarios whose FOI is stored in fixtures/se_foi_golden.rds. That file was
# generated once from the original per-location-loop implementation of
# create_SE_process() (PR #142 head, e1258dc). To regenerate it, check out the loop
# implementation, source this file, and run
#   golden <- lapply(se_golden_scenarios(), capture_se_foi)
#   saveRDS(golden, "tests/testthat/fixtures/se_foi_golden.rds", version = 2)
se_golden_scenarios <- function() {
  all_on <- function(timestep, efficacy = 0.7) {
    list(
      household = list(switch = c(1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1), efficacy = efficacy, timestep = timestep),
      workplace = list(switch = c(1, 0, 0, 1, 1), efficacy = efficacy, timestep = timestep),
      school = list(switch = c(1, 1, 0, 0), efficacy = efficacy, timestep = timestep),
      leisure = list(switch = c(0, 1, 1), efficacy = efficacy, timestep = timestep)
    )
  }
  list(
    baseline = list(),
    riskiness_off_explicit = list(riskiness = FALSE),
    riskiness_on = list(riskiness = TRUE),
    uvc_before_switch_on = list(riskiness = TRUE, uvc = all_on(timestep = 100)),
    uvc_after_switch_on = list(riskiness = TRUE, uvc = all_on(timestep = 2)),
    uvc_no_riskiness = list(riskiness = FALSE, uvc = all_on(timestep = 2, efficacy = 0.35)),
    uvc_single_setting = list(riskiness = TRUE, uvc = list(
      workplace = list(switch = c(1, 1, 1, 1, 1), efficacy = 0.9, timestep = 0)
    )),
    no_infectious = list(infectious = integer(0), hospitalised = c(12, 22)),
    only_hospitalised_and_single = list(infectious = 17, hospitalised = c(1, 8, 12, 22, 29)),
    everyone_infectious = list(infectious = setdiff(1:36, c(2, 25, 26)), hospitalised = integer(0)),
    time_varying_betas = list(
      riskiness = TRUE, time_varying = TRUE,
      uvc = all_on(timestep = 4),
      betas = list(
        beta_household = seq(0.30, 0.50, length.out = 20),
        beta_workplace = seq(0.20, 0.45, length.out = 20),
        beta_school = seq(0.10, 0.60, length.out = 20),
        beta_leisure = seq(0.40, 0.25, length.out = 20),
        beta_community = seq(0.05, 0.12, length.out = 20)
      )
    ),
    distinct_betas = list(
      riskiness = TRUE,
      betas = list(beta_household = 0.9, beta_workplace = 0.31, beta_school = 0.47,
                   beta_leisure = 0.22, beta_community = 0.13)
    ),
    dt_quarter = list(riskiness = TRUE, dt = 0.25)
  )
}

# FOI at steps t = 1..8 for one scenario. Between steps the infectious set is
# changed so that any caching of infectious counts across steps would be caught.
# dqrng is seeded because the daily leisure redraw uses it.
capture_se_foi <- function(scenario, steps = 1:8) {
  fixture <- do.call(make_se_fixture, scenario)
  probe <- se_foi_probe(fixture)
  dqrng::dqset.seed(2024)
  out <- vector("list", length(steps))
  for (k in seq_along(steps)) {
    t <- steps[k]
    if (t == 4) { # a new infectious person, and one recovers
      ds <- fixture$variables_list$disease_state
      ds$queue_update("I_mild", individual::Bitset$new(se_fixture_n)$insert(5))
      ds$queue_update("R", individual::Bitset$new(se_fixture_n)$insert(1))
      ds$.update()
    }
    out[[k]] <- probe(t)
  }
  names(out) <- paste0("t", steps)
  out
}
