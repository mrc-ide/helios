## Verify that no individual is ever scheduled in two mutually-exclusive
## events simultaneously.
##
## We wrap the two binomial-bearing processes so that after they schedule
## events, we check the intersection of the two mutually-exclusive
## scheduled-sets is empty. If the invariant is ever violated, the script
## stops with an informative error.

devtools::load_all()

## ---- Build assertion-wrapped versions of the two processes -----------------

make_checked_EI_process <- function(variables_list, events_list,
                                    parameters_list, renderer) {
  inner <- create_EI_process(variables_list, events_list,
                             parameters_list, renderer)
  function(t) {
    inner(t)
    a <- events_list$EI_mild_event$get_scheduled()
    b <- events_list$EIhosp_event$get_scheduled()
    # intersect a copy of a with b -- $and is in-place, so copy first
    overlap <- a$copy()
    overlap$and(b)
    if (overlap$size() > 0) {
      stop(sprintf(
        "INVARIANT VIOLATED at t=%d: %d individuals scheduled in BOTH EI_mild_event and EIhosp_event",
        t, overlap$size()
      ))
    }
  }
}

make_checked_I_hosp_exit_process <- function(variables_list, events_list,
                                             parameters_list, renderer) {
  inner <- create_I_hosp_exit_process(variables_list, events_list,
                                      parameters_list, renderer)
  function(t) {
    inner(t)
    a <- events_list$I_hosp_R_event$get_scheduled()
    b <- events_list$I_hosp_D_event$get_scheduled()
    overlap <- a$copy()
    overlap$and(b)
    if (overlap$size() > 0) {
      stop(sprintf(
        "INVARIANT VIOLATED at t=%d: %d individuals scheduled in BOTH I_hosp_R_event and I_hosp_D_event",
        t, overlap$size()
      ))
    }
  }
}

## ---- Run a simulation with the wrapped processes ---------------------------

params <- get_parameters(overrides = list(
  human_population = 2000,
  simulation_time = 100,
  number_initial_S = 1980,
  number_initial_E = 20,
  number_initial_I = 0,
  number_initial_R = 0
))

set.seed(params$seed)

variables_list <- create_variables(params)
params <- variables_list$parameters_list
variables_list <- variables_list$variables_list
events_list <- create_events(variables_list, params)

timesteps <- round(params$simulation_time / params$dt)
renderer <- individual::Render$new(timesteps)

processes_list <- list(
  SE_process = create_SE_process(variables_list, events_list, params, renderer),
  EI_process = make_checked_EI_process(variables_list, events_list, params, renderer),
  I_mild_R_process = create_I_mild_R_process(variables_list, events_list, params, renderer),
  I_hosp_exit_process = make_checked_I_hosp_exit_process(variables_list, events_list, params, renderer)
)

processes_list <- c(
  processes_list,
  renderer = individual::categorical_count_renderer_process(
    renderer, variables_list$disease_state,
    c("S", "E", "I_mild", "I_hosp", "R", "D")
  )
)

cat("Running simulation with mutex assertions enabled...\n")
individual::simulation_loop(
  variables = variables_list,
  events = unlist(events_list),
  processes = processes_list,
  timesteps = timesteps
)

cat("\nPASSED: no double-scheduling detected over",
    timesteps, "timesteps.\n")

out <- renderer$to_dataframe()
cat("\nFinal compartment counts:\n")
print(tail(out, 1)[, c("S_count", "E_count", "I_mild_count",
                       "I_hosp_count", "R_count", "D_count")])
