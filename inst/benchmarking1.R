# devtools::install_github('mrc-ide/individual')

library(helios); library(tictoc); library(profvis)

parameters_list <- get_parameters(overrides = list(human_population = 10000,
                                                   number_initial_S = 9800,
                                                   number_initial_E = 100,
                                                   number_initial_I = 100,
                                                   number_initial_R = 0,
                                                   simulation_time = 5),
                                  archetype = "sars_cov_2")
variables_list <- create_variables(parameters_list)
parameters_list <- variables_list$parameters_list
variables_list <- variables_list$variables_list
events_list <- create_events(variables_list, parameters_list)
renderer <- individual::Render$new(parameters_list$simulation_time / parameters_list$dt)
x <- create_SE_process(variables_list, events_list, parameters_list, renderer)
timesteps <- round(parameters_list$simulation_time/parameters_list$dt)
renderer <- individual::Render$new(timesteps)
processes_list <- create_processes(variables_list = variables_list,
                                   events_list = events_list,
                                   parameters_list = parameters_list,
                                   renderer = renderer)

profvis({
  x <- individual::simulation_loop(
    variables = variables_list,
    events = unlist(events_list),
    processes = processes_list,
    timesteps = timesteps)
  })

tic()
x <- run_simulation(parameters_list = parameters_list)
toc()

plot(x$timestep, x$S_count, type = "l", ylim = c(0, 10^4))
lines(x$timestep, x$E_count)
lines(x$timestep, x$I_count)
lines(x$timestep, x$R_count)

# 1 minute is 10 days
# 36.5 minutes is 1 year
# 1 hour is 1.65 years
# 10 hours is 16.5 years
# 24 hours is 40 years (approx)

library(profvis)
profvis({
  x(1)
})

tic()
run_simulation(parameters_list)
toc()


profvis({
  run_simulation(parameters_list)
})

spec_household <- household_bitset_list[[i]]
spec_household$to_vector()

I$

spec_household_I <- I$copy()$and(spec_household)
