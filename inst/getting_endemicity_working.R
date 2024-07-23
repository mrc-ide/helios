# Loading helios
library(helios); library(tictoc)

# Calculating approximate equilibrium solution for flu
human_pop <- 2000
simulation_time <- 5000
parameters_list <- get_parameters(overrides = list(human_population = human_pop,
                                                   endemic_or_epidemic = "endemic",
                                                   number_initially_exposed = 15,
                                                   duration_immune = 50,
                                                   prob_inf_external = 0.1,
                                                   simulation_time = simulation_time),
                                  archetype = "sars_cov_2")

# Running the model
tic()
output <- run_simulation(parameters_list)
toc()

plot(output$timestep, output$S_count, type = "l", ylim = c(0, human_pop), xlim = c(0, simulation_time))
lines(output$timestep, output$E_count, col = "red")
lines(output$timestep, output$I_count, col = "green")
lines(output$timestep, output$R_count, col = "purple")

## with population of 50,000 - running 20 days with 0.5 day timestep (40 total) = 90 seconds
## with population of 50,000 - running 40 days with 0.5 day timestep (80 total) = 160 seconds
## -> approximately 0.25 days (0.50 timesteps) per second, or 1 day every 4 seconds
## Running 2500 days (5000 timesteps) with 50,000 population would take approx 10,000 seconds (or 3 hours)

## For flu - runtime should be X days (Y burnin + 365 with UVC)
## For SC2 - runtime should be A days (B burnin + 365 with UVC)

## need to check how simulation time scales with population size
## 5000 pop with flu and duration immune 1 year takes X hours to run

