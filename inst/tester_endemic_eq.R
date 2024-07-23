# Loading helios
library(helios); library(tictoc)

# Calculating approximate equilibrium solution for flu
# if(archetype == "flu") {
#   parameters$duration_exposed = 1
#   parameters$duration_infectious = 2
#   parameters$beta_household = 0.132
#   parameters$beta_workplace = 0.132
#   parameters$beta_school = 0.132
#   parameters$beta_leisure = 0.132
#   parameters$beta_community = 0.044
# }

# Checking the new get parameters works
human_pop <- 5000
simulation_time <- 500
parameters_list <- get_parameters(overrides = list(human_population = human_pop,
                                                   number_initial_S = 1500,
                                                   number_initial_E = 100,
                                                   number_initial_I = 200,
                                                   number_initial_R = 3200,
                                                   endemic_or_epidemic = "endemic",
                                                   duration_immune = 365,
                                                   prob_inf_external = 0.000025,
                                                   simulation_time = simulation_time),
                                  archetype = "sars_cov_2")

# Running the model
tic()
output <- run_simulation(parameters_list)
toc()

## with population of 50,000 - running 20 days with 0.5 day timestep (40 total) = 90 seconds
## with population of 50,000 - running 40 days with 0.5 day timestep (80 total) = 160 seconds
## -> approximately 0.25 days (0.50 timesteps) per second, or 1 day every 4 seconds
## Running 2500 days (5000 timesteps) with 50,000 population would take approx 10,000 seconds (or 3 hours)

par(mfrow = c(1, 1))
plot(output$timestep, output$S_count, type = "l", ylim = c(0, human_pop))
lines(output$timestep, output$E_count, col = "red")
lines(output$timestep, output$I_count, col = "green")
lines(output$timestep, output$R_count, col = "purple")

par(mfrow = c(2, 2))
plot(output$timestep, output$S_count, type = "l", ylim = c(0, max(output$S_count)))
plot(output$timestep, output$E_count, col = "red", type = "l", ylim = c(0, max(output$E_count)))
plot(output$timestep, output$I_count, col = "green", type = "l", ylim = c(0, max(output$I_count)))
plot(output$timestep, output$R_count, col = "purple", type = "l", ylim = c(0, max(output$R_count)))


## For flu - runtime should be X days (Y burnin + 365 with UVC)
## For SC2 - runtime should be A days (B burnin + 365 with UVC)

## need to check how simulation time scales with population size
## 5000 pop with flu and duration immune 1 year takes X hours to run

