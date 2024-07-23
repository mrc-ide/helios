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
parameters_list <- get_parameters(overrides = list(human_population = 50000,
                                                   number_initial_S = 10 * 1700,
                                                   number_initial_E = 10 * 100,
                                                   number_initial_I = 10 * 200,
                                                   number_initial_R = 10 * 3000,
                                                   endemic_or_epidemic = "endemic",
                                                   duration_immune = 365,
                                                   prob_inf_external = 0.0025,
                                                   simulation_time = 40),
                                  archetype = "flu")

# Checking the new generate_initial_disease_states works
initial_disease_states <- generate_initial_disease_states(parameters_list = parameters_list)

# Running the model
tic()
output <- run_simulation(parameters_list)
toc()

## with population of 50,000 - running 20 days with 0.5 day timestep (40 total) = 90 seconds
## with population of 50,000 - running 40 days with 0.5 day timestep (80 total) = 160 seconds
## -> approximately 0.25 days (0.50 timesteps) per second, or 1 day every 4 seconds
## Running 2500 days (5000 timesteps) with 50,000 population would take approx 10,000 seconds (or 3 hours)

plot(output$timestep, output$S_count, type = "l", ylim = c(0, 2500))
lines(output$timestep, output$E_count, col = "red")
lines(output$timestep, output$I_count, col = "green")
lines(output$timestep, output$R_count, col = "purple")

## For flu - runtime should be X days (Y burnin + 365 with UVC)
## For SC2 - runtime should be A days (B burnin + 365 with UVC)

## need to check how simulation time scales with population size
## 5000 pop with flu and duration immune 1 year takes X hours to run

