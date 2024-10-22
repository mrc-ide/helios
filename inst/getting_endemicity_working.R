# Loading helios
library(helios); library(tictoc)

# Calculating approximate equilibrium solution for flu
human_pop <- 10000
archetype <- "flu"
if (archetype == "flu") {
  S <- round(0.66 * human_pop)
  E <- round(0.01 * human_pop)
  I <- round(0.02 * human_pop)
  R <- human_pop - S - E - I
  prob_inf_external <- 0.5 / human_pop
} else {
  S <- round(0.33 * human_pop)
  E <- round(0.03 * human_pop)
  I <- round(0.06 * human_pop)
  R <- human_pop - S - E - I
  prob_inf_external <- 0.5 / human_pop
}
simulation_time <- 7000
parameters_list <- get_parameters(overrides = list(human_population = human_pop,
                                                   endemic_or_epidemic = "endemic",
                                                   number_initial_S = S,
                                                   number_initial_E = E,
                                                   number_initial_I = I,
                                                   number_initial_R = R,
                                                   duration_immune = 365,
                                                   prob_inf_external = 0.5/human_pop,
                                                   simulation_time = simulation_time),
                                  archetype = archetype)
#### check why this isn't working when prob_inf_external is 0 - is this because of the same issue as before with
#### duration of immunity?

# Running the model
tic()
output <- run_simulation(parameters_list)
toc()

par(mfrow = c(1, 3))
plot(output$timestep, output$S_count, type = "l", ylim = c(0, human_pop), xlim = c(0, simulation_time))
lines(output$timestep, output$E_count, col = "red")
lines(output$timestep, output$I_count, col = "green")
lines(output$timestep, output$R_count, col = "purple")
plot(output$timestep, output$E_new, col = "purple", type = "l")
plot(output$timestep, output$n_external_infections, col = "purple", type = "l")

sum(output$n_external_infections[(simulation_time - 730):simulation_time])
sum(output$E_new[(simulation_time - 730):simulation_time])
sum(output$E_new[(simulation_time - 730):simulation_time]) / (sum(output$E_new[(simulation_time - 730):simulation_time]) + sum(output$n_external_infections[(simulation_time - 730):simulation_time]))


## with population of 50,000 - running 20 days with 0.5 day timestep (40 total) = 90 seconds
## with population of 50,000 - running 40 days with 0.5 day timestep (80 total) = 160 seconds
## -> approximately 0.25 days (0.50 timesteps) per second, or 1 day every 4 seconds
## Running 2500 days (5000 timesteps) with 50,000 population would take approx 10,000 seconds (or 3 hours)

## For flu - runtime should be X days (Y burnin + 365 with UVC)
## For SC2 - runtime should be A days (B burnin + 365 with UVC)

## need to check how simulation time scales with population size
## 5000 pop with flu and duration immune 1 year takes X hours to run

