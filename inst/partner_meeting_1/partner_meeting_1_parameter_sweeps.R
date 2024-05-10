#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+++++ Partner Meeting 1: Parameter Sweeps +++++#
#+++++++++++++++++++++++++++++++++++++++++++++++#



# Transmission Proportions:
beta_household <- 0.3
beta_school <- 0.2
beta_workplace <- 0.2
beta_leisure <- 0.2
beta_community <- 0.1

# Set up the R0s to be simulated
R0s <- c(1.25, 1.5, 2, 2.5, 3)

# Set up the far-UVC coverages to simulate:
favr_uvc_coverage <- seq(from = 0, to = 1, by = 0.2)

# Set up the far-UVC efficacies to simulate
favr_uvc_efficacy <- seq(from = 0, to = 1, by = 0.2)

# Set up the coverage types to be simulated
coverage_type <- c("random", "targeted")

# Number of iterations to simulate for each parameterisation:
iterations <- 5

# Set up the unique simulations to run
simulations_to_run <- expand.grid(R0s,
                                  favr_uvc_coverage,
                                  favr_uvc_efficacy,
                                  coverage_type)

# Calculate the number of simulations that will be run:
nrow(simulations_to_run) * iterations
