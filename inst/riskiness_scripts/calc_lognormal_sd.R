# Load required libraries
library(EnvStats); library(stats)

# Checking truncated lognormal giving what we need
n <- 100000
x <- rlnormTrunc(n = n, meanlog = 0, sdlog = 0.3689928, min = 1/sqrt(5), max = sqrt(5)) # min and max produce settings as much as 5x different to each other in riskiness
hist(x, breaks = 50, xlim = c(0, 2.5))
mean(x)
median(x)

sum(x < 0.6667) / n
sum(x > 1/0.6667) / n

# Function to calculate the difference between desired and actual tail mass
tail_mass_diff <- function(sdlog) {

  min <- 1/sqrt(5)
  max <- sqrt(5)

  # Calculate CDF values at the points of interest for tail mass
  cdf_lower_tail_start <- plnormTrunc(q = 1/sqrt(5), meanlog = 0, sdlog, min = min, max = max)
  cdf_lower_tail_end <- plnormTrunc(q = 0.6667, meanlog = 0, sdlog, min = min, max = max)
  cdf_upper_tail_start <- plnormTrunc(q = 1/0.6667, meanlog = 0, sdlog, min = min, max = max)
  cdf_upper_tail_end <- plnormTrunc(q = sqrt(5), meanlog = 0, sdlog, min = min, max = max)

  # Calculate actual tail masses
  lower_tail_mass <- cdf_lower_tail_end - cdf_lower_tail_start
  upper_tail_mass <- cdf_upper_tail_end - cdf_upper_tail_start

  # Calculate the total difference from desired tail masses
  total_diff <- abs(lower_tail_mass - 0.125) + abs(upper_tail_mass - 0.125)

  return(total_diff)
}

result <- optimize(tail_mass_diff, interval = c(0.01, 3), tol = 1e-9)
result
