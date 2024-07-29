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

min(x)
max(x)

x <- rlnormTrunc(n = 100000, meanlog = 0, sdlog = 0.3689928, min = 1/sqrt(5), max = sqrt(5)) # min and max produce settings as much as 5x different to each other in riskiness
mean(x)
hist(x)
hist(x/mean(x))

# Function to calculate the difference between desired and actual tail mass
# tail_mass_diff <- function(sdlog, ratio) {
#
#   min <- 1/sqrt(ratio)
#   max <- sqrt(ratio)
#
#   # Calculate CDF values at the points of interest for tail mass
#   cdf_lower_tail_start <- plnormTrunc(q = min, meanlog = 0, sdlog, min = min, max = max)
#   cdf_lower_tail_end <- plnormTrunc(q = 0.6667, meanlog = 0, sdlog, min = min, max = max)
#   cdf_upper_tail_start <- plnormTrunc(q = 1/0.6667, meanlog = 0, sdlog, min = min, max = max)
#   cdf_upper_tail_end <- plnormTrunc(q = max, meanlog = 0, sdlog, min = min, max = max)
#
#   # Calculate actual tail masses
#   lower_tail_mass <- cdf_lower_tail_end - cdf_lower_tail_start
#   upper_tail_mass <- cdf_upper_tail_end - cdf_upper_tail_start
#
#   # Calculate the total difference from desired tail masses
#   total_diff <- abs(lower_tail_mass - 0.025) + abs(upper_tail_mass - 0.025)
#
#   return(total_diff)
# }

### what this does is find the best sdlog given constraints of:
### 1) min and max given by the ratio of most to least well ventilated
### 2) assumption that 80% of the probability density falls in the IQR of the ratio
###    (on the multiplicative scale). I.e. if the ratio was 4, this would imply the
###    least risky is 0.5, most risky is 2 (2/0.5 = 4). We assume that 80% of probability
###    density lies between a 2x riskiness difference, i.e. between 1 - sqrt(2) and sqrt(2)
###    (i.e. 0.7071 and 1.41, where 1.41/0.7071 = 2)
tail_mass_diff2 <- function(sdlog, ratio) {

  min <- 1/sqrt(ratio)
  max <- sqrt(ratio)

  lower_quartile <- 1 - 1/sqrt(ratio/2)
  upper_quartile <- sqrt(ratio/2)

  # Calculate CDF values at the points of interest for tail mass
  cdf_lower_tail_start <- plnormTrunc(q = min, meanlog = 0, sdlog, min = min, max = max)
  cdf_lower_tail_end <- plnormTrunc(q = lower_quartile, meanlog = 0, sdlog, min = min, max = max)
  cdf_upper_tail_start <- plnormTrunc(q = upper_quartile, meanlog = 0, sdlog, min = min, max = max)
  cdf_upper_tail_end <- plnormTrunc(q = max, meanlog = 0, sdlog, min = min, max = max)

  # Calculate actual tail masses
  lower_tail_mass <- cdf_lower_tail_end - cdf_lower_tail_start
  upper_tail_mass <- cdf_upper_tail_end - cdf_upper_tail_start

  # Calculate the total difference from desired tail masses
  total_diff <- abs(lower_tail_mass - 0.1) + abs(upper_tail_mass - 0.1)

  return(total_diff)
}

ratio <- 6.35
result <- optimize(f = tail_mass_diff2,
                   ratio = ratio,
                   interval = c(0.01, 3),
                   tol = 1e-9)

x <- rlnormTrunc(n = n, meanlog = 0, sdlog = result$minimum, min = 1/sqrt(ratio), max = sqrt(ratio)) # min and max produce settings as much as 5x different to each other in riskiness
hist(x, breaks = 50, xlim = c(0, 3))

min(x)
max(x)
max(x) / min(x)
