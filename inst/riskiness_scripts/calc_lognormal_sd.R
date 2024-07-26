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

x <- rlnormTrunc(n = 100000, meanlog = 0, sdlog = 0.3689928, min = 1/sqrt(5), max = sqrt(5)) # min and max produce settings as much as 5x different to each other in riskiness
mean(x)
hist(x)
hist(x/mean(x))

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


#' #' generate_setting_specific_riskiness
#' #'
#' #' @description
#' #' generate_setting_specific_riskiness() takes a mean, sd (on the log scale), a minimum and maximum and
#' #' returns n draws from a truncated lognormal distribution. These draws form the basis for setting-specific
#' #' riskiness (effectively a setting-specific modifier of the FOI experienced by an individual). The function
#' #' returns a vector containing the truncated lognormal draws.
#' #'
#' #' @param n The number of draws required
#' #' @param meanlog Log10 of the mean of the truncated lognormal distribution (set to 0 if you want th mean to be 1)
#' #' @param sdlog Standard deviation of the truncated lognormal distribution
#' #' @param min  Minimum value draws from the truncated lognormal distribution can take
#' #' @param max  Maxmimum value draws from the truncated lognormal distribution can take
#' #' @family miscellaneous
#' #' @export
#' generate_setting_specific_riskiness <- function(n, meanlog, sdlog, min, max) {
#'
#'   setting_specific_riskiness <- rlnormTrunc(n = n, meanlog = meanlog, sdlog = sdlog, min = min, max = max)
#'
#'
#'
#' }
