#' Sample from an offset truncated power distribution
#'
#' Default values adapted from Ferguson et al. (2005). The final sample is
#' adjusted to ensure that the total sum of samples equals to `N`.
#'
#' @param N The total population size
#' @param prop_max The proportion of `N` which is the maximum value of a single sample
#' @param a A parameter of the distribution (?)
#' @param c A parameter of the distribution (?)
#'
#' @export
sample_offset_truncated_power_distribution <- function(N, prop_max = 0.1, a = 5.36, c = 1.34) {
  if (prop_max <= 0 | prop_max > 1) {
    stop("prop_max should be in (0, 1]")
  }
  if (prop_max >= 0.2) {
    warning("prop_max is 0.2 or more! This may be an inappropriate choice.")
  }

  max <- N * prop_max
  m <- 1:max
  cdf <- 1 - (((1 + max / a) / (1 + m / a))^c - 1) / ((1 + max / a)^c - 1)

  samples <- c()
  remaining <- N

  while(remaining > 0) {
    u <- runif(1)
    diff <- abs(cdf - u)
    draw <- m[which(diff == min(diff))]
    samples <- c(samples, draw)
    remaining <- remaining - draw
  }

  if(sum(samples) >= N) {
    samples[length(samples)] <- samples[length(samples)] - (sum(samples) - N)
  }

  return(samples)
}
