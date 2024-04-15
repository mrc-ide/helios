#' Sample variables from an offset truncated power distribution with fixed sum
#'
#' Default values adapted from Ferguson et al. (2005). The final sample is
#' adjusted to ensure that the total sum of samples equals to `N`.
#'
#' @param N An integer giving the sum of generated random variables
#' @param prop_max The proportion of `N` which is the maximum value of a single sample
#' @param a A parameter of the distribution (?)
#' @param c A parameter of the distribution (?)
#'
#' @family sampling
#' @export
sample_offset_truncated_power_distribution <- function(N, prop_max = 0.1, a = 5.36, c = 1.34) {
  if (prop_max <= 0 | prop_max > 1) {
    stop("prop_max should be in (0, 1]")
  }
  if (prop_max >= 0.2) {
    warning("prop_max is 0.2 or more! This may be an inappropriate choice.")
  }

  max <- round(N * prop_max)
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

#' Sample variables from an log-normal distribution with fixed sum
#'
#' The final sample is adjusted to ensure that the total sum of samples equals to `N`.
#' All samples are rounded.
#'
#' @inheritParams sample_offset_truncated_power_distribution
#' @param meanlog See the `meanlog` argument of [dlnorm()]
#' @param sdlog See the `sdlog` argument of [dlnorm()]
#'
#' @family sampling
#' @export
sample_log_normal <- function(N, prop_max = 0.1, meanlog, sdlog) {
  if (prop_max <= 0 | prop_max > 1) {
    stop("prop_max should be in (0, 1]")
  }
  if (prop_max >= 0.2) {
    warning("prop_max is 0.2 or more! This may be an inappropriate choice.")
  }

  max <- round(N * prop_max)

  samples <- c()
  remaining <- N

  while(remaining > 0) {
    draw <- round(rlnorm(n = 1, meanlog = meanlog, sdlog = sdlog))
    if (draw < max) {
      samples <- c(samples, draw)
      remaining <- remaining - draw
    }
  }

  if(sum(samples) >= N) {
    samples[length(samples)] <- samples[length(samples)] - (sum(samples) - N)
  }

  return(samples)
}

#' Sample variables from a negative binomial distribution with fixed sum
#'
#' This function samples from a negative binomial distribution. Rather than
#' specifying the total number of samples, as with `rnbinom`, this function
#' takes as input an integer `N` which the resulting samples must sum to. This
#' is achieved by adjusting the final sample. As such, the resulting samples
#' are not strictly speaking from a negative binomial distribution.
#'
#' @inheritParams sample_offset_truncated_power_distribution
#' @param mu See the `mu` argument of [rnbinom()]
#' @param size See the `size` argument of [rnbinom()]
#'
#' @family sampling
#' @export
sample_negbinom <- function(N, prop_max = 0.1, mu, size) {
  if (prop_max <= 0 | prop_max > 1) {
    stop("prop_max should be in (0, 1]")
  }
  if (prop_max >= 0.2) {
    warning("prop_max is 0.2 or more! This may be an inappropriate choice.")
  }

  max <- round(N * prop_max)

  samples <- c()
  remaining <- N

  while(remaining > 0) {
    draw <- round(rnbinom(n = 1, mu = mu, size = size))
    if (draw < max) {
      samples <- c(samples, draw)
      remaining <- remaining - draw
    }
  }

  if(sum(samples) >= N) {
    samples[length(samples)] <- samples[length(samples)] - (sum(samples) - N)
  }

  return(samples)
}
