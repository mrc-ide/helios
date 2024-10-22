% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampling.R
\name{sample_negbinom}
\alias{sample_negbinom}
\title{Sample variables from a negative binomial distribution with fixed sum}
\usage{
sample_negbinom(N, prop_max = 0.1, mu, size)
}
\arguments{
\item{N}{An integer giving the sum of generated random variables}

\item{prop_max}{The proportion of \code{N} which is the maximum value of a single sample}

\item{mu}{See the \code{mu} argument of \code{\link[=rnbinom]{rnbinom()}}}

\item{size}{See the \code{size} argument of \code{\link[=rnbinom]{rnbinom()}}}
}
\description{
This function samples from a negative binomial distribution. Rather than
specifying the total number of samples, as with \code{rnbinom}, this function
takes as input an integer \code{N} which the resulting samples must sum to. This
is achieved by adjusting the final sample. As such, the resulting samples
are not strictly speaking from a negative binomial distribution.
}
\seealso{
Other sampling: 
\code{\link{sample_log_normal}()},
\code{\link{sample_offset_truncated_power_distribution}()}
}
\concept{sampling}
