# generate_time_varying_beta_vectors.R
#
# PURPOSE
# -------
# Given a fixed transmission_fraction split (household/workplace/leisure/
# community ratios, summing to 1) and a time-varying beta_community vector
# (e.g. the output of get_beta_from_R0_finalsize() applied to an Rt(t)
# series), expands it into the five setting-specific beta vectors Helios
# needs when time_varying_transmission_on = TRUE: beta_household, beta_workplace,
# beta_school, beta_leisure, beta_community, each the same length as the
# input beta_community vector.
#
# This is a plain construction-time helper -- it runs no Helios
# simulations. Helios's core parameters have no concept of
# transmission_fraction themselves; the ratio expansion always happens
# here, before get_parameters() is called.

#' Expand a transmission_fraction split and a beta_community vector into
#' the five setting-specific beta vectors Helios needs
#'
#' @param transmission_fraction Named numeric vector with names `household`,
#'   `workplace`, `leisure`, `community`, summing to 1.
#' @param beta_community_vector Numeric vector, the time-varying community beta
#'   (one value per simulated day).
#'
#' @return A named list with five numeric vectors, each the same length as
#'   `beta_community_vector`: `beta_household`, `beta_workplace`, `beta_school`,
#'   `beta_leisure`, `beta_community`. `beta_school` is always identical to
#'   `beta_workplace`. Ready to pass directly into
#'   `get_parameters(overrides = ...)` alongside `time_varying_transmission_on = TRUE`.
#'
#' @export
generate_time_varying_beta_vectors <- function(transmission_fraction, beta_community_vector) {

  #=== Validate inputs ===#
  required_settings <- c("household", "workplace", "leisure", "community")
  if (!setequal(names(transmission_fraction), required_settings)) {
    stop(sprintf(
      "transmission_fraction must have exactly these names: %s",
      paste(required_settings, collapse = ", ")
    ))
  }
  if (!isTRUE(all.equal(sum(transmission_fraction), 1))) {
    stop("transmission_fraction must sum to 1")
  }
  if (!is.numeric(beta_community_vector)) {
    stop("beta_community_vector must be numeric")
  }

  #=== Derive setting-specific ratios relative to community ===#
  household_ratio <- transmission_fraction["household"] / transmission_fraction["community"]
  workplace_ratio <- transmission_fraction["workplace"] / transmission_fraction["community"]
  leisure_ratio   <- transmission_fraction["leisure"]   / transmission_fraction["community"]
  school_ratio    <- workplace_ratio

  #=== Expand into five vectors, one per setting ===#
  result <- list(
    beta_household = unname(household_ratio * beta_community_vector),
    beta_workplace = unname(workplace_ratio * beta_community_vector),
    beta_school    = unname(school_ratio    * beta_community_vector),
    beta_leisure   = unname(leisure_ratio   * beta_community_vector),
    beta_community = beta_community_vector
  )

  result
}
