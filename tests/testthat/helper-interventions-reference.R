# Reference (oracle) implementation used by test-interventions.R.
#
# A copy of the "random" coverage loop of generate_setting_intervention_switches()
# and generate_joint_intervention_switches() as it was before it was replaced by
# select_random_locations(): draw one location at a time with sample() and drop it
# from the candidates with setdiff(). It is quadratic in the number of locations,
# which is why it was replaced, and is kept so the replacement can be checked
# against it.

reference_select_random_locations <- function(location_size, target, error_message) {
  sum <- 0
  indices <- c()
  location_indices <- 1:length(location_size)

  while (sum < target) {
    i <- sample(location_indices, 1)
    sum <- sum + location_size[i]
    indices <- c(indices, i)
    location_indices <- setdiff(location_indices, i)
    if (length(location_indices) == 0 & sum < target) {
      stop(error_message)
    }
  }
  indices
}
