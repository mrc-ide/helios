sample_bitset <- function(b, rate) {
  individual::filter_bitset(b, bernoulli(b$size(), rate))
}

bitset_at <- function(b, i) {
  individual::filter_bitset(b, i)
}

#' @title find the indices of a where it intersects with b
#' @description synonymous with \code{which(a$to_vector() %in%
#' b$to_vector())} but faster
#' @param a the bitset to index
#' @param b the bitset to check
#' @noRd
bitset_index <- function(a, b) bitset_index_cpp(a$.bitset, b$.bitset)

#' get_setting_sizes
#'
#' @description
#' get_setting_sizes() retrieves the number of individuals in each individual setting within
#' a setting type (e.g. number of individuals in each workplace). The function calculates these
#' from the list of model variables for the workplace, school, and household settings, but takes them
#' directly as an input for leisure.The function returns a list with a vector of individual setting
#' sizes for each setting type.
#'
#' @param variables_list A list of model variables as generated using `create_variables()`
#' @param leisure_sizes The sizes of the leisure settings as sampled using `sample_negbinom()`
#'
#' @export
get_setting_sizes <- function(variables_list, leisure_sizes) {

  # Open list to store the vectors of setting population sizes:
  setting_populations <- list()

  # Retrieve the workplace populations
  workplace_sizes <- vector()
  workplaces <- 1:max(as.numeric(variables_list$workplace$get_categories()))
  for(i in workplaces) {
    workplace_sizes[i] <- variables_list$workplace$get_size_of(as.character(i))
  }

  # Retrieve the school populations
  school_sizes <- vector()
  schools <- 1:max(as.numeric(variables_list$school$get_categories()))
  for(i in schools) {
    school_sizes[i] <- variables_list$school$get_size_of(as.character(i))
  }

  # Retrieve the household populations
  household_sizes <- vector()
  households <- 1:max(as.numeric(variables_list$household$get_categories()))
  for(i in households) {
    household_sizes[i] <- variables_list$household$get_size_of(as.character(i))
  }

  # Populate the list of workplace populations:
  setting_populations$workplace <- workplace_sizes
  setting_populations$school <- school_sizes
  setting_populations$leisure <- leisure_sizes
  setting_populations$household <- household_sizes

  # Return the list of setting populations:
  setting_populations

}
