sample_bitset <- function(b, rate) {
  individual::filter_bitset(b, bernoulli(b$size(), rate))
}

bitset_at <- function(b, i) {
  individual::filter_bitset(b, i)
}

#' Find the indices of `a` where it intersects with `b`
#'
#' Synonymous with `which(a$to_vector() %in% b$to_vector())` but faster
#'
#' @param a the bitset to index
#' @param b the bitset to check
#' @noRd
bitset_index <- function(a, b) bitset_index_cpp(a$.bitset, b$.bitset)
