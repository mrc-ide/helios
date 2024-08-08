#include <Rcpp.h>
#include <individual.h>

//' @export
//[[Rcpp::export]]
size_t bitset_and_size(
    const Rcpp::XPtr<individual_index_t> a,
    const Rcpp::XPtr<individual_index_t> b
) {
  if (a->max_size() != b->max_size()) {
    Rcpp::stop("Incompatible bitset sizes");
  }
  auto n = 0u;
  // for (auto i = 0u; i < a->bitmap.size(); ++i) {
  //   // popcount implemented in individual::IterableBitset.h
  //   n += popcount(a->bitmap[i] & b->bitmap[i]);
  // }
  return n;
};

// #include <Rcpp.h>
// #include <individual.h>
//
// //[[Rcpp::export]]
// size_t bitset_and_size(
//     const Rcpp::XPtr<individual_index_t> a,
//     const Rcpp::XPtr<individual_index_t> b
// ) {
//   if (a->max_size() != b->max_size()) {
//     Rcpp::stop("Incompatible bitset sizes");
//   }
//   auto n = 0u;
//   for (auto i = 0u; i < a->bitmap.size(); ++i) {
//     // popcount implemented in individual::IterableBitset.h
//     n += popcount(a->bitmap[i] & b->bitmap[i]);
//   }
//   return n;
// };
