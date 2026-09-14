#' @keywords internal
"_PACKAGE"

#' @importFrom dplyr %>% mutate
#' @importFrom graphics abline arrows par
#' @importFrom stats approxfun lm median predict rbinom rgamma rlnorm rnbinom
#'   rpois runif sd
#' @importFrom utils modifyList tail
NULL

# Package datasets and data frame columns referenced without quotes
utils::globalVariables(c(
  "baseline_household_demographics_uk",
  "baseline_household_demographics_usa",
  "beta_total",
  "index",
  "schools_uk",
  "schools_usa",
  "type"
))
