args(vapply)

baseline_ACH_variability_function <- function(a, b) {
  return(a + b + rnorm(10, 1))
}

variation_in_ACH_function <- function(c, d, e, f) {
  return(f(c, d, e, f))
}

x <- list(intervetion_one = list(name = "uvc",
                                 affected_by_baseline_ACH = TRUE,
                                 baseline_ACH_function = baseline_ACH_variability_function,
                                 baseline_ACH_parameters = list("a" = 6, "b" = 7),
                                 variation_in_ACH_change = TRUE,
                                 variation_in_ACH_function = variation_in_ACH_function,
                                 variation_in_ACH_parameters = list("c" = 6, "d" = 7),
                                 coverage = 0.5))

x$intervetion_one$baseline_ACH_function()

names(x$intervetion_one)
