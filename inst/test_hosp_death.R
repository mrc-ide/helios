## Quick smoke test for hospitalization & death implementation

devtools::load_all()

params <- get_parameters(overrides = list(
  human_population = 2000,
  simulation_time = 100,
  number_initial_S = 1980,
  number_initial_E = 20,
  number_initial_I = 0,
  number_initial_R = 0
))

res <- run_simulation(params)
out <- res$result

cat("Output columns:\n")
print(names(out))

cat("\nFinal row:\n")
print(tail(out, 1))

cat("\nState counts at final timestep:\n")
final <- tail(out, 1)
state_cols <- c("S_count", "E_count", "I_mild_count", "I_hosp_count",
                "R_count", "D_count")
print(final[, state_cols])

cat("\nSum of compartments at final step (should equal human_population =",
    params$human_population, "):\n")
print(sum(unlist(final[, state_cols])))

cat("\nCumulative H_new (new hospitalizations) and D_new (new deaths):\n")
if ("H_new" %in% names(out)) cat("  H_new total:", sum(out$H_new, na.rm = TRUE), "\n")
if ("D_new" %in% names(out)) cat("  D_new total:", sum(out$D_new, na.rm = TRUE), "\n")

cat("\nQuick plot of trajectories:\n")
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  library(tidyr)
  long <- tidyr::pivot_longer(out, tidyr::all_of(state_cols),
                              names_to = "state", values_to = "n")
  p <- ggplot(long, aes(timestep, n, color = state)) +
    geom_line() +
    theme_minimal()
  print(p)
}
