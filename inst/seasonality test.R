# scratch_seasonality_check.R
# Quick manual check that the seasonality changes work and nothing broke.
# Run from the package root with:  Rscript scratch_seasonality_check.R
# (or open in RStudio and source it)

# Load the package in its current (uninstalled) state:
devtools::load_all(".")

# ---------------------------------------------------------------------------
# 1. Baseline run: seasonality OFF (exercises the unchanged code path)
# ---------------------------------------------------------------------------
cat("\n=== Baseline run (seasonality off) ===\n")
params_off <- get_parameters(overrides = list(simulation_time = 30))
out_off <- run_simulation(params_off)
res_off <- out_off$result

cat("Rows returned:", nrow(res_off), "\n")
cat("Columns:", paste(names(res_off), collapse = ", "), "\n")
cat("Final-row state counts:\n")
print(res_off[nrow(res_off), grep("_count", names(res_off))])

# ---------------------------------------------------------------------------
# 2. Seasonal run: seasonality ON with a winter-peaking cosine curve
#    multiplier centred on 1.0, ranging roughly 0.7 -> 1.3 over the year
# ---------------------------------------------------------------------------
cat("\n=== Seasonal run (seasonality on) ===\n")
# cos() peaks where its argument is 0, so using (day - 1) puts the maximum on
# day 1 (treated as mid-winter / 1 Jan): multiplier = 1.3 in deep winter, falling
# to a trough of 0.7 around day ~183 (mid-summer), then climbing back up.
seasonal_curve <- 1 + 0.3 * cos(2 * pi * ((1:365) - 1) / 365)
cat("multiplier length:", length(seasonal_curve),
    "| range:", round(min(seasonal_curve), 3), "to", round(max(seasonal_curve), 3), "\n")

params_on <- get_parameters(overrides = list(
  simulation_time        = 30,
  seasonality_on         = TRUE,
  seasonality_multiplier = seasonal_curve
))
out_on <- run_simulation(params_on)
res_on <- out_on$result

cat("Rows returned:", nrow(res_on), "\n")
cat("Final-row state counts:\n")
print(res_on[nrow(res_on), grep("_count", names(res_on))])

# ---------------------------------------------------------------------------
# 3. Validation guard: seasonality on with a bad-length multiplier should error
# ---------------------------------------------------------------------------
cat("\n=== Validation check (expect an error below) ===\n")
bad <- tryCatch(
  get_parameters(overrides = list(seasonality_on = TRUE,
                                  seasonality_multiplier = rep(1, 100))),
  error = function(e) conditionMessage(e)
)
cat("Got error as expected:\n  ", bad, "\n")

# ---------------------------------------------------------------------------
# 4. Visualise the results
# ---------------------------------------------------------------------------
library(ggplot2)

# --- 4a. Plot the seasonal multiplier curve itself ---
# This is the vector that scales beta on each day of the year. It is purely a
# user-supplied input (here a sine wave); the model just indexes into it by day.
mult_df <- data.frame(day = 1:365, multiplier = seasonal_curve)
p_mult <- ggplot(mult_df, aes(day, multiplier)) +
  geom_line(colour = "steelblue", linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  labs(
    title = "Seasonal multiplier applied to beta",
    subtitle = "beta_effective(day) = beta_baseline * multiplier(day)",
    x = "Day of year", y = "Multiplier"
  ) +
  theme_minimal()
ggsave("seasonality_multiplier.png", p_mult, width = 7, height = 4, dpi = 120)
cat("\nSaved plot: seasonality_multiplier.png\n")

# --- 4b. Compare 'no seasonality' vs 'seasonality on' ---
# Passing the SAME `seed` to both runs makes population generation and all random
# draws identical, so any difference in the epidemic curve is due to seasonality
# alone (a fair, like-for-like comparison). We also run for longer (a full year)
# so the seasonal curve has time to make a visible difference.
compare_time <- 365   # days
shared_seed  <- 1

cat("\n=== Comparison run (no seasonality vs seasonality, same seed) ===\n")

res_off2 <- run_simulation(get_parameters(overrides = list(
  simulation_time = compare_time,
  seed            = shared_seed
)))$result

res_on2 <- run_simulation(get_parameters(overrides = list(
  simulation_time        = compare_time,
  seed                   = shared_seed,
  seasonality_on         = TRUE,
  seasonality_multiplier = seasonal_curve
)))$result

# Convert the timestep counter to days for the x-axis.
# There are 1/dt timesteps per day (dt = 0.5 default => 2 steps/day), so day = timestep * dt.
dt <- 0.5
combined <- rbind(
  data.frame(
    day        = res_off2$timestep * dt,
    infectious = res_off2$I_mild_count,
    scenario   = "No seasonality"
  ),
  data.frame(
    day        = res_on2$timestep * dt,
    infectious = res_on2$I_mild_count,
    scenario   = "Seasonality on"
  )
)

p_cmp <- ggplot(combined, aes(day, infectious, colour = scenario)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Infectious individuals: seasonality vs none",
    subtitle = "Same seed, so the only difference is the seasonal beta multiplier",
    x = "Day", y = "Currently infectious (I_mild count)", colour = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")
ggsave("seasonality_comparison.png", p_cmp, width = 8, height = 4.5, dpi = 120)
cat("Saved plot: seasonality_comparison.png\n")

# NOTE: the default model runs in 'epidemic' mode, where the outbreak burns through
# the population once and dies out. Seasonality is most visually striking in
# 'endemic' mode (sustained transmission with recurring seasonal waves). To explore
# that, add these overrides to BOTH runs above:
#   endemic_or_epidemic = "endemic", duration_immune = 60, prob_inf_external = 0.001

cat("\nDone.\n")
