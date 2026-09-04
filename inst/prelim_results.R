# How intervention strength (delta-eACH) impacts outbreak size
# SARS-CoV-2, joint intervention, 10k pop
# Run from /Users/geethaj/helios (ach_efficacy_update branch)

library(dplyr)
library(ggplot2)

devtools::load_all("/Users/geethaj/helios")

# ---- Scan grid --------------------------------------------------------------
pop_size      <- 10000
sim_time      <- 150
delta_grid    <- c(0, 1, 2, 3, 5, 8, 12)        # eACH added by the intervention
coverage      <- 0.5                             # fraction covered (fixed)
cov_type_grid <- c("random", "targeted_riskiness")
seed_grid     <- 1:3

# ---- Single run -------------------------------------------------------------
run_one <- function(seed, delta, coverage_type) {
  params <- get_parameters(
    overrides = list(
      human_population = pop_size,
      number_initial_S = pop_size - 10,
      number_initial_E = 10,
      number_initial_I = 0,
      number_initial_R = 0,
      simulation_time  = sim_time,
      seed             = seed
    ),
    archetype = "flu"
  ) %>%
    set_setting_specific_ach("workplace", mean = 4.8, sd = 1.5) %>%
    set_setting_specific_ach("school",    mean = 4.0, sd = 1.2) %>%
    set_setting_specific_ach("leisure",   mean = 3.0, sd = 1.0) %>%
    set_setting_specific_ach("household", mean = 0.5, sd = 0.2)

  # delta = 0 means no intervention effect -> serves as the baseline point.
  if (delta > 0) {
    intv <- make_intervention(
      name                          = paste0("delta_", delta),
      delta_depends_on_baseline_ach = FALSE,
      delta_function                = local({ d <- delta; function() d }),
      delta_params                  = list(),
      coverage                      = coverage
    )
    params <- set_intervention_ach(
      params,
      setting         = "joint",
      coverage_target = "individuals",
      coverage_type   = coverage_type,
      timestep        = 0,
      intv
    )
  }

  output <- run_simulation(params)
  data.frame(
    final_pct = tail(output$R_count, 1) / pop_size * 100,
    peak_pct  = max(output$I_count) / pop_size * 100
  )
}

# ---- Run the scan -----------------------------------------------------------
grid <- expand.grid(
  delta         = delta_grid,
  coverage_type = cov_type_grid,
  seed          = seed_grid,
  stringsAsFactors = FALSE
)

# At delta = 0 the coverage_type is irrelevant, but we run it under both labels
# so the baseline point appears on both lines.
results <- list()
for (i in seq_len(nrow(grid))) {
  row <- grid[i, ]
  cat(sprintf("[%d/%d] delta=%.0f type=%s seed=%d\n",
              i, nrow(grid), row$delta, row$coverage_type, row$seed))
  results[[i]] <- cbind(row, run_one(row$seed, row$delta, row$coverage_type))
}
results_df <- bind_rows(results)

# ---- Summarise across seeds -------------------------------------------------
summary_df <- results_df %>%
  group_by(delta, coverage_type) %>%
  summarise(
    final_mean  = mean(final_pct),
    final_lower = min(final_pct),
    final_upper = max(final_pct),
    peak_mean   = mean(peak_pct),
    .groups = "drop"
  ) %>%
  mutate(coverage_label = factor(
    coverage_type,
    levels = c("random", "targeted_riskiness"),
    labels = c("Random", "Targeted")
  ))

# ---- Save -------------------------------------------------------------------
out_dir <- "/Users/geethaj/helios_comparison/output/branch"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(results_df, file.path(out_dir, "delta_scan_sc2.csv"), row.names = FALSE)

# ---- Plot: final epidemic size vs intervention delta ------------------------
p <- ggplot(summary_df, aes(x = delta, y = final_mean,
                            color = coverage_label, fill = coverage_label)) +
  geom_ribbon(aes(ymin = final_lower, ymax = final_upper),
              alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c(Random = "#5DADE2", Targeted = "#1B4F72")) +
  scale_fill_manual(values  = c(Random = "#5DADE2", Targeted = "#1B4F72")) +
  labs(
    title    = "Outbreak size vs intervention strength",
    subtitle = paste0("SARS-CoV-2 | joint intervention | ",
                      coverage * 100, "% coverage | 10k pop, 5 seeds"),
    x = "Intervention strength (eACH added)",
    y = "Mean final epidemic size (% of population)",
    color = "Coverage\ntype", fill = "Coverage\ntype"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p)
ggsave(file.path(out_dir, "delta_scan_sc2.png"), plot = p,
       width = 8, height = 5, dpi = 150)

cat("\nSaved to", out_dir, "\n")
