

library(dplyr)
library(ggplot2)
library(tidyr)

devtools::load_all("/Users/geethaj/helios")

# ---- Scan grid --------------------------------------------------------------
pop_size       <- 10000
sim_time       <- 365
coverage_grid  <- c(0, 0.2, 0.4, 0.6, 0.8)
efficacy_grid  <- c(0.4, 0.6, 0.8)
cov_type_grid  <- c("random", "targeted_riskiness")
seed_grid      <- 1:2

# ---- Calibrate delta per efficacy target ------------------------------------
# Constant-delta intervention; one delta per target efficacy, calibrated at
# the midpoint of joint settings (baseline_ach=4, V=15). Realized per-location
# efficacy will land in a ~10-percentage-point band around the target.
calibrated_deltas <- sapply(efficacy_grid, function(e) {
  efficacy_to_delta(target_efficacy = e, baseline_ach = 4.0, V = 15)
})
names(calibrated_deltas) <- as.character(efficacy_grid)
cat("Calibrated deltas:\n"); print(calibrated_deltas)

# ---- Helper: single simulation run ------------------------------------------
run_one <- function(seed, coverage, efficacy_target, coverage_type) {
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
    archetype = "sars_cov_2"
  ) %>%
    set_setting_specific_ach("workplace", mean = 4.8, sd = 1.5) %>%
    set_setting_specific_ach("school",    mean = 4.0, sd = 1.2) %>%
    set_setting_specific_ach("leisure",   mean = 3.0, sd = 1.0) %>%
    set_setting_specific_ach("household", mean = 0.5, sd = 0.2)

  if (coverage > 0) {
    d <- calibrated_deltas[[as.character(efficacy_target)]]
    intv <- make_intervention(
      name                     = paste0("uvc_eff", efficacy_target),
      delta_depends_on_baseline_ach = FALSE,
      delta_function    = local({
        delta_val <- d
        function() delta_val
      }),
      delta_params      = list(),
      coverage = coverage
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
    peak_count  = max(output$I_count),
    peak_pct    = max(output$I_count) / pop_size * 100,
    final_count = tail(output$R_count, 1),
    final_pct   = tail(output$R_count, 1) / pop_size * 100
  )
}

# ---- Baseline runs (coverage = 0, no intervention) --------------------------
baseline_results <- list()
for (s in seed_grid) {
  cat(sprintf("[baseline] seed=%d\n", s))
  baseline_results[[length(baseline_results) + 1]] <- cbind(
    seed = s,
    run_one(seed = s, coverage = 0, efficacy_target = NA, coverage_type = NA)
  )
}
baseline_df <- bind_rows(baseline_results)

# Expand baseline across all (eff, cov_type) panels so the plot's coverage=0
# point appears in every panel.
baseline_expanded <- crossing(
  baseline_df,
  efficacy_target = efficacy_grid,
  coverage_type   = cov_type_grid
) %>% mutate(coverage = 0)

# ---- Intervention runs ------------------------------------------------------
intervention_grid <- expand_grid(
  coverage        = coverage_grid[coverage_grid > 0],
  efficacy_target = efficacy_grid,
  coverage_type   = cov_type_grid,
  seed            = seed_grid
)
total <- nrow(intervention_grid)
intervention_results <- list()

start_time <- Sys.time()
for (i in seq_len(total)) {
  row <- intervention_grid[i, ]
  cat(sprintf("[%d/%d] cov=%.1f eff=%.1f type=%s seed=%d  (elapsed: %s)\n",
              i, total, row$coverage, row$efficacy_target,
              row$coverage_type, row$seed,
              format(Sys.time() - start_time)))
  intervention_results[[i]] <- cbind(
    row,
    run_one(
      seed            = row$seed,
      coverage        = row$coverage,
      efficacy_target = row$efficacy_target,
      coverage_type   = row$coverage_type
    )
  )
}
intervention_df <- bind_rows(intervention_results)

# ---- Combine + save ---------------------------------------------------------
results_df <- bind_rows(baseline_expanded, intervention_df) %>%
  select(coverage, efficacy_target, coverage_type, seed,
         peak_count, peak_pct, final_count, final_pct)

out_dir <- "/Users/geethaj/helios_comparison/output/branch"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(results_df, file.path(out_dir, "scan_sc2.csv"), row.names = FALSE)
cat("\nResults saved to", file.path(out_dir, "scan_sc2.csv"), "\n")

# ---- Summarize across seeds -------------------------------------------------
summary_df <- results_df %>%
  group_by(efficacy_target, coverage, coverage_type) %>%
  summarise(
    peak_mean   = mean(peak_pct),
    peak_lower  = min(peak_pct),
    peak_upper  = max(peak_pct),
    final_mean  = mean(final_pct),
    final_lower = min(final_pct),
    final_upper = max(final_pct),
    .groups = "drop"
  ) %>%
  mutate(
    coverage_label = factor(
      coverage_type,
      levels = c("random", "targeted_riskiness"),
      labels = c("Random", "Targeted")
    ),
    efficacy_label = factor(
      paste0(efficacy_target * 100, "% Efficacy"),
      levels = paste0(efficacy_grid * 100, "% Efficacy")
    )
  )

# ---- Plot: peak epidemic size -----------------------------------------------
p_peak <- ggplot(summary_df, aes(x = coverage * 100, color = coverage_label,
                                 fill = coverage_label)) +
  geom_ribbon(aes(ymin = peak_lower, ymax = peak_upper), alpha = 0.25, color = NA) +
  geom_line(aes(y = peak_mean), linewidth = 1) +
  facet_grid(. ~ efficacy_label) +
  scale_color_manual(values = c(Random = "#5DADE2", Targeted = "#1B4F72")) +
  scale_fill_manual(values  = c(Random = "#5DADE2", Targeted = "#1B4F72")) +
  scale_y_continuous(limits = c(0, NA), breaks = seq(0,25, by = 5)) +
  labs(
    x = "Joint Intervention Coverage (%)",
    y = "Mean Peak Epidemic Size\n(% of population)",
    color = "Coverage\nType", fill = "Coverage\nType",
    title = "SARS-CoV-2 | branch scan | 50k pop, 5 seeds"
  ) +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "grey90", color = NA))

print(p_peak)
ggsave(file.path(out_dir, "scan_sc2_peak.png"), plot = p_peak,
       width = 10, height = 4, dpi = 150)

# ---- Plot: final epidemic size ----------------------------------------------
p_final <- ggplot(summary_df, aes(x = coverage * 100, color = coverage_label,
                                  fill = coverage_label)) +
  geom_ribbon(aes(ymin = final_lower, ymax = final_upper), alpha = 0.25, color = NA) +
  geom_line(aes(y = final_mean), linewidth = 1) +
  facet_grid(. ~ efficacy_label) +
  scale_color_manual(values = c(Random = "#5DADE2", Targeted = "#1B4F72")) +
  scale_fill_manual(values  = c(Random = "#5DADE2", Targeted = "#1B4F72")) +
  scale_y_continuous(limits = c(0, NA), breaks = seq(0, 100, by = 25)) +
  labs(
    x = "Joint Intervention Coverage (%)",
    y = "Mean Final Epidemic Size\n(% of population)",
    color = "Coverage\nType", fill = "Coverage\nType",
    title = "SARS-CoV-2 |ACH branch |  50k pop, 5 seeds"
  ) +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "grey90", color = NA))

print(p_final)
ggsave(file.path(out_dir, "scan_sc2_final.png"), plot = p_final,
       width = 10, height = 4, dpi = 150)

cat("\nDone. Total elapsed:", format(Sys.time() - start_time), "\n")
