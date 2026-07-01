# ===========================================================================
# efficacy curve ( Wells-Riley, no simulation)
# ===========================================================================
library(ggplot2)

baseline_ach <- c(household = 0.5, leisure = 3.0, school = 4.0, workplace = 4.8)
volume_per_person <- c(household = 50, leisure = 8, school = 10, workplace = 27)
delta_grid <- 0:30

eff_grid <- expand.grid(setting = names(baseline_ach), delta = delta_grid)
eff_grid$baseline_ach <- baseline_ach[eff_grid$setting]
eff_grid$V <- volume_per_person[eff_grid$setting]
eff_grid$efficacy <- mapply(
  function(ach, delta, V) ach_to_efficacy(baseline_ach = ach, delta = delta, V = V),
  eff_grid$baseline_ach, eff_grid$delta, eff_grid$V
)

p_EFF <- ggplot(eff_grid, aes(x = delta, y = efficacy, color = setting)) +
  geom_line(linewidth = 1) +
  labs(
    x = "Intervention delta (eACH)", y = "Efficacy (relative reduction in infection probability)",
    title = "Delta ACH vs Efficacy, Wells-Riley Test",
    color = "Setting (baseline ACH)"
  ) +
  theme_minimal()
print(p_EFF)
