# (1) base params
params <- get_parameters(overrides = list(human_population = N, ...), archetype = "sars_cov_2")

# (2) optional: turn on setting-specific ACH for any subset of settings.
#     If omitted for a setting, that setting gets uniform riskiness = 1.
params <- params %>%
  set_setting_specific_ach("workplace", mean = 4.8, sd = 1.5) %>%
  set_setting_specific_ach("school",    mean = 4.0, sd = 1.2) %>%
  set_setting_specific_ach("leisure",   mean = 3.0, sd = 1.0) %>%
  set_setting_specific_ach("household", mean = 0.5, sd = 0.2)

# (3) optional: define an intervention. Three knobs:
#     - constant delta:                 baseline_ach_function = function() <delta>, affected_by_baseline_ach = FALSE
#     - delta varies with baseline:     baseline_ach_function = function(ach) ..., affected_by_baseline_ach = TRUE
#     - add unit-to-unit noise:         variation = TRUE, variation_function = rnorm, variation_params = list(mean=0, sd=...)
intv <- make_intervention(name = ..., baseline_ach_function = ..., ...)

# (4) optional: install the intervention. Setting can be "workplace", "school",
#     "leisure", "household", or "joint" (which deploys to wp/sch/leisure as a pool).
params <- set_intervention_ach(
  params,
  setting         = "joint",       # or one specific setting
  coverage        = 0.5,           # fraction of total size to cover
  coverage_target = "individuals", # or "square_footage"
  coverage_type   = "random",      # or "targeted_riskiness"
  timestep        = 0,
  intv
)

# run
output <- run_simulation(params)
