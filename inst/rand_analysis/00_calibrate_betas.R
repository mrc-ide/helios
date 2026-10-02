# Per-city beta calibration for the RAND flu (non-pandemic) scenarios.
#
# Target: a 30% attack rate (all infections, symptomatic and asymptomatic)
# over 365 days, the same for every city. This matches the baseline used in
# the earlier NYC flu analysis, and is consistent with CDC's 2023-24 season
# estimate of ~40 million symptomatic illnesses (~12% of ~335 million) if
# roughly 40% of infections are symptomatic. Helios does not distinguish
# symptomatic from asymptomatic infection, so the target is total infections.
#
# Each city is calibrated separately because its household and school
# structure changes how much transmission a given beta produces, so a
# shared target still yields city-specific betas.
#
# Beta is matched directly against the simulated attack rate at day 365
# (the sweep's AR_mean column), not via the closed-form final-size R0.
# Near the epidemic threshold, a finite-length run may not reach the
# asymptotic final size, so matching the simulated attack rate keeps the
# calibration consistent with the runs it feeds.
#
# Limitations, to note in the write-up:
#   - Calibration runs in epidemic mode (fully susceptible population),
#     whereas real seasonal flu circulates through a partially immune
#     population. The implied R0 (~1.19) is close to the published range
#     for seasonal flu (~1.2-1.4).
#   - The SARS-CoV-2 (pandemic) scenario is deferred until RAND clarifies
#     the time window and hospitalization definition behind their
#     '20-'21 rates.
#
# run_beta_sweep() spins up its own parallel cluster and calls
# library(helios) inside it, rather than inheriting this session's
# devtools::load_all(). The bootstrap section below therefore reinstalls
# helios from source so the installed package includes this branch's
# changes (e.g. household_reference_panel).

# ---------------------------------------------------------------------------
# Bootstrap (for a machine that has never run helios before)
# ---------------------------------------------------------------------------
options(repos = c(CRAN = "https://cloud.r-project.org"))
options(rlang_interactive = FALSE)

cran_pkgs <- c(
  "devtools", "remotes", "withr", "dplyr", "tidyr", "tibble", "readr",
  "EnvStats", "dqrng", "truncnorm"
)
missing_cran <- setdiff(cran_pkgs, rownames(installed.packages()))
if (length(missing_cran) > 0) install.packages(missing_cran)

if (!requireNamespace("individual", quietly = TRUE)) {
  withr::with_makevars(
    c(CXX_STD = "CXX17"),
    remotes::install_github("mrc-ide/individual@feat/logi_size")
  )
}

pkg_path <- normalizePath(".")
devtools::install(pkg_path, quiet = TRUE, upgrade = "never")
devtools::load_all(pkg_path)

# ---------------------------------------------------------------------------
# Settings
# ---------------------------------------------------------------------------
# Overridable via env vars for a fast first pass, e.g.:
#   RAND_CALIB_POP=2000 RAND_CALIB_N_BETA=4 RAND_CALIB_N_REPS=2 RAND_CALIB_SIM_TIME_CAP=20 \
#     Rscript inst/rand_analysis/00_calibrate_betas.R

TARGET_ATTACK_RATE <- 0.30
SWEEP_POPULATION   <- as.integer(Sys.getenv("RAND_CALIB_POP", "100000"))

# Initial exposed as a fraction of the population, shared with
# 02_run_scenarios.R so the calibrated betas carry over to the runs. With R0
# near 1.2, a handful of seed cases would leave many runs to die out by
# chance; 0.2% (200 at 100,000) avoids that.
SEED_FRACTION      <- 0.002
SWEEP_SEED_E       <- as.integer(round(SWEEP_POPULATION * SEED_FRACTION))
SWEEP_N_BETA       <- as.integer(Sys.getenv("RAND_CALIB_N_BETA", "25"))
SWEEP_N_REPS       <- as.integer(Sys.getenv("RAND_CALIB_N_REPS", "20"))
SIM_TIME           <- as.integer(Sys.getenv("RAND_CALIB_SIM_TIME_CAP", "365"))

# The target sits just above the epidemic threshold, so the sweep is
# concentrated at low beta for resolution there.
BETA_RANGE <- c(
  as.numeric(Sys.getenv("RAND_CALIB_BETA_MIN", "0.01")),
  as.numeric(Sys.getenv("RAND_CALIB_BETA_MAX", "0.20"))
)

# Derived from get_parameters()'s own default beta ratios
# (household = workplace = school = leisure = 0.5, community = 0.2), rather
# than an invented split -- no other transmission_fraction convention exists
# elsewhere in this repo.
transmission_fraction <- c(
  household = 0.5, workplace = 0.5, leisure = 0.5, community = 0.2
)
transmission_fraction <- transmission_fraction / sum(transmission_fraction)

# ---------------------------------------------------------------------------
# City data
# ---------------------------------------------------------------------------

cities <- list(
  san_francisco = readRDS("inst/rand_analysis/city_data/san_francisco.rds"),
  pittsburgh    = readRDS("inst/rand_analysis/city_data/pittsburgh.rds"),
  nyc           = readRDS("inst/rand_analysis/city_data/nyc.rds")
)

# ---------------------------------------------------------------------------
# Sweep beta per city, then interpolate the target attack rate -> beta
# ---------------------------------------------------------------------------

calibrated_rows <- vector("list", length(cities))
sweep_results <- vector("list", length(cities))
names(sweep_results) <- names(cities)

for (i in seq_along(cities)) {
  city <- names(cities)[i]
  city_data <- cities[[city]]

  model_params <- get_parameters(
    archetype = "flu",
    overrides = list(
      human_population = SWEEP_POPULATION,
      number_initial_S = SWEEP_POPULATION - SWEEP_SEED_E,
      number_initial_E = SWEEP_SEED_E,
      simulation_time = SIM_TIME,
      seed = 1,
      household_distribution_country = "custom",
      household_reference_panel = city_data$household_reference_panel,
      school_distribution_country = "custom",
      school_reference_sizes = city_data$school_reference_sizes,
      workplace_distribution_country = "USA"
    )
  )

  for (setting in c("workplace", "school", "leisure", "household")) {
    model_params <- set_default_ach(
      model_params,
      setting,
      switch(setting, workplace = 3.1, school = 3.3, leisure = 3.5, household = 2.0)
    )
  }

  message(sprintf("Calibrating %s (target attack rate = %.2f)...", city, TARGET_ATTACK_RATE))

  sweep_result <- run_beta_sweep(
    transmission_fraction = transmission_fraction,
    model_params = model_params,
    beta_community_range = BETA_RANGE,
    n_beta = SWEEP_N_BETA,
    population = SWEEP_POPULATION,
    n_reps = SWEEP_N_REPS,
    sim_time = SIM_TIME
  )
  sweep_results[[city]] <- sweep_result

  sweep_table <- sweep_result$sweep_table[order(sweep_result$sweep_table$beta_community), ]

  # Interpolation is only valid if the swept attack rates bracket the target.
  if (min(sweep_table$AR_mean) > TARGET_ATTACK_RATE ||
      max(sweep_table$AR_mean) < TARGET_ATTACK_RATE) {
    stop(sprintf(
      "Swept attack rates for %s (%.3f to %.3f) do not bracket the target %.2f -- widen RAND_CALIB_BETA_MIN/MAX.",
      city, min(sweep_table$AR_mean), max(sweep_table$AR_mean), TARGET_ATTACK_RATE
    ))
  }

  beta_community <- stats::approx(
    x = sweep_table$AR_mean,
    y = sweep_table$beta_community,
    xout = TARGET_ATTACK_RATE,
    ties = mean
  )$y

  calibrated_rows[[i]] <- tibble::tibble(
    city = city,
    pathogen = "non_pandemic",
    target_attack_rate = TARGET_ATTACK_RATE,
    implied_R0 = -log(1 - TARGET_ATTACK_RATE) / TARGET_ATTACK_RATE,
    beta_community = beta_community,
    beta_household = beta_community * transmission_fraction["household"] / transmission_fraction["community"],
    beta_workplace = beta_community * transmission_fraction["workplace"] / transmission_fraction["community"],
    beta_school    = beta_community * transmission_fraction["workplace"] / transmission_fraction["community"],
    beta_leisure   = beta_community * transmission_fraction["leisure"]   / transmission_fraction["community"]
  )
}

calibrated_betas <- dplyr::bind_rows(calibrated_rows)

saveRDS(calibrated_betas, "inst/rand_analysis/city_data/calibrated_betas.rds")
readr::write_csv(calibrated_betas, "inst/rand_analysis/city_data/calibrated_betas.csv")
saveRDS(sweep_results, "inst/rand_analysis/city_data/calibration_sweeps.rds")

print(calibrated_betas)
