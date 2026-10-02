# Run matrix driver for the RAND UV-C vs. glycol scenario comparison.
#
# Covers 4 location types with real per-city data (workplace, school,
# leisure, household). Community is deferred pending confirmation of
# default_ach_community and volume_per_person_community.
#
# Workplace uses the nationwide "USA" default for every city (see
# inst/rand_analysis/build_city_data.R for why RTI's city-specific workplace
# data was rejected). Household and school data are city-specific, loaded
# from inst/rand_analysis/city_data/.
#
# Baseline ventilation uses the package defaults from get_parameters()
# (uniform ACH: workplace 3.1, school 3.3, leisure 3.5, household 2.0) -- not
# the alternative "realistic" heterogeneous set used elsewhere in this repo's
# sanity checks. This is a modeling choice flagged for confirmation, not a
# settled decision.
#
# Overridable via env vars for a fast first pass before committing to the
# full replicate count, e.g.:
#   RAND_N_REPS=1 RAND_POP=5000 Rscript inst/rand_analysis/02_run_scenarios.R
#
# Run this from the package root (helios/) on a machine that may never have
# had helios installed before -- the bootstrap section below installs
# whatever is missing. Safe to re-run.

# ---------------------------------------------------------------------------
# Bootstrap (for a machine that has never run helios before)
# ---------------------------------------------------------------------------
# Without an explicit repos option, install.packages() pops up an
# interactive CRAN-mirror chooser on the first call -- in a non-interactive
# Rscript session that has no terminal to read from and ends up misreading
# the rest of this file as menu selections. Setting a mirror and disabling
# rlang's interactive prompts avoids that.
options(repos = c(CRAN = "https://cloud.r-project.org"))
options(rlang_interactive = FALSE)

cran_pkgs <- c(
  "devtools", "remotes", "withr", "dplyr", "tidyr", "tibble", "readr",
  "EnvStats", "dqrng", "truncnorm"
)
missing_cran <- setdiff(cran_pkgs, rownames(installed.packages()))
if (length(missing_cran) > 0) install.packages(missing_cran)

# "individual" must come from the branch helios currently depends on (see
# Remotes: in DESCRIPTION), not CRAN.
#
# On Windows with a recent Rtools/GCC, the source build can fail with errors
# like "'reference' in ... allocator_type does not name a type": GCC
# defaults to -std=gnu++20, and individual's C++ headers use pre-C++20
# std::allocator members that C++20 removed. Forcing CXX_STD = CXX17 for
# this build works around it without touching individual's source.
if (!requireNamespace("individual", quietly = TRUE)) {
  withr::with_makevars(
    c(CXX_STD = "CXX17"),
    remotes::install_github("mrc-ide/individual@feat/logi_size")
  )
}

# Captured explicitly (rather than relying on load_all()'s default of "the
# current working directory") because PSOCK worker processes spawned below
# are not guaranteed to start in the same working directory as this master
# session.
pkg_path <- normalizePath(".")
devtools::load_all(pkg_path)
source("inst/rand_analysis/build_city_data.R")
source("inst/rand_analysis/01_interventions.R")

N_REPS     <- as.integer(Sys.getenv("RAND_N_REPS", "3"))
POPULATION <- as.integer(Sys.getenv("RAND_POP", "250000"))

# Initial exposed as a fraction of the population -- must match
# SEED_FRACTION in 00_calibrate_betas.R so the calibrated betas reproduce
# the target attack rate here (0.2%, i.e. 500 at 250,000).
SEED_FRACTION <- 0.002
SEED_E <- as.integer(round(POPULATION * SEED_FRACTION))
N_WORKERS  <- max(1, min(10, parallel::detectCores() - 1))

# ---------------------------------------------------------------------------
# City data
# ---------------------------------------------------------------------------

cities <- list(
  san_francisco = readRDS("inst/rand_analysis/city_data/san_francisco.rds"),
  pittsburgh    = readRDS("inst/rand_analysis/city_data/pittsburgh.rds"),
  nyc           = readRDS("inst/rand_analysis/city_data/nyc.rds")
)

# Per-city, per-pathogen betas calibrated against each city's real
# hospitalization rate (see inst/rand_analysis/00_calibrate_betas.R) --
# replaces the archetype's shared nationwide beta defaults, so baseline
# burden reflects each city's own transmission intensity, not just its
# demographics.
calibrated_betas_path <- "inst/rand_analysis/city_data/calibrated_betas.rds"
if (!file.exists(calibrated_betas_path)) {
  stop(
    "Calibrated betas not found at ", calibrated_betas_path, " -- run ",
    "inst/rand_analysis/00_calibrate_betas.R first."
  )
}
calibrated_betas <- readRDS(calibrated_betas_path)

# ---------------------------------------------------------------------------
# Scenario matrix
# ---------------------------------------------------------------------------

# Flu only for now. The SARS-CoV-2 (pandemic) scenario is deferred until its
# calibration target is settled -- see inst/rand_analysis/00_calibrate_betas.R.
pathogens <- tibble::tribble(
  ~pathogen,      ~archetype,    ~duration_days,
  "non_pandemic", "flu",         365
)

location_types <- c("workplace", "school", "leisure", "household")
interventions <- list(uvc = uvc, glycol = glycol)

intervention_rows <- tidyr::expand_grid(
  city = names(cities),
  pathogen = pathogens$pathogen,
  intervention = names(interventions),
  location_type_treated = location_types
) |>
  dplyr::mutate(coverage = 1)

baseline_rows <- tidyr::expand_grid(
  city = names(cities),
  pathogen = pathogens$pathogen
) |>
  dplyr::mutate(
    intervention = "none",
    location_type_treated = NA_character_,
    coverage = 0
  )

run_matrix <- dplyr::bind_rows(intervention_rows, baseline_rows) |>
  dplyr::left_join(pathogens, by = "pathogen") |>
  tidyr::expand_grid(replicate = seq_len(N_REPS)) |>
  dplyr::mutate(
    run_id = sprintf(
      "%s_%s_%s_%s_rep%d",
      city, pathogen, intervention,
      dplyr::coalesce(location_type_treated, "baseline"), replicate
    ),
    # Seed depends only on city + pathogen + replicate, never on intervention
    # or location type, so every scenario in a replicate is a paired
    # comparison against the same underlying population draw.
    seed = as.integer(factor(paste(city, pathogen, replicate)))
  )

# ---------------------------------------------------------------------------
# Single-run worker
# ---------------------------------------------------------------------------

run_one_scenario <- function(row, cities, interventions, calibrated_betas, population, seed_e) {
  city_data <- cities[[row$city]]

  betas <- calibrated_betas[
    calibrated_betas$city == row$city & calibrated_betas$pathogen == row$pathogen,
  ]
  if (nrow(betas) != 1) {
    stop(sprintf(
      "Expected exactly one calibrated beta row for city=%s, pathogen=%s, found %d",
      row$city, row$pathogen, nrow(betas)
    ))
  }

  parameters_list <- get_parameters(
    archetype = row$archetype,
    overrides = list(
      human_population = population,
      number_initial_S = population - seed_e,
      number_initial_E = seed_e,
      seed = row$seed,
      simulation_time = row$duration_days,
      household_distribution_country = "custom",
      household_reference_panel = city_data$household_reference_panel,
      school_distribution_country = "custom",
      school_reference_sizes = city_data$school_reference_sizes,
      workplace_distribution_country = "USA",
      beta_community = betas$beta_community,
      beta_household = betas$beta_household,
      beta_workplace = betas$beta_workplace,
      beta_school    = betas$beta_school,
      beta_leisure   = betas$beta_leisure
    )
  )

  for (setting in c("workplace", "school", "leisure", "household")) {
    parameters_list <- set_default_ach(
      parameters_list,
      setting,
      switch(setting, workplace = 3.1, school = 3.3, leisure = 3.5, household = 2.0)
    )
  }

  if (row$intervention != "none") {
    parameters_list <- set_intervention_ach(
      parameters_list,
      setting = row$location_type_treated,
      coverage_target = "individuals",
      coverage_type = "random",
      timestep = 0,
      intervention = interventions[[row$intervention]]
    )
  }

  result <- run_simulation(parameters_list)$result
  result$run_id <- row$run_id
  result
}

# ---------------------------------------------------------------------------
# Run the matrix
# ---------------------------------------------------------------------------

cl <- parallel::makeCluster(N_WORKERS)
parallel::clusterCall(cl, function(p) { devtools::load_all(p); NULL }, pkg_path)
parallel::clusterExport(cl, c("cities", "interventions", "calibrated_betas", "POPULATION", "SEED_E", "run_one_scenario"))

run_rows <- split(run_matrix, seq_len(nrow(run_matrix)))

results <- parallel::parLapply(cl, run_rows, function(row) {
  run_one_scenario(row, cities, interventions, calibrated_betas, POPULATION, SEED_E)
})

parallel::stopCluster(cl)

# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------

dir.create("inst/rand_analysis/results", showWarnings = FALSE)
saveRDS(results, "inst/rand_analysis/results/scenario_results.rds")
readr::write_csv(run_matrix, "inst/rand_analysis/results/run_matrix.csv")

cat("Completed", length(results), "runs out of", nrow(run_matrix), "planned.\n")
