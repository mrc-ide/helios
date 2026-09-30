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

devtools::load_all()
source("inst/rand_analysis/build_city_data.R")
source("inst/rand_analysis/01_interventions.R")

N_REPS     <- as.integer(Sys.getenv("RAND_N_REPS", "3"))
POPULATION <- as.integer(Sys.getenv("RAND_POP", "250000"))
N_WORKERS  <- max(1, min(10, parallel::detectCores() - 1))

# ---------------------------------------------------------------------------
# City data
# ---------------------------------------------------------------------------

cities <- list(
  san_francisco = readRDS("inst/rand_analysis/city_data/san_francisco.rds"),
  pittsburgh    = readRDS("inst/rand_analysis/city_data/pittsburgh.rds"),
  nyc           = readRDS("inst/rand_analysis/city_data/nyc.rds")
)

# ---------------------------------------------------------------------------
# Scenario matrix
# ---------------------------------------------------------------------------

pathogens <- tibble::tribble(
  ~pathogen,      ~archetype,    ~duration_days,
  "non_pandemic", "flu",         365,
  "pandemic",     "sars_cov_2",  150
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

run_one_scenario <- function(row, cities, interventions, population) {
  city_data <- cities[[row$city]]

  parameters_list <- get_parameters(
    archetype = row$archetype,
    overrides = list(
      human_population = population,
      number_initial_S = population - 5,
      number_initial_E = 5,
      seed = row$seed,
      simulation_time = row$duration_days,
      household_distribution_country = "custom",
      household_reference_panel = city_data$household_reference_panel,
      school_distribution_country = "custom",
      school_reference_sizes = city_data$school_reference_sizes,
      workplace_distribution_country = "USA"
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
parallel::clusterCall(cl, function() { devtools::load_all(); NULL })
parallel::clusterExport(cl, c("cities", "interventions", "POPULATION", "run_one_scenario"))

run_rows <- split(run_matrix, seq_len(nrow(run_matrix)))

results <- parallel::parLapply(cl, run_rows, function(row) {
  run_one_scenario(row, cities, interventions, POPULATION)
})

parallel::stopCluster(cl)

# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------

dir.create("inst/rand_analysis/results", showWarnings = FALSE)
saveRDS(results, "inst/rand_analysis/results/scenario_results.rds")
readr::write_csv(run_matrix, "inst/rand_analysis/results/run_matrix.csv")

cat("Completed", length(results), "runs out of", nrow(run_matrix), "planned.\n")
