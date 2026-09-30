# Builds everything helios needs for a city from RTI's 2010 SyntheticPopulations
# data (github.com/RTIInternational/SyntheticPopulations), keyed by FIPS code(s).
# A city made of multiple counties (e.g. NYC) is supported by passing several
# FIPS codes; their household/school/workplace data are pooled.
#
# Usage:
#   source("inst/rand_analysis/build_city_data.R")
#   sf  <- build_city_data("06075", "san_francisco")
#   pgh <- build_city_data("42003", "pittsburgh")
#   nyc <- build_city_data(c("36005", "36047", "36061", "36081", "36085"), "nyc")
#
# Each result is a list with:
#   household_reference_panel -- data frame (child, adult, elderly), one row per household
#   school_reference_sizes    -- numeric vector of real per-school enrollment totals
#   workplace_a, workplace_c, workplace_prop_max -- fitted offset truncated power
#     distribution parameters (see sample_offset_truncated_power_distribution())
#   n_households, n_schools, n_workplaces -- counts, for sanity-checking
#
# To use in a run:
#   p <- get_parameters(overrides = list(
#     household_distribution_country = "custom",
#     household_reference_panel = sf$household_reference_panel,
#     school_distribution_country = "custom",
#     school_reference_sizes = sf$school_reference_sizes,
#     workplace_distribution_country = "custom",
#     workplace_a = sf$workplace_a,
#     workplace_c = sf$workplace_c,
#     workplace_prop_max = sf$workplace_prop_max,
#     ...
#   ))

#' Download and extract one county's RTI SyntheticPopulations zip
#'
#' Caches the extracted files under a local directory so repeated calls
#' (e.g. rerunning a driver script) don't re-download.
#'
#' @param fips A single 5-digit FIPS code, e.g. "06075".
#' @param cache_dir Directory to download/extract into.
download_rti_county <- function(fips, cache_dir) {
  county_dir <- file.path(cache_dir, fips)
  if (dir.exists(county_dir) && length(list.files(county_dir)) > 0) {
    return(county_dir)
  }

  state_fips <- substr(fips, 1, 2)
  url <- sprintf(
    "https://media.githubusercontent.com/media/RTIInternational/SyntheticPopulations/main/2010/County/%s/2010_ver1_%s.zip",
    state_fips,
    fips
  )
  zip_path <- file.path(cache_dir, paste0(fips, ".zip"))
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  download_result <- utils::download.file(
    url,
    zip_path,
    quiet = TRUE,
    mode = "wb"
  )
  if (download_result != 0) {
    stop("Failed to download RTI data for FIPS ", fips, " from ", url)
  }

  dir.create(county_dir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(zip_path, exdir = county_dir)
  county_dir
}

#' Fit the offset truncated power distribution to a vector of real establishment sizes
#'
#' Matches the model's own CDF (see sample_offset_truncated_power_distribution())
#' to the empirical CDF of the supplied sizes via least squares.
#'
#' @param sizes Numeric vector of real establishment sizes.
fit_offset_truncated_power_distribution <- function(sizes) {
  max_val <- max(sizes)
  m_grid <- 1:max_val
  empirical_cdf_vals <- sapply(m_grid, function(m) mean(sizes <= m))

  model_cdf <- function(m, a, c) {
    1 - (((1 + max_val / a) / (1 + m / a))^c - 1) / ((1 + max_val / a)^c - 1)
  }

  loss <- function(par) {
    a <- par[1]
    c <- par[2]
    if (a <= 0 || c <= 0) {
      return(1e10)
    }
    sum((model_cdf(m_grid, a, c) - empirical_cdf_vals)^2)
  }

  fit <- stats::optim(
    par = c(5.36, 1.34),
    fn = loss,
    method = "Nelder-Mead",
    control = list(maxit = 5000)
  )

  list(
    a = fit$par[1],
    c = fit$par[2],
    prop_max = max_val / sum(sizes)
  )
}

#' Keep only rows within a radius (km) of a center point
#'
#' Uses a flat-earth approximation (fine at city scale). Returns a logical mask.
#'
#' @param lat,lon Numeric vectors of coordinates to test.
#' @param center_lat,center_lon Numeric scalars giving the center point.
#' @param radius_km Numeric scalar radius in kilometers.
within_radius_km <- function(lat, lon, center_lat, center_lon, radius_km) {
  km_per_deg_lat <- 111.0
  km_per_deg_lon <- 111.0 * cos(center_lat * pi / 180)
  dist_km <- sqrt(
    ((lat - center_lat) * km_per_deg_lat)^2 +
      ((lon - center_lon) * km_per_deg_lon)^2
  )
  dist_km <= radius_km
}

#' Build the full set of city-specific helios inputs from RTI data
#'
#' @param fips_codes Character vector of one or more 5-digit FIPS codes.
#' Multiple codes are pooled together (e.g. NYC's five boroughs).
#' @param city_name A short label used only for cache subdirectory naming.
#' @param cache_dir Directory to cache downloaded/extracted RTI files in.
#' Defaults to a subdirectory of the R session's temp directory.
#' @param geo_filter Optional list with `center_lat`, `center_lon`, and
#' `radius_km`, used when a FIPS code covers a wider area than the city of
#' interest (e.g. a county spanning both a city and its suburbs). Households,
#' schools, and workplaces are each filtered to this radius using their own
#' coordinates before being pooled. Default = NULL (no filtering, use the
#' full FIPS area as-is).
#'
#' @section Known issue -- workplace_a/workplace_c/workplace_prop_max:
#' The `workers` field in RTI's workplaces.txt does not check out against
#' real employment figures (e.g. it sums to several times San Francisco's
#' actual job count), and the workers-per-household ratio varies 6.6x-24.6x
#' across the counties checked (SF, Allegheny, and NYC's five boroughs) --
#' too inconsistent to be a uniform scaling artifact that would at least
#' preserve the *shape* of the fit. Do not use the fitted workplace_a/
#' workplace_c/workplace_prop_max for a live analysis without re-validating
#' this first; the household_reference_panel and school_reference_sizes
#' outputs do not have this problem and checked out against real household/
#' school counts.
build_city_data <- function(
  fips_codes,
  city_name,
  cache_dir = file.path(tempdir(), "rti_cache"),
  geo_filter = NULL
) {
  household_rows <- list()
  school_sizes <- c()
  workplace_sizes <- c()

  for (fips in fips_codes) {
    county_dir <- download_rti_county(fips, cache_dir)
    files <- list.files(county_dir, full.names = TRUE)

    people_file <- files[grepl("synth_people\\.txt$", files)]
    households_file <- files[grepl("synth_households\\.txt$", files)]
    schools_file <- files[grepl("_schools\\.txt$", files)]
    workplaces_file <- files[grepl("_workplaces\\.txt$", files)]

    people <- utils::read.csv(people_file, stringsAsFactors = FALSE)
    schools <- utils::read.csv(schools_file, stringsAsFactors = FALSE)
    workplaces <- utils::read.csv(workplaces_file, stringsAsFactors = FALSE)

    if (!is.null(geo_filter)) {
      households <- utils::read.csv(households_file, stringsAsFactors = FALSE)
      keep_hh_ids <- households$sp_id[within_radius_km(
        households$latitude, households$longitude,
        geo_filter$center_lat, geo_filter$center_lon, geo_filter$radius_km
      )]
      people <- dplyr::filter(people, sp_hh_id %in% keep_hh_ids)

      schools <- dplyr::filter(schools, within_radius_km(
        latitude, longitude,
        geo_filter$center_lat, geo_filter$center_lon, geo_filter$radius_km
      ))
      workplaces <- dplyr::filter(workplaces, within_radius_km(
        latitude, longitude,
        geo_filter$center_lat, geo_filter$center_lon, geo_filter$radius_km
      ))
    }

    household_rows[[fips]] <- dplyr::group_by(people, sp_hh_id) |>
      dplyr::summarise(
        child = sum(age <= 18),
        adult = sum(age > 18 & age <= 69),
        elderly = sum(age >= 70),
        .groups = "drop"
      ) |>
      dplyr::select(child, adult, elderly)

    # A small number of rows carry latitude/longitude (0, 0) and implausible
    # enrollment totals -- data-entry errors unrelated to this county (e.g. a
    # Maine school record with a multi-million enrollment turned up in the
    # SF extract). Drop them regardless of which city's data is being pulled.
    valid_schools <- !is.na(schools$total) & schools$total > 0 & schools$latitude != 0
    school_sizes <- c(school_sizes, schools$total[valid_schools])
    workplace_sizes <- c(workplace_sizes, workplaces$workers[!is.na(workplaces$workers) & workplaces$workers > 0])
  }

  household_reference_panel <- dplyr::bind_rows(household_rows)
  workplace_fit <- fit_offset_truncated_power_distribution(workplace_sizes)

  list(
    city_name = city_name,
    fips_codes = fips_codes,
    household_reference_panel = household_reference_panel,
    school_reference_sizes = school_sizes,
    workplace_a = workplace_fit$a,
    workplace_c = workplace_fit$c,
    workplace_prop_max = workplace_fit$prop_max,
    n_households = nrow(household_reference_panel),
    n_schools = length(school_sizes),
    n_workplaces = length(workplace_sizes)
  )
}
