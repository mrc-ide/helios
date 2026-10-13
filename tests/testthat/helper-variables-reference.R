# Reference (oracle) implementations used by test-variables.R.
#
# These are verbatim copies of generate_initial_leisure() and
# generate_initial_households_bootstrap() as they were before they were vectorised
# (one pass of the loop per person / per household). They are kept so that the
# vectorised versions can be checked against them.

reference_generate_initial_leisure <- function(parameters_list, leisure_setting_sizes) {
  # Check that the requisite parameters are present:
  if (!("human_population" %in% names(parameters_list))) {
    stop("parameters list must contain a variable called human_population")
  }
  if (!("seed" %in% names(parameters_list))) {
    stop("parameters list must contain a variable called seed")
  }
  if (!("leisure_mean_size" %in% names(parameters_list))) {
    stop("parameters list must contain a variable called leisure_mean_size")
  }
  if (!("leisure_overdispersion_size" %in% names(parameters_list))) {
    stop(
      "parameters list must contain a variable called leisure_overdispersion_size"
    )
  }
  if (!("leisure_mean_number_settings" %in% names(parameters_list))) {
    stop(
      "parameters list must contain a variable called leisure_mean_number_settings"
    )
  }

  # Setting the seed
  set.seed(parameters_list$seed)

  # Calculating the number of leisure visits that each person makes per week
  leisure_visits_per_person_per_week <- rpois(
    n = parameters_list$human_population,
    lambda = parameters_list$leisure_mean_number_settings
  )
  leisure_visits_per_person_per_week[
    leisure_visits_per_person_per_week > 7
  ] <- 7 # capping it at 1 leisure visit per day (max 7 per week)

  # Populating a list with the leisure visits made by each individual
  leisure_visit_list <- vector(
    mode = "list",
    length = parameters_list$human_population
  )
  for (i in 1:parameters_list$human_population) {
    # Creating a temporary vector of leisure visits for that person
    # -> Each element indicates where they visit on which day of the week
    #    and a 0 means nowhere visited on that day of the week
    temp_leisure_visit <- rep(0, 7)

    # Sampling which locations individuals visit for their leisure visits (weighted according to leisure setting size)
    temp_location_leisure_visits <- sample(
      x = 1:length(leisure_setting_sizes),
      size = leisure_visits_per_person_per_week[i],
      replace = FALSE,
      prob = leisure_setting_sizes
    )

    # Sampling which day(s) of the week the individual makes those visit(s) and assigning visits randomly
    days_visits_made <- sample(
      x = 1:7,
      size = leisure_visits_per_person_per_week[i],
      replace = FALSE
    )
    if (leisure_visits_per_person_per_week[i] == 1) {
      temp_leisure_visit[days_visits_made] <- temp_location_leisure_visits
    } else {
      temp_leisure_visit[days_visits_made] <- sample(
        x = temp_location_leisure_visits,
        size = leisure_visits_per_person_per_week[i],
        replace = FALSE
      )
    }
    leisure_visit_list[[i]] <- temp_leisure_visit
  }

  return(leisure_visit_list) # a list where each element contains a vector that specifies the ids of the leisure settings
  # that each individual visits e.g. list(c(2, 6, 19, 35), c(1, 8), c(6, 10, 45)... etc)
}

reference_generate_initial_households_bootstrap <- function(parameters_list) {
  ## Checking country is either "UK" or "USA"
  country <- parameters_list$household_distribution_country
  if (!(country %in% c("UK", "USA"))) {
    stop("Country specified must be either USA or UK")
  }

  ## Using data from RTI's synthetic population for San Francisco to bootstrap https://fred.publichealth.pitt.edu/syn_pops
  if (country == "USA") {
    ref_panel <- baseline_household_demographics_usa
  } else if (country == "UK") {
    ## Using Hinch et al's synthetic population from ONS
    ref_panel <- baseline_household_demographics_uk
  }

  # Creating blank age-class vectors and household assignment vectors to populate
  age_class_vector <- rep("", parameters_list$human_population)
  individual_households <- rep(NA, parameters_list$human_population)

  ## Looping over this whilst there still remain any unassigned individuals
  counter <- 1
  household_counter <- 1
  while (counter <= parameters_list$human_population) {
    ## Sampling a random household from the reference panel
    temp_index <- sample(1:nrow(ref_panel), 1, replace = TRUE)
    random_household <- ref_panel[temp_index, ]

    ## Expanding out the random household
    household_age_individuals <- rep(
      x = names(random_household),
      times = random_household
    )
    for (i in 1:length(household_age_individuals)) {
      age_class_vector[counter] <- household_age_individuals[i]
      individual_households[counter] <- household_counter
      counter <- counter + 1
    }
    household_counter <- household_counter + 1
  }

  ## Checking if there are any NAs or missing age_class assignments - if there are, randomly assigning
  ## them to houses and age-groups
  if (sum(is.na(individual_households)) > 0) {
    index_unassigned <- which(is.na(individual_households))
    if (length(index_unassigned) > 3) {
      stop("too many NAs in household generation - check this")
    }
    individual_households[index] <- sample(
      x = 1:max(individual_households),
      size = length(index_unassigned),
      replace = TRUE
    )
    age_class_vector[index] <- sample(
      x = unique(age_class_vector),
      size = length(index_unassigned),
      replace = TRUE,
      prob = unname(table(age_class_vector)) / parameters_list$human_population
    )
  }

  ## Scrambling the order of individuals so that household members don't appear right next to each other
  ## -> This might not be needed, need to give this some thought
  scrambled_index <- sample(
    x = 1:parameters_list$human_population,
    size = parameters_list$human_population,
    replace = FALSE
  )

  return(list(
    individual_households = individual_households[scrambled_index],
    age_class_vector = age_class_vector[scrambled_index]
  ))
}
