##' variables.R
##'
##' This file contains the function(s) required to initate the list of model variables.

create_variables <- function(parameters) {

  # Initiate and populate the disease state variable:
  states <- c('S', 'D', 'A', 'U', 'Tr')
  initial_states <- initial_state(parameters)
  state <- individual::CategoricalVariable$new(categories = states, initial_values = initial_states)

  # Initiate and populate the age class variable:
  age_classes <- c('child', 'adult', 'elderly')
  initial_age_classes <- initial_age_classes(parameters = parameters)
  age_class <- individual::CategoricalVariable$new(categories = age_classes, initial_values = initial_states)

  # Initiate and populate the workplace setting variable:
  workplace_settings <- c('office', 'school', 'healthcare_facility', 'leisure')
  initial_workplace_settings <- initial_workplace_settings(parameters = parameters)
  workplace_setting <- individual::CategoricalVariable$new(categories = workplace_settings, initial_values = initial_workplace_settings)

  variables <- list(state,
                    age_class,
                    workplace_setting)

}


# Function that gets the initial state of all individuals the system:
initial_state <- function(parameters) {

}


# Function that gets the initial age classes of all individuals the system:
initial_age_class <- function(parameters) {

}


# Function that gets the initial settings for all individuals in the system:
initial_workplace_setting <- function(parameters) {


}

