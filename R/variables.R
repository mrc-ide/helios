##' variables.R
##'
##' This file contains the function(s) required to initate the list of model variables.

create_variables <- function(parameters) {

  # Initialise and populate the disease state variable:
  disease_states <- c("S", "E", "I", "R")
  initial_disease_states <- generate_initial_disease_states(parameters)
  disease_state_variable <- individual::CategoricalVariable$new(categories = disease_states,
                                                                initial_values = initial_disease_states)

  # Initialise and populate the age class variable:
  age_classes <- c("child", "adult", "elderly")
  initial_age_classes <- generate_age_classes(parameters = parameters)
  age_class_variable <- individual::CategoricalVariable$new(categories = age_classes,
                                                            initial_values = initial_age_classes)

  # Initialise and populate the workplace setting variable:
  initial_workplace_settings <- generate_workplaces(parameters = parameters, age_class_variable = age_class_variable)
  workplace_variable <- CategoricalVariable$new(categories = as.character(0:parameters$num_workplaces), ## change this to be the maximum of as.numeric(initial_workplace_settings) when proper function is in
                                                initial_values = initial_workplace_settings)

  # Initialise and populate the school setting variable:
  initial_school_settings <- generate_schools(parameters = parameters, age_class_variable = age_class_variable)
  school_variable <- CategoricalVariable$new(categories = as.character(0:parameters$num_schools), ## change this to be the maximum of as.numeric(initial_school_settings) when proper function is in
                                             initial_values = initial_school_settings)

  # Initialise and populate the household variable
  initial_households <- generate_households(parameters = parameters, age_class_variable = age_class_variable)
  household_variable <- CategoricalVariable$new(categories = as.character(1:max(initial_households)),
                                                initial_values = as.character(initial_households))

  variables <- list(disease_state_variable,
                    age_class_variable,
                    workplace_variable,
                    school_variable,
                    household_variable)
  return(variables)

}


# Function that gets the initial disease state of all individuals the system:
generate_initial_disease_states <- function(parameters) {

  ## Checking that parameters contains "initial_infections" and "N"
  if (!("initial_infections" %in% names(parameters))) {
    stop("parameters list must contain a variable called initial_infections")
  }
  if (!("N" %in% names(parameters))) {
    stop("parameters list must contain a variable called N")
  }
  if (!("seed" %in% names(parameters))) {
    stop("parameters list must contain a variable called seed")
  }

  ## Initialising the disease states
  set.seed(parameters$seed)
  initial_disease_states <- rep("S", parameters$N)
  infection_index <- sample(x = 1:length(initial_disease_states),
                            size = parameters$initial_infections,
                            replace = FALSE, prob = NULL)
  initial_disease_states[infection_index] <- "E"
  return(initial_disease_states)

}


# Function that gets the age classes for all individuals in the system:
generate_age_classes <- function(parameters) {

  ## Checking that parameters contains "age_group_proportions" and "N"
  if (!("age_group_proportions" %in% names(parameters))) {
    stop("parameters list must contain a variable called age_group_proportions")
  }
  if (!("N" %in% names(parameters))) {
    stop("parameters list must contain a variable called N")
  }
  if (!("seed" %in% names(parameters))) {
    stop("parameters list must contain a variable called seed")
  }

  ## Initialising the disease states
  set.seed(parameters$seed)
  age_classes <- sample(c("child", "adult", "elderly"),
                        size = parameters$N, replace = TRUE,
                        prob = parameters$age_group_proportions)
  return(age_classes)

}


# Function that gets the workplace assignments for all individuals in the system:
## THIS IS JUST A PLACEHOLDER FOR THE REAL THING
generate_workplaces <- function(parameters, age_class_variable) {

  ## Replace this with a test to check the relevant parameters are held in the parameters list
  ## E.g. N, workplace size distribution parameters etc
  # if (!("initial_infections" %in% names(parameters))) {
  #   stop("parameters list must contain a variable called N")
  # }

  ## Note the below is wrong and just a placeholder - I don't think we want to fix the number of
  ## schools manually, instead let it be a function of the distribution of workplace sizes and the population size

  ## Calculating number of adults and assigning them to workplaces
  set.seed(parameters$seed)
  num_adults <- age_class_variable$get_size_of("adult") # get number of adults
  index_adults <- age_class_variable$get_index_of("adult")$to_vector() # get the index of adults in age_class_variable
  adult_workplace_assignments <- sample(as.character(1:parameters$num_workplaces), size = length(index_adults), replace = TRUE, prob = NULL) # randomly sample workplaces for each adult

  ## Creating a vector containing workplace assignments for all individuals (i.e. adult_workplace_assignments for adults,
  ## 0 for children and elderly)
  workplace_vector <- vector(mode = "character", length = parameters$N) # create an empty vector to be filled with workplace assignments
  workplace_vector[index_adults] <- adult_workplace_assignments # append workplace assignments to main workplaces vector
  workplace_vector[workplace_vector == ""] <- "0" # replace blanks with 0s (these are children/elderly people)

  return(workplace_vector)

}

# Function that generates the school assignments for all individuals in the system:
## THIS IS JUST A PLACEHOLDER FOR THE REAL THING
generate_schools <- function(parameters, age_class_variable) {

  ## Replace this with a test to check the relevant parameters are held in the parameters list
  ## E.g. N, school size distribution parameters etc
  # if (!("initial_infections" %in% names(parameters))) {
  #   stop("parameters list must contain a variable called N")
  # }

  ## Note the below is wrong and just a placeholder - I don't think we want to fix the number of
  ## schools manually, instead let it be a function of the distribution of workplace sizes and the population size

  ## Calculating number of adults and assigning them to workplaces
  set.seed(parameters$seed)
  num_children <- age_class_variable$get_size_of("child") # get number of children
  index_children <- age_class_variable$get_index_of("child")$to_vector() # get the index of children in age_class_variable
  child_school_assignments <- sample(as.character(1:parameters$num_schools), size = length(index_children), replace = TRUE, prob = NULL) # randomly sample schools for each child

  ## Creating a vector containing school assignments for all individuals (i.e. child_school_assignments for children,
  ## 0 for adults and elderly)
  schools_vector <- vector(mode = "character", length = parameters$N) # create empty vector to be filled with school assignments
  schools_vector[index_children] <- child_school_assignments # append school assignment to main schools vector
  schools_vector[schools_vector == ""] <- "0" # replace blanks with 0s (these are adults/elderly people)

  return(schools_vector)

}

# Function that generates the household assignments for all individuals in the system:
generate_households <- function(parameters, age_class_variable) {

  ## Setting seed
  set.seed(parameters$seed)

  ## Checking population size N is the same as implied by age_class_variable
  if (parameters$N != age_class_variable$get_size_of(age_class_variable$get_categories())) {
    stop("N and implied age_class_vector are different lengths")
  }

  ## Extracting out the vector of underlying values from age_class_variable
  age_class_vector <- rep("", parameters$N)
  age_class_vector[age_class_variable$get_index_of("child")$to_vector()] <- "child"
  age_class_vector[age_class_variable$get_index_of("adult")$to_vector()] <- "adult"
  age_class_vector[age_class_variable$get_index_of("elderly")$to_vector()] <- "elderly"

  ## Track which individuals are assigned
  assigned <- rep(FALSE, parameters$N)
  individual_households <- rep(NA, parameters$N)
  household_counter <- 1

  ## Looping over this whilst there still remain any unassigned individuals
  while(sum(assigned) < parameters$N) {

    # Check if only children are left unassigned - if this is the case, then we just distribute
    # them randomly across households
    unassigned_indices <- which(!assigned)
    if(all(age_class_vector[unassigned_indices] == "child")) {
      # Distribute remaining children across existing households randomly
      for(child_idx in unassigned_indices) {
        selected_household <- sample(household_counter, 1)
        assigned[child_idx] <- TRUE
        individual_households[child_idx] <- selected_household
      }
      break # Exit the main loop as all remaining unassigned individuals are children and have been assigned
    }

    # Draw household size
    household_size <- rpois(n = 1, lambda = parameters$mean_household_size)

    # Initialize temporary household storage
    temp_household <- c()

    ## Looping over this whilst current household isn't full
    while(length(temp_household) < household_size && sum(assigned) < parameters$N) {

      # Randomly select an unassigned individual
      candidates <- which(!assigned)
      selected <- sample(x = candidates, size = 1)

      # Ensure selected individual meets the household formation criteria (i.e. children have to have at least 1 adult in household)
      if(age_class_vector[selected] == "child") {

        ## Check whether current household has an adult - if it does, just add the child
        if(sum(age_class_vector[temp_household] == "adult") >= 1) {
          temp_household <- c(temp_household, selected)
          assigned[selected] <- TRUE
        } else { # if not, get an adult to be added to the household
          # Ensure there is at least one unassigned adult to pair with
          unassigned_adults <- which(age_class_vector == "adult" & !assigned)
          if(length(unassigned_adults) < 1) {
            # No available adults to pair with the child, break from the inner loop
            break
          }
          # Pair child with adult(s)
          num_adults_to_add <- 1
          adults_to_add <- sample(unassigned_adults, num_adults_to_add)
          temp_household <- c(temp_household, selected, adults_to_add)
          assigned[c(selected, adults_to_add)] <- TRUE
        }

      } else {
        # Add the individual to the household if there's enough space
        if((length(temp_household) + 1) <= household_size) {
          temp_household <- c(temp_household, selected)
          assigned[selected] <- TRUE
        }
      }
    }
    # Add the completed household to the list of households, if any members were added
    if(length(temp_household) > 0) {
      individual_households[temp_household] <- household_counter
      household_counter <- household_counter + 1
    }
  }

  return(individual_households = individual_households)
}
