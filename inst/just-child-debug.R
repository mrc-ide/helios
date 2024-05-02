library(helios)

parameters_list <- get_parameters(
  list(human_population = 10000, seed = 1)
)

age_classes <- c("child", "adult", "elderly")
initial_age_classes <- helios:::generate_initial_age_classes(parameters_list)
age_class_variable <- individual::CategoricalVariable$new(categories = age_classes, initial_values = initial_age_classes)

# temp <- generate_initial_households(parameters_list, age_class_variable)

# Check that the requisite parameters are present:
if (!("human_population" %in% names(parameters_list))) {
  stop("parameters list must contain a variable called human_population")
}
if (!("seed" %in% names(parameters_list))) {
  stop("parameters list must contain a variable called seed")
}
if (!("mean_household_size" %in% names(parameters_list))) {
  stop("parameters list must contain a variable called mean_household_size")
}

# Setting seed
set.seed(parameters_list$seed)

if (parameters_list$human_population != age_class_variable$get_size_of(age_class_variable$get_categories())) {
  stop("Human population and age_class_vector are different lengths")
}

# Extracting vector of underlying values from age_class_variable
age_class_vector <- rep("", parameters_list$human_population)
age_class_vector[age_class_variable$get_index_of("child")$to_vector()] <- "child"
age_class_vector[age_class_variable$get_index_of("adult")$to_vector()] <- "adult"
age_class_vector[age_class_variable$get_index_of("elderly")$to_vector()] <- "elderly"

# Track which individuals are assigned
assigned <- rep(FALSE, parameters_list$human_population)
individual_households <- rep(NA, parameters_list$human_population)
household_counter <- 1
if_loop <- list()

# Looping over this while there remain unassigned individuals
while(sum(assigned) < parameters_list$human_population) {

  unassigned_indices <- which(!assigned)

  # If only children are left unassigned then distribute them randomly across households
  if(all(age_class_vector[unassigned_indices] == "child")) {
    for(child_idx in unassigned_indices) {
      selected_household <- sample(household_counter, 1)
      assigned[child_idx] <- TRUE
      individual_households[child_idx] <- selected_household
    }
    break
  }

  # Draw household size
  household_size <- rpois(n = 1, lambda = parameters_list$mean_household_size)

  # Initialize temporary household storage
  temp_household <- c()

  # Looping over this while temp_household isn't full
  while(length(temp_household) < household_size && sum(assigned) < parameters_list$human_population) {

    # Randomly select an unassigned individual
    candidates <- which(!assigned)
    selected <- sample(x = candidates, size = 1)

    # Ensure selected individual meets the household formation criteria (i.e. children have to have at least 1 adult in household)
    if(age_class_vector[selected] == "child") {

      message("Assigning child!")

      # Check whether current household has an adult - if it does, just add the child
      if(sum(age_class_vector[temp_household] == "adult") >= 1) {
        temp_household <- c(temp_household, selected)
        assigned[selected] <- TRUE
      } else {
        message("In the else loop!")
        if_loop[[household_counter]] <- TRUE
        # Ensure there is at least one unassigned adult to pair with
        unassigned_adults <- which(age_class_vector == "adult" & !assigned)
        if(length(unassigned_adults) < 1) {
          message("No available adults to pair child with!")
          break
        }
        # Pair child with adult(s)
        adults_to_add <- sample(unassigned_adults, 1)
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

which(sapply(if_loop, isTRUE))

df <- data.frame(individual_households, age_class_vector)

just_child <- 0
just_child_list <- list()

for(i in individual_households) {
  household <- df[df$individual_households == i, ]
  if(all(household$age_class_vector == "child")) {
    just_child <- just_child + 1
    just_child_list[[i]] <- household$age_class_vector
  }
}

names(just_child_list) <- seq_along(just_child_list)
just_child_list[sapply(just_child_list, is.null)] <- NULL
just_child_list
