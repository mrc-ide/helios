## Loading required libraries
library(individual)

## Model parameters
parameters <- list(initial_infections = 5, N = 100, age_group_proportions = c(0.1, 0.6, 0.1),
                   num_workplaces = 5, num_schools = 2, seed = 20, mean_household_size = 3)

## Testing disease states function
generate_initial_disease_states(parameters)

## Testing age classes function
test_age_classes <- generate_age_classes(parameters)
age_classes <- c("child", "adult", "elderly")
test_age_class_variable <- individual::CategoricalVariable$new(categories = age_classes,
                                                               initial_values = test_age_classes)
## Testing workplace function
generate_workplaces(parameters, test_age_class_variable)

## Testing schools function
generate_schools(parameters, test_age_class_variable)

## Testing household function
generate_households(parameters = parameters, age_class_variable = test_age_class_variable)

## Testing creation of all parameters
create_variables(parameters)


N <- 10000  # Example population size
age_class_variable <- sample(c("child", "adult", "elderly"), N, replace = TRUE)  # Example age class distribution
test_age_class_variable <- individual::CategoricalVariable$new(categories = age_classes,
                                                               initial_values = age_class_variable)
parameters <- list(initial_infections = 5, N = N, age_group_proportions = c(0.1, 0.6, 0.1),
                   num_workplaces = 5, num_schools = 2, seed = 20, mean_household_size = 3)
households <- generate_households(parameters, test_age_class_variable)
sum(is.na(households))
hist(table(households), breaks = 20)
hist(truncdist::rtrunc(n = max(households), spec = "pois", a = 0, b = Inf, lambda = parameters$mean_household_size), breaks = 20)

table(table(households)) / max(households)
table(truncdist::rtrunc(n = N, spec = "pois", a = 0, b = Inf, lambda = parameters$mean_household_size)) / N
