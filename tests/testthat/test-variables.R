#============================#
#===== create_variables =====#
#============================#

#===========================================#
#===== generate_initial_disease_states =====#
#===========================================#

test_that("generate_initial_disease_states errors if parameter list does not contain number_initially_exposed parameter", {
  # Establish the list of model parameters:
  parameters_list <- get_parameters()

  # Remove the number_initially_exposed parameter:
  parameters_list$number_initially_exposed <- NULL

  # Check that generate_initial_disease_states() errors when number_initially_exposed not in the parameters
  # list:
  expect_error(
    object = generate_initial_disease_states(parameters_list = parameters_list),
    regexp = "parameters list must contain a variable called number_initially_exposed"
  )
})

test_that("generate_initial_disease_states errors if parameter list does not contain human_population parameter", {
  # Establish the list of model parameters:
  parameters_list <- get_parameters()

  # Remove the number_initially_exposed parameter:
  parameters_list$human_population <- NULL

  # Check that generate_initial_disease_states() errors when number_initially_exposed not in the parameters
  # list:
  expect_error(
    object = generate_initial_disease_states(parameters_list = parameters_list),
    regexp = "parameters list must contain a variable called human_population"
  )
})

test_that("generate_initial_disease_states errors if parameter list does not contain seed parameter", {
  # Establish the list of model parameters:
  parameters_list <- get_parameters()

  # Remove the number_initially_exposed parameter:
  parameters_list$seed <- NULL

  # Check that generate_initial_disease_states() errors when number_initially_exposed not in the parameters
  # list:
  expect_error(
    object = generate_initial_disease_states(parameters_list = parameters_list),
    regexp = "parameters list must contain a variable called seed"
  )
})

test_that("generate_initial_disease_states returns the expected disease states", {
  # Establish the list of model parameters:
  parameters_list <- get_parameters(
    overrides = list(number_initially_exposed = 47)
  )

  # Generate the initial disease states:
  initial_disease_states <- generate_initial_disease_states(
    parameters_list = parameters_list
  )

  # Check that the number of exposed individuals matches expectation:
  expect_equal(
    object = sum(initial_disease_states == "E"),
    parameters_list$number_initially_exposed
  )

  # Check that the number of susceptible indiivduals matches expectation:
  expect_equal(
    sum(initial_disease_states == "S"),
    parameters_list$human_population - parameters_list$number_initially_exposed
  )
})

test_that("generate_initial_disease_states returns vector containing only susceptible and exposed individuals", {
  # Establish list of model parameters:
  parameters_list <- get_parameters()

  # Generate the initial disease states:
  initial_disease_states <- generate_initial_disease_states(
    parameters_list = parameters_list
  )

  # Generate a vector of the disease states:
  disease_states <- c("S", "E")

  # Check that the initial disease states are all recognised disease states
  expect_contains(initial_disease_states, disease_states)
})

#==============================================#
#===== generate_initial_schools_bootstrap =====#
#==============================================#

#=======================================#
#===== generate_initial_workplaces =====#
#=======================================#

test_that("generate_initial_workplaces errors if parameter_list does not contain human_population", {
  # Establish the list of model parameters:
  parameters_list <- get_parameters()

  # Establish the list of model variables:
  vars_and_params <- create_variables(parameters_list = parameters_list)

  # Retrieve the variables from the create_variables() outputs:
  variables_list <- vars_and_params[[1]]

  # Re-establish the model parameters list:
  parameters_list <- vars_and_params[[2]]

  # Remove human_population from the parameters list:
  parameters_list$human_population <- NULL

  # Check that the generate_initial_schools() function errors due to missing human_population parameter:
  expect_error(
    object = generate_initial_workplaces(
      parameters_list = parameters_list,
      age_class_variable = variables_list$age_class,
      school_variable = variables_list$school
    ),
    regexp = "parameters list must contain a variable called human_population"
  )
})

test_that("generate_initial_workplaces errors if parameter_list does not contain seed", {
  # Establish the list of model parameters:
  parameters_list <- get_parameters()

  # Establish the list of model variables:
  vars_and_params <- create_variables(parameters_list = parameters_list)

  # Retrieve the variables from the create_variables() outputs:
  variables_list <- vars_and_params[[1]]

  # Re-establish the model parameters list:
  parameters_list <- vars_and_params[[2]]

  # Remove seed from the parameters list:
  parameters_list$seed <- NULL

  # Check that the generate_initial_schools() function errors due to missing seed parameter:
  expect_error(
    object = generate_initial_workplaces(
      parameters_list = parameters_list,
      age_class_variable = variables_list$age_class,
      school_variable = variables_list$school
    ),
    regexp = "parameters list must contain a variable called seed"
  )
})

test_that("generate_initial_workplaces errors if parameter_list does not contain workplace_prop_max", {
  # Establish the list of model parameters:
  parameters_list <- get_parameters()

  # Establish the list of model variables:
  vars_and_params <- create_variables(parameters_list = parameters_list)

  # Retrieve the variables from the create_variables() outputs:
  variables_list <- vars_and_params[[1]]

  # Re-establish the model parameters list:
  parameters_list <- vars_and_params[[2]]

  # Remove workplace_prop_max from the parameters list:
  parameters_list$workplace_prop_max <- NULL

  # Check that the generate_initial_schools() function errors due to missing workplace_prop_max parameter:
  expect_error(
    object = generate_initial_workplaces(
      parameters_list = parameters_list,
      age_class_variable = variables_list$age_class,
      school_variable = variables_list$school
    ),
    regexp = "parameters list must contain a variable called workplace_prop_max"
  )
})

test_that("generate_initial_workplaces errors if parameter_list does not contain workplace_a", {
  # Establish the list of model parameters:
  parameters_list <- get_parameters()

  # Establish the list of model variables:
  vars_and_params <- create_variables(parameters_list = parameters_list)

  # Retrieve the variables from the create_variables() outputs:
  variables_list <- vars_and_params[[1]]

  # Re-establish the model parameters list:
  parameters_list <- vars_and_params[[2]]

  # Remove workplace_a from the parameters list:
  parameters_list$workplace_a <- NULL

  # Check that the generate_initial_schools() function errors due to missing workplace_a parameter:
  expect_error(
    object = generate_initial_workplaces(
      parameters_list = parameters_list,
      age_class_variable = variables_list$age_class,
      school_variable = variables_list$school
    ),
    regexp = "parameters list must contain a variable called workplace_a"
  )
})

test_that("generate_initial_workplaces errors if parameter_list does not contain workplace_c", {
  # Establish the list of model parameters:
  parameters_list <- get_parameters()

  # Establish the list of model variables:
  vars_and_params <- create_variables(parameters_list = parameters_list)

  # Retrieve the variables from the create_variables() outputs:
  variables_list <- vars_and_params[[1]]

  # Re-establish the model parameters list:
  parameters_list <- vars_and_params[[2]]

  # Remove workplace_c from the parameters list:
  parameters_list$workplace_c <- NULL

  # Check that the generate_initial_schools() function errors due to missing workplace_c parameter:
  expect_error(
    object = generate_initial_workplaces(
      parameters_list = parameters_list,
      age_class_variable = variables_list$age_class,
      school_variable = variables_list$school
    ),
    regexp = "parameters list must contain a variable called workplace_c"
  )
})

#====================================#
#===== generate_initial_leisure =====#
#====================================#

#=================================================#
#===== generate_initial_households_bootstrap =====#
#==================================================#
