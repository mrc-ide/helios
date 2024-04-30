
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
  expect_error(object = generate_initial_disease_states(parameters_list = parameters_list),
               regexp = "parameters list must contain a variable called number_initially_exposed")

})

test_that("generate_initial_disease_states errors if parameter list does not contain human_population parameter", {

  # Establish the list of model parameters:
  parameters_list <- get_parameters()

  # Remove the number_initially_exposed parameter:
  parameters_list$human_population <- NULL

  # Check that generate_initial_disease_states() errors when number_initially_exposed not in the parameters
  # list:
  expect_error(object = generate_initial_disease_states(parameters_list = parameters_list),
               regexp = "parameters list must contain a variable called human_population")

})

test_that("generate_initial_disease_states errors if parameter list does not contain seed parameter", {

  # Establish the list of model parameters:
  parameters_list <- get_parameters()

  # Remove the number_initially_exposed parameter:
  parameters_list$seed <- NULL

  # Check that generate_initial_disease_states() errors when number_initially_exposed not in the parameters
  # list:
  expect_error(object = generate_initial_disease_states(parameters_list = parameters_list),
               regexp = "parameters list must contain a variable called seed")

})

test_that("generate_initial_disease_states returns the expected disease states", {

  # Establish the list of model parameters:
  parameters_list <- get_parameters(overrides = list(number_initially_exposed = 47))

  # Generate the initial disease states:
  initial_disease_states <- generate_initial_disease_states(parameters_list = parameters_list)

  # Check that the number of exposed individuals matches expectation:
  expect_equal(object = sum(initial_disease_states == "E"), parameters_list$number_initially_exposed)

  # Check that the number of susceptible indiivduals matches expectation:
  expect_equal(sum(initial_disease_states == "S"), parameters_list$human_population - parameters_list$number_initially_exposed)

})

test_that("generate_initial_disease_states returns vector containing only susceptible and exposed individuals", {

  # Establish list of model parameters:
  parameters_list <- get_parameters()

  # Generate the initial disease states:
  initial_disease_states <- generate_initial_disease_states(parameters_list = parameters_list)

  # Generate a vector of the disease states:
  disease_states <- c("S", "E")

  # Check that the initial disease states are all recognised disease states
  expect_contains(initial_disease_states, disease_states)


})

#========================================#
#===== generate_initial_age_classes =====#
#========================================#

test_that("generate_initial_age_classes errors if parameter list does not contain initial_proportion_child parameter", {

  # Establish the list of model parameters:
  parameters_list <- get_parameters()

  # Remove the initial_proportion_child parameter:
  parameters_list$initial_proportion_child <- NULL

  # Check that generate_initial_age_classes() errors when initial_proportion_child not in the parameters
  # list:
  expect_error(object = generate_initial_age_classes(parameters_list = parameters_list),
               regexp = "parameters list must contain a variable called initial_proportion_child")

})

test_that("generate_initial_age_classes errors if parameter list does not contain initial_proportion_adult parameter", {

  # Establish the list of model parameters:
  parameters_list <- get_parameters()

  # Remove the initial_proportion_adult parameter:
  parameters_list$initial_proportion_adult <- NULL

  # Check that generate_initial_age_classes() errors when initial_proportion_adult not in the parameters
  # list:
  expect_error(object = generate_initial_age_classes(parameters_list = parameters_list),
               regexp = "parameters list must contain a variable called initial_proportion_adult")

})

test_that("generate_initial_age_classes errors if parameter list does not contain initial_proportion_elderly parameter", {

  # Establish the list of model parameters:
  parameters_list <- get_parameters()

  # Remove the initial_proportion_elderly parameter:
  parameters_list$initial_proportion_elderly <- NULL

  # Check that generate_initial_age_classes() errors when initial_proportion_elderly not in the parameters
  # list:
  expect_error(object = generate_initial_age_classes(parameters_list = parameters_list),
               regexp = "parameters list must contain a variable called initial_proportion_elderly")

})

test_that("generate_initial_age_classes errors if parameter list does not contain human_population parameter", {

  # Establish the list of model parameters:
  parameters_list <- get_parameters()

  # Remove the human_population parameter:
  parameters_list$human_population <- NULL

  # Check that generate_initial_age_classes() errors when human_population not in the parameters
  # list:
  expect_error(object = generate_initial_age_classes(parameters_list = parameters_list),
               regexp = "parameters list must contain a variable called human_population")

})

test_that("generate_initial_age_classes errors if parameter list does not contain seed parameter", {

  # Establish the list of model parameters:
  parameters_list <- get_parameters()

  # Remove the seed parameter:
  parameters_list$seed <- NULL

  # Check that generate_initial_age_classes() errors when seed not in the parameters
  # list:
  expect_error(object = generate_initial_age_classes(parameters_list = parameters_list),
               regexp = "parameters list must contain a variable called seed")

})

test_that("generate_initial_age_classes contains only the expected age classes", {

  # Establish the list of model parameters:
  parameters_list <- get_parameters(overrides = list(initial_proportion_child = 0.5,
                                                     initial_proportion_adult = 0.2,
                                                     initial_proportion_elderly = 0.3,
                                                     human_population = 1000))

  # Generate the initial age classes:
  initial_age_classes <- generate_initial_age_classes(parameters_list = parameters_list)

  # Store the age classes:
  age_classes <- c("child", "adult", "elderly")

  # Check that all values in the initial age classes are child, adult, or elderly
  expect_contains(object = unique(initial_age_classes), expected = age_classes)

})

test_that("generate_initial_age_classes errors if initial age class proportions do not sum to 1", {

  # Establish parameter list with initial child proportion >1
  parameters_list <- get_parameters(overrides = list(initial_proportion_child = 1.2))

  # Check that the generate_initial_age_classes function errors when age class proportions do not
  # sum to 1:
  expect_error(object = generate_initial_age_classes(parameters_list = parameters_list),
               regexp = "initial age class proportions do not sum to 1")

})

#====================================#
#===== generate_initial_schools =====#
#====================================#

test_that("generate_initial_schools errors if parameter_list does not contain human_population", {

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
  expect_error(object = generate_initial_schools(parameters_list = parameters_list, age_class_variable = variables_list$age_class),
               regexp = "parameters list must contain a variable called human_population")

})

test_that("generate_initial_schools errors if parameter_list does not contain seed", {

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
  expect_error(object = generate_initial_schools(parameters_list = parameters_list, age_class_variable = variables_list$age_class),
               regexp = "parameters list must contain a variable called seed")

})

test_that("generate_initial_schools errors if parameter_list does not contain school_meanlog", {

  # Establish the list of model parameters:
  parameters_list <- get_parameters()

  # Establish the list of model variables:
  vars_and_params <- create_variables(parameters_list = parameters_list)

  # Retrieve the variables from the create_variables() outputs:
  variables_list <- vars_and_params[[1]]

  # Re-establish the model parameters list:
  parameters_list <- vars_and_params[[2]]

  # Remove school_meanlog from the parameters list:
  parameters_list$school_meanlog <- NULL

  # Check that the generate_initial_schools() function errors due to missing school_meanlog parameter:
  expect_error(object = generate_initial_schools(parameters_list = parameters_list, age_class_variable = variables_list$age_class),
               regexp = "parameters list must contain a variable called school_meanlog")

})

test_that("generate_initial_schools errors if parameter_list does not contain school_sdlog", {

  # Establish the list of model parameters:
  parameters_list <- get_parameters()

  # Establish the list of model variables:
  vars_and_params <- create_variables(parameters_list = parameters_list)

  # Retrieve the variables from the create_variables() outputs:
  variables_list <- vars_and_params[[1]]

  # Re-establish the model parameters list:
  parameters_list <- vars_and_params[[2]]

  # Remove school_meanlog from the parameters list:
  parameters_list$school_sdlog <- NULL

  # Check that the generate_initial_schools() function errors due to missing school_sdlog parameter:
  expect_error(object = generate_initial_schools(parameters_list = parameters_list, age_class_variable = variables_list$age_class),
               regexp = "parameters list must contain a variable called school_sdlog")

})

test_that("generate_initial_schools errors if parameter_list does not contain school_student_staff_ratio", {

  # Establish the list of model parameters:
  parameters_list <- get_parameters()

  # Establish the list of model variables:
  vars_and_params <- create_variables(parameters_list = parameters_list)

  # Retrieve the variables from the create_variables() outputs:
  variables_list <- vars_and_params[[1]]

  # Re-establish the model parameters list:
  parameters_list <- vars_and_params[[2]]

  # Remove school_meanlog from the parameters list:
  parameters_list$school_student_staff_ratio <- NULL

  # Check that the generate_initial_schools() function errors due to missing school_student_staff_ratio parameter:
  expect_error(object = generate_initial_schools(parameters_list = parameters_list, age_class_variable = variables_list$age_class),
               regexp = "parameters list must contain a variable called school_student_staff_ratio")

})

test_that("generate_initial_schools returns a vector equal in length to the number of people simulated and assigns some zeroes", {

  # Establish the list of model parameters:
  parameters_list <- get_parameters()

  # Establish the list of model variables:
  vars_and_params <- create_variables(parameters_list = parameters_list)

  # Retrieve the variables from the create_variables() outputs:
  variables_list <- vars_and_params[[1]]

  # Re-establish the model parameters list:
  parameters_list <- vars_and_params[[2]]

  # Generate the vector of initial schools:
  initial_schools <- generate_initial_schools(parameters_list = parameters_list, age_class_variable = variables_list$age_class)

  # Check that the schools object has entries for each individual in the population:
  expect_length(object = initial_schools, n = parameters_list$human_population)

  # Check that the schools object contains a non-zero number of "0" (no school) entries:
  expect_gt(object = sum(initial_schools == "0"), 0)


})

test_that("generate_initial_schools assigns at least one adult to each school", {

  # Establish the list of model parameters:
  parameters_list <- get_parameters()

  # Establish the list of model variables:
  vars_and_params <- create_variables(parameters_list = parameters_list)

  # Retrieve the variables from the create_variables() outputs:
  variables_list <- vars_and_params[[1]]

  # Re-establish the model parameters list:
  parameters_list <- vars_and_params[[2]]

  # Generate the vector of initial schools:
  initial_schools <- generate_initial_schools(parameters_list = parameters_list, age_class_variable = variables_list$age_class)

  # Get the indices of all adults in the population:
  adult_age_class_indices <- variables_list$age_class$get_index_of("adult")$to_vector()
  elderly_age_class_indices <- variables_list$age_class$get_index_of("elderly")$to_vector()


  # Check that all schools have at least one adult assigned to them:
  expect_true(all(table(as.numeric(initial_schools[adult_age_class_indices])) > 0))

  # Ensure that no elderly individuals are assigned schools
  expect_equal(object = sum(initial_schools[elderly_age_class_indices] > 0), 0)

})

test_that("generate_initial_schools assigns no elderly individuals to any school", {

  # Establish the list of model parameters:
  parameters_list <- get_parameters()

  # Establish the list of model variables:
  vars_and_params <- create_variables(parameters_list = parameters_list)

  # Retrieve the variables from the create_variables() outputs:
  variables_list <- vars_and_params[[1]]

  # Re-establish the model parameters list:
  parameters_list <- vars_and_params[[2]]

  # Generate the vector of initial schools:
  initial_schools <- generate_initial_schools(parameters_list = parameters_list, age_class_variable = variables_list$age_class)

  # Get the indices of all elderly individuals in the population:
  elderly_age_class_indices <- variables_list$age_class$get_index_of("elderly")$to_vector()

  # Check that all schools have no elderly individuals assigned to them:
  expect_identical(object = sum(as.numeric(initial_schools[elderly_age_class_indices])), 0)

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
  expect_error(object = generate_initial_workplaces(parameters_list = parameters_list,
                                                    age_class_variable = variables_list$age_class,
                                                    school_variable = variables_list$school),
               regexp = "parameters list must contain a variable called human_population")

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
  expect_error(object = generate_initial_workplaces(parameters_list = parameters_list,
                                                    age_class_variable = variables_list$age_class,
                                                    school_variable = variables_list$school),
               regexp = "parameters list must contain a variable called seed")

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
  expect_error(object = generate_initial_workplaces(parameters_list = parameters_list,
                                                    age_class_variable = variables_list$age_class,
                                                    school_variable = variables_list$school),
               regexp = "parameters list must contain a variable called workplace_prop_max")

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
  expect_error(object = generate_initial_workplaces(parameters_list = parameters_list,
                                                    age_class_variable = variables_list$age_class,
                                                    school_variable = variables_list$school),
               regexp = "parameters list must contain a variable called workplace_a")

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
  expect_error(object = generate_initial_workplaces(parameters_list = parameters_list,
                                                    age_class_variable = variables_list$age_class,
                                                    school_variable = variables_list$school),
               regexp = "parameters list must contain a variable called workplace_c")

})


#====================================#
#===== generate_initial_leisure =====#
#====================================#




#=======================================#
#===== generate_initial_households =====#
#=======================================#




#=================================================#
#===== generate_initial_households_bootstrap =====#
#=================================================#







