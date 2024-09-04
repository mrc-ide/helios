run_simulation_hipercow <- function(parameters) {

  # Load in the malariasimulation package:
  library(helios)

  # Run the simulation:
  s <- helios::run_simulation(parameters_list = parameters)

  # Append the simulation identifier to the simulation outputs:
  s$ID <- parameters$ID
  s$iteration <- parameters$iteration
  s$archetype <- parameters$pathogen
  s$coverage_type <- parameters$far_uvc_joint_coverage_type
  s$coverage <- parameters$far_uvc_joint_coverage
  s$efficacy <- parameters$far_uvc_joint_efficacy
  s$disease_status <- parameters$endemic_or_epidemic


  # Create an output list:
  output <- list()

  # Append the simulation output and the parameter list to the output:
  output$parameters <- parameters
  output$simulation <- s

  # Return the outputs
  return(output)

}



# parameter_lists[[1]]$ID
# parameter_lists[[1]]$iteration
# parameter_lists[[1]]$pathogen
# parameter_lists[[1]]$far_uvc_joint_coverage_type
# parameter_lists[[1]]$far_uvc_joint_coverage
# parameter_lists[[1]]$far_uvc_joint_efficacy
# parameter_lists[[1]]$endemic_or_epidemic
