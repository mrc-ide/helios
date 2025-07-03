run_simulation_hipercow <- function(parameters, file_save = FALSE) {

  # Load in the helios package:
  #library(helios)

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

  # Save the output as a .rds if file_save switched on:
  if(file_save) {
    saveRDS(object = output,
            file = paste0("./inst/hipercow_demo/Simulation_",
                          parameters$ID,
                          "_",
                          parameters$endemic_or_epidemic,
                          "_",
                          parameters$far_uvc_joint_coverage_type,
                          "_",
                          parameters$iteration,
                          "_output.rds"
            )
    )
  }

  # Return the outputs
  return(output)

}
