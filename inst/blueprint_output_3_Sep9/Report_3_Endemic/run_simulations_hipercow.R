

# Specify the run_scenrio() function:
run_simulation_hipercow <- function(parameters) {

  # Load in the malariasimulation package:
  library(helios)

  # Run the simulation:
  s <- helios::run_simulation(parameters_list = parameters)

  # Append the simulation identifier to the simulation outputs:
  s$ID <- parameters$ID

  # Create an output list:
  output <- list()

  # Append the simulation output and the parameter list to the output:
  output$parameters <- parameters
  output$simulation <- s

  # Return the outputs
  return(output)

}



