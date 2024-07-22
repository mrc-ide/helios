#' set_setting_specific_riskiness
#'
#' @param parameters_list A list of parameters as generated using `get_parameters`
#' @param setting A character string describing the setting in which Far UVC is being deployed
#' @param mean The mean for the truncated log-normal distribution from which riskiness values will be drawn
#' @param sd The standard deviation for the truncated log-normal distribution from which riskiness values will be drawn
#' @param min The minimum value the truncated log-normal distribution from which riskiness values will be drawn
#' @param max The maximum value the truncated log-normal distribution from which riskiness values will be drawn
#'
#' @family setting-specific riskiness
#' @export
set_setting_specific_riskiness <- function(parameters_list, setting, mean, sd, min, max) {

  # Ensure only a single setting passed to the function:
  if(length(setting) > 1) {
    stop("Error: Number of settings input greater than 1, parameterise for one setting at a time")
  }

  # Check that the setting supplied is from the accepted list:
  if(setting != "workplace" & setting != "school" & setting != "leisure" & setting != "household") {
    stop("Error: Input setting invalid - far UVC only deployable in workplace, school, leisure, or household settings")
  }

  # Switch on setting-specific riskiness for the specified setting:
  parameter_list[[paste0(setting, "_specific_riskiness_workplace")]] <- TRUE

  # Overwrite the log-normal distribution parameters for the specified setting:
  parameters_list[[paste0(setting, "_specific_riskiness_workplace_meanlog")]] <- mean
  parameters_list[[paste0(setting, "_specific_riskiness_workplace_sdlog")]] <- sd
  parameters_list[[paste0(setting, "_specific_riskiness_workplace_min")]] <- min
  parameters_list[[paste0(setting, "_specific_riskiness_workplace_max")]] <- max

  # Return the modified paramter list:
  return(parameters_list)

}

#' generate_setting_specific_riskinesses
#'
#' @param parameters_list A list of parameters as generated using `get_parameters`
#' @param setting A character string describing the setting in which Far UVC is being deployed
#' @param number_of_locations The number of locations for the setting-type for which riskiness values are to be drawn
#'
#' @family setting-specific riskiness
#' @export
generate_setting_specific_riskinesses <- function(parameters_list, setting, number_of_locations) {

  # Ensure only a single setting passed to the function:
  if(length(setting) > 1) {
    stop("Error: Number of settings input greater than 1, parameterise for one setting at a time")
  }

  # Check that the setting supplied is from the accepted list:
  if(setting != "workplace" & setting != "school" & setting != "leisure" & setting != "household") {
    stop("Error: Input setting invalid - far UVC only deployable in workplace, school, leisure, or household settings")
  }

  # If setting-specific riskiness is switched on for the specified setting type, calculate setting-specific
  # riskiness values for each location within the setting using the rlnormTrunc() function:
  if(parameters_list[[paste0("setting_specific_riskiness_", setting)]]) {
    setting_specific_riskiness <- EnvStats::rlnormTrunc(n = number_of_locations,
                                                        meanlog = parameters_list[[paste0("setting_specific_riskiness_", setting, "_meanlog")]],
                                                        sdlog = parameters_list[[paste0("setting_specific_riskiness_", setting, "sdlog")]],
                                                        min = parameters_list[[paste0("setting_specific_riskiness_", setting, "_min")]],
                                                        max = parameters_list[[paste0("setting_specific_riskiness_", setting, "_max")]])
  } else {

    # If setting-specific riskiness is switched off for the specified setting type, assign each location a 1:
    setting_specific_riskiness <- rep(1, number_of_locations)
  }

  # Return the vector of setting-specific riskiness values:
  return(setting_specific_riskiness)
}


