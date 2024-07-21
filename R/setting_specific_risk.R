#' set_setting_specific_riskiness
#'
#' @param parameter_list A list of parameters as generated using `get_parameters`
#' @param setting A character string describing the setting in which Far UVC is being deployed
#' @param mean The mean for the truncated log-normal distribution from which riskiness values will be drawn
#' @param sd The standard deviation for the truncated log-normal distribution from which riskiness values will be drawn
#' @param min The minimum value the truncated log-normal distribution from which riskiness values will be drawn
#' @param max The maximum value the truncated log-normal distribution from which riskiness values will be drawn
#'
#' @family setting-specific riskiness
#' @export
set_setting_specific_risk <- function(parameter_list, setting, mean, sd, min, max) {

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
  parameter_list[[paste0(setting, "_specific_riskiness_workplace_meanlog")]] <- mean
  parameter_list[[paste0(setting, "_specific_riskiness_workplace_sdlog")]] <- sd
  parameter_list[[paste0(setting, "_specific_riskiness_workplace_min")]] <- min
  parameter_list[[paste0(setting, "_specific_riskiness_workplace_max")]] <- max

  # Return the modified paramter list:
  return(parameter_list)

}




