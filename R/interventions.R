#' set_uvc
#'
#' @param parameters_list A list of parameters as generated using `get_parameters`
#' @param setting A character string describing the setting in which Far UVC is being deployed
#' @param coverage A numeric value describing the proportion of the settings in which Far UVC is deployed
#' @param coverage_style A character describing the type of coverage (random or targeted)
#' @param efficacy A numeric value describing the efficacy of the Far UVC deployed
#' @param timestep A numeric value describing the timestep in which Far UVC is deployed
#'
#' @export
set_uvc <- function(parameters_list, setting, coverage, coverage_type, efficacy, timestep) {

  # Ensure only a single setting passed to the function:
  if(length(setting) > 1) {
    stop("Error: Number of settings input greater than 1, parameterise for one setting at a time")
  }

  # Ensure only one coverage type is passed to the function:
  if(length(coverage_type) > 1) {
    stop("Error: Number of coverage types input greater than 1, parameterise for one coverage type at a time")
  }

  # Ensure input setting is from the allowed options
  if(setting != "workplace" & setting != "school" & setting != "leisure" & setting != "household") {
    stop("Error: Input setting invalid - far UVC only deployable in workplace, school, leisure, or household settings")
  }

  # Ensure the coverage type is from the allowed options:
  # Ensure input setting is from the allowed options
  if(coverage_type != "random" & coverage_type != "targeted") {
    stop("Error: Input setting invalid - far UvC only deployable in random or targeted coverage types")
  }

  # Check that coverage is between 0 and 1:
  if(coverage < 0 | coverage > 1) {
    stop("Error: coverage must take a value between 0 and 1")
  }

  # Check that efficacy is between 0 and 1:
  if(efficacy < 0 | efficacy > 1) {
    stop("Error: efficacy must take a value between 0 and 1")
  }

  # Switch on FAR UVC for the specified setting:
  parameters_list[[paste0("far_uvc_", setting)]] <- TRUE

  # Append the setting-specific Far-UVC coverage type:
  parameters_list[[paste0("far_uvc_", setting, "_coverage_type")]] <- coverage_type

  # Append the setting-specific Far-UVC coverage:
  parameters_list[[paste0("far_uvc_", setting, "_coverage")]] <- coverage

  # Append the setting-specific Far-UVC efficacy:
  parameters_list[[paste0("far_uvc_", setting, "_efficacy")]] <- efficacy

  # Append the setting-specific Far-UVC timestep:
  parameters_list[[paste0("far_uvc_", setting, "_timestep")]] <- timestep

  # Return the modified list of parameters
  parameters_list

}


