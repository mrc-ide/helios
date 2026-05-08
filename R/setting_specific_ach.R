#Generating ACH values for locations in a setting

generate_setting_specific_ach <- function(parameters_list, setting, number_of_locations) {
  #TODO: establish a baseline value for ACH if not specified by user

  #retrieve mean and sd values from parameters.R
  mu <- parameters_list[[paste0("setting_specific_ach_", setting, "_mean")]]
  sigma <- parameters_list[[paste0("setting_specific_ach_", setting, "_sd")]]

  #Draw values from truncated normal distribution
  ach_values <- rtruncnorm( n = number_of_locations, a = 0, b = Inf, mean =mu, sd = sigma)

  #return number of locations
  return(ach_values[1:number_of_locations])
}

#Convert ACH values to riskiness using W-R
convert_ach_to_riskiness <- function(ach_values, parameters_list, setting) {
  # W-R parameters from parameters_list in parameters.R
  I <- 1
  pi <- parameters_list$wells_riley_emission_rate
  kD <- parameters_list$wells_riley_decay_rate
  r <- parameters_list$wells_riley_infection_prob_per_ffu
  RRtv <- parameters_list$wells_riley_respiratory_rate_factor
  t <- parameters_list$wells_riley_time_in_room

  volume_per_person <- parameters_list[[paste0("volume_per_person_", setting)]]

  # alpha values
  alpha_values <- ach_values + kD

  # steady-state concentration (per-person volume, density-based)
  Css_values <- (I * pi) / (alpha_values * volume_per_person)

  # p(infection)
  p_inf_values <- 1 - exp(-r * Css_values * RRtv * t)

  # reference p_inf for normalization
  ach_ref <- parameters_list$wells_riley_reference_ach
  if (is.null(ach_ref)) {
    ach_ref <- median(ach_values)
  }
  alpha_ref <- ach_ref + kD
  Css_ref <- (I * pi) / (alpha_ref * volume_per_person)
  p_inf_ref <- 1 - exp(-r * Css_ref * RRtv * t)

  # relative riskiness
  riskiness <- p_inf_values / p_inf_ref

  return(riskiness)
}

# Set ACH distribution for a setting type
set_setting_specific_ach <- function(parameters_list, setting, mean, sd) {
  parameters_list[[paste0("setting_specific_ach_", setting)]]        <- TRUE
  parameters_list[[paste0("setting_specific_ach_", setting, "_mean")]] <- mean
  parameters_list[[paste0("setting_specific_ach_", setting, "_sd")]]   <- sd

  return(parameters_list)
}

make_intervention <- function(name,
                              affected_by_baseline_ach = FALSE,
                              baseline_ach_function    = NULL,
                              baseline_ach_params      = list(),
                              variation                = FALSE,
                              variation_function       = NULL,
                              variation_params         = list(),
                              coverage                 = 1.0) {
  list(
    name                     = name,
