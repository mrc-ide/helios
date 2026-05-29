#Generating ACH values for locations in a setting

generate_setting_specific_ach <- function(parameters_list, setting, number_of_locations) {
  # If setting-specific ACH is switched off for this setting, return uniform
  # values. convert_ach_to_riskiness normalizes by the median, so uniform ACH
  # yields uniform riskiness == 1, matching the "no setting-specific riskiness"
  # behavior on main.
  # The caveat/downside is that this impacts the efficacy if the default
  # rooms have a low ACH.
  if (!isTRUE(parameters_list[[paste0("setting_specific_ach_", setting)]])) {
    return(rep(1, number_of_locations))
  }

  #retrieve mean and sd values from parameters.R or user input
  mu <- parameters_list[[paste0("setting_specific_ach_", setting, "_mean")]]
  sigma <- parameters_list[[paste0("setting_specific_ach_", setting, "_sd")]]

  #Draw values from truncated normal distribution
  ach_values <- truncnorm::rtruncnorm(n = number_of_locations, a = 0, b = Inf, mean = mu, sd = sigma)

  return(ach_values)
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

  # reference p_inf for normalization: anchor riskiness at the setting's
  # median ACH, so the typical location has riskiness ~ 1.
  ach_ref <- median(ach_values)
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

