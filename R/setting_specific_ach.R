#Generating ACH values for locations in a setting

generate_setting_specific_ach <- function(parameters_list, setting, number_of_locations) {
  #TODO: establish a baseline value for ACH if not specified by user

  #retrieve mean and sd values from parameters.R
  mu <- parameters_list[[paste0("setting_specific_ach_", setting, "_mean")]]
  sigma <- parameters_list[[paste0("setting_specific_ach_", setting, "_sd")]]

  #Draw values from truncated normal distribution
  ach_values <- rtruncnorm( n = number_of_locations, a = 0, b = Inf, mean =mu, sd = sigma)


  #return num of locaions
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



#Getting AQI efficacy from ACH
calculate_efficacy_from_ach <- function(ach_values, parameters_list, setting) {
  #determine which ach -> efficacy relationship is being used (need to make sure these are all defined for each setting)
  relationship_type <- parameters_list[[paste0("far_uvc_", setting, "_ach_efficacy_relationship")]]

  #use constant as default
  if (is.null(relationship_type)) {
    relationship_type <- "constant"
  }

  #functions for constant
  if (relationship_type == "constant") {
    efficacy <- parameters_list[[paste0("far_uvc_", setting, "_max_efficacy")]]
    #same efficacy for each location in a setting
    efficacy_values <- rep(efficacy, length(ach_values))
    #efficacy = max_eff/(1+ exp(-k(x - x0))),
    #need to define all of these in the parameter list
  } else if (relationship_type == "sigmoid") {
    max_eff <- parameters_list[[paste0("far_uvc_", setting, "_max_efficacy")]]
    k <- parameters_list[[paste0("far_uvc_", setting, "_sigmoid_k")]]
    x0 <- parameters_list[[paste0("far_uvc_", setting, "_sigmoid_x0")]]
    efficacy_values <- max_eff / (1 + exp(-k * (ach_values - x0)))
  }

  return(efficacy_values)
}

set_intervention_ach <- function(parameters_list,
                                 setting,
                                 coverage,
                                 coverage_target,
                                 coverage_type,
                                 timestep,
                                 delta_ach = 0,
                                 delta_uv = 0) {
  parameters_list[[paste0("intervention_", setting, "_active")]] <- TRUE
  parameters_list[[paste0("intervention_", setting, "_coverage")]] <- coverage
  parameters_list[[paste0("intervention_", setting, "_coverage_target")]] <- coverage_target
  parameters_list[[paste0("intervention_", setting, "_coverage_type")]] <- coverage_type
  parameters_list[[paste0("intervention_", setting, "_timestep")]] <- timestep
  parameters_list[[paste0("intervention_", setting, "_delta_ach")]] <- delta_ach
  parameters_list[[paste0("intervention_", setting, "_delta_uv")]] <- delta_uv

  return(parameters_list)
}
# set_uvc_ach <- function (parameters_list,
#                          setting,
#                          coverage,
#                          coverage_target,
#                          coverage_type,
#                          timestep,
#                          relationship_type,
#                          max_efficacy,
#                          sigmoid_k,
#                          sigmoid_x0) {
#
#   parameters_list[[paste0("far_uvc_", setting)]] <- TRUE
#   parameters_list[[paste0("far_uvc_", setting, "_coverage")]] <- coverage
#   parameters_list[[paste0("far_uvc_", setting, "_coverage_target")]] <- coverage_target
#   parameters_list[[paste0("far_uvc_", setting, "_coverage_type")]] <- coverage_type
#   parameters_list[[paste0("far_uvc_", setting, "_timestep")]] <- timestep
#
#   parameters_list[[paste0("far_uvc_", setting, "_ach_efficacy_relationship")]] <- relationship_type
#   parameters_list[[paste0("far_uvc_", setting, "_max_efficacy")]] <- max_efficacy
#   parameters_list[[paste0("far_uvc_", setting, "_sigmoid_k")]] <- sigmoid_k
#   parameters_list[[paste0("far_uvc_", setting, "_sigmoid_x0")]] <- sigmoid_x0
#
#   return(parameters_list)
#
#
# }

#Set ACH distribution for a setting type
#need to add validation
set_setting_specific_ach <- function(parameters_list, setting, mean, sd) {
  parameters_list[[paste0("setting_specific_ach_", setting)]] <- TRUE
  parameters_list[[paste0("setting_specific_ach_", setting, "_mean")]] <- mean
  parameters_list[[paste0("setting_specific_ach_", setting, "_sd")]] <- sd

  return(parameters_list)
}

calculate_efficacy_from_ach <- function(ach_values, parameters_list, setting) {
  I      <- 1
  pi     <- parameters_list$wells_riley_emission_rate
  kD     <- parameters_list$wells_riley_decay_rate
  r      <- parameters_list$wells_riley_infection_prob_per_ffu
  RRtv   <- parameters_list$wells_riley_respiratory_rate_factor
  t      <- parameters_list$wells_riley_time_in_room
  V      <- parameters_list[[paste0("volume_per_person_", setting)]]

  delta_ach       <- parameters_list[[paste0("intervention_", setting, "_delta_ach")]]
  delta_uv <- parameters_list[[paste0("intervention_", setting, "_delta_uv")]]
  if (is.null(delta_ach))       delta_ach <- 0
  if (is.null(delta_uv)) delta_uv <- 0

  alpha_pre  <- ach_values + kD
  alpha_post <- (ach_values + delta_ach) + (kD + delta_uv)

  p_pre  <- 1 - exp(-r * (I * pi / (alpha_pre  * V)) * RRtv * t)
  p_post <- 1 - exp(-r * (I * pi / (alpha_post * V)) * RRtv * t)

  efficacy_values <- 1 - p_post / p_pre

  return(efficacy_values)
}
# calculate_efficacy_from_ach <- function(ach_values, parameters_list, setting) {
#   #determine which ach -> efficacy relationship is being used (need to make sure these are all defined for each setting)
#   relationship_type <- parameters_list[[paste0("far_uvc_", setting, "_ach_efficacy_relationship")]]
#
#   #use constant as default
#   if (is.null(relationship_type)) {
#     relationship_type <- "constant"
#   }
#
#   #functions for constant
#   if (relationship_type == "constant") {
#     efficacy <- parameters_list[[paste0("far_uvc_", setting, "_max_efficacy")]]
#     #same efficacy for each location in a setting
#     efficacy_values <- rep(efficacy, length(ach_values))
#     #efficacy = max_eff/(1+ exp(-k(x - x0))),
#     #need to define all of these in the parameter list
#   } else if (relationship_type == "sigmoid") {
#     max_eff <- parameters_list[[paste0("far_uvc_", setting, "_max_efficacy")]]
#     k <- parameters_list[[paste0("far_uvc_", setting, "_sigmoid_k")]]
#     x0 <- parameters_list[[paste0("far_uvc_", setting, "_sigmoid_x0")]]
#     efficacy_values <- max_eff / (1 + exp(-k * (ach_values - x0)))
#   }
#
#   return(efficacy_values)
# }

#### Helper Functions
#Converting UVC to delta_uv ACH equivalent
#inputs f: frac of room irradiated, E_acg: avg fluence rate, k = UV inactivation constant
uv_to_delta <- function(f, E_avg, k) {
  f*E_avg_k*3.6
}

#Input: ACH, Output: Efficacy
ach_to_efficacy <- function(baseline_ach,
                            delta_ach = 0,
                            delta_uv = 0,
                            kD = 0.61,
                            r = 0.0126,
                            pi = 397,
                            I = 1,
                            RRtv = 1,
                            t = 1,
                            V = 50) {
  A <- r * I * pi * RRtv * t/V
  alpha_pre <- baseline_ach + kD
  alpha_post <- baseline_ach + delta_ach + kD + delta_uv
  p_pre <- 1 - exp(-A/alpha_pre)
  p_post <- 1 - exp(-A/alpha_post)
  return(1- p_post/p_pre)
}

#efficacy to delta ACH
efficacy_to_delta <- function(target_efficacy,
                              baseline_ach,
                              kD = 0.61,
                              r = 0.0126,
                              pi = 397,
                              I = 1,
                              RRtv = 1,
                              t = 1,
                              V = 50) {
  A          <- r * I * pi_q * RRtv * t / V
  alpha_pre  <- baseline_ach + kD
  p_pre      <- 1 - exp(-A / alpha_pre)
  alpha_post <- -A / log(1 - p_pre * (1 - desired_efficacy))
  return(alpha_post - alpha_pre)
}


