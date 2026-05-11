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

# # Old definitions: these have been moved to R/interventions.R. Kept here
# # commented out for reference. The live versions are in interventions.R.
# make_intervention <- function(name,
#                               affected_by_baseline_ach = FALSE,
#                               baseline_ach_function    = NULL,
#                               baseline_ach_params      = list(),
#                               variation                = FALSE,
#                               variation_function       = NULL,
#                               variation_params         = list(),
#                               coverage                 = 1.0) {
#   list(
#     name                     = name,
#     affected_by_baseline_ach = affected_by_baseline_ach,
#     baseline_ach_function    = baseline_ach_function,
#     baseline_ach_params      = baseline_ach_params,
#     variation                = variation,
#     variation_function       = variation_function,
#     variation_params         = variation_params,
#     coverage                 = coverage
#   )
# }
#
# #Store intervention for a setting in parameters_list
# set_intervention_ach <- function(parameters_list,
#                                  setting,
#                                  coverage_target,
#                                  coverage_type,
#                                  timestep,
#                                  ...) {
#   interventions <- list(...)
#
#   parameters_list[[paste0("intervention_", setting, "_active")]] <- TRUE
#   parameters_list[[paste0("intervention_", setting, "_list")]]  <- interventions
#   parameters_list[[paste0("intervention_", setting, "_coverage_target")]] <- coverage_target
#   parameters_list[[paste0("intervention_", setting, "_coverage_type")]] <- coverage_type
#   parameters_list[[paste0("intervention_", setting, "_timestep")]] <- timestep
#
#   return(parameters_list)
# }
#
#
# calculate_efficacy_from_ach <- function(ach_values, parameters_list, setting) {
#   I    <- 1
#   pi   <- parameters_list$wells_riley_emission_rate
#   kD   <- parameters_list$wells_riley_decay_rate
#   r    <- parameters_list$wells_riley_infection_prob_per_ffu
#   RRtv <- parameters_list$wells_riley_respiratory_rate_factor
#   t    <- parameters_list$wells_riley_time_in_room
#   V    <- parameters_list[[paste0("volume_per_person_", setting)]]
#
#   n           <- length(ach_values)
#   total_delta <- rep(0, n)
#
#   interventions <- parameters_list[[paste0("intervention_", setting, "_list")]]
#
#   if (is.null(interventions) || length(interventions) ==0 ) {
#     return(rep(0,n))
#   }
#   for (intervention in interventions) {
#
#     # call baseline_ach_function to get delta for each location
#     if (intervention$affected_by_baseline_ach) {
#       # pass baseline ACH as first argument, then params
#       delta_i <- mapply(
#         function(ach) do.call(intervention$baseline_ach_function,
#                               c(list(ach), intervention$baseline_ach_params)),
#         ach_values
#       )
#     } else {
#       # function only uses its own params — same delta replicated across locations
#       delta_i <- rep(
#         do.call(intervention$baseline_ach_function, intervention$baseline_ach_params),
#         n
#       )
#     }
#
#     # add location-to-location variation if requested
#     if (intervention$variation && !is.null(intervention$variation_function)) {
#       noise   <- do.call(intervention$variation_function,
#                          c(list(n), intervention$variation_params))
#       delta_i <- pmax(0, delta_i + noise)
#     }
#
#     total_delta <- total_delta + delta_i
#   }
#
#   alpha_pre  <- ach_values + kD
#   alpha_post <- ach_values + kD + total_delta
#
#   p_pre  <- 1 - exp(-r * (I * pi / (alpha_pre  * V)) * RRtv * t)
#   p_post <- 1 - exp(-r * (I * pi / (alpha_post * V)) * RRtv * t)
#
#   return(1 - p_post / p_pre)
# }


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
# uv_to_delta <- function(f, E_avg, k) {
#   f*E_avg *k *3.6
# }
#
# #Input: ACH, Output: Efficacy
# ach_to_efficacy <- function(baseline_ach,
#                             delta = 0,
#                             kD = 0.61,
#                             r = 0.0126,
#                             pi = 397,
#                             I = 1,
#                             RRtv = 1,
#                             t = 1,
#                             V = 50) {
#   A <- r * I * pi * RRtv * t/V
#   alpha_pre <- baseline_ach + kD
#   alpha_post <- baseline_ach + kD + delta
#   p_pre <- 1 - exp(-A/alpha_pre)
#   p_post <- 1 - exp(-A/alpha_post)
#   return(1- p_post/p_pre)
# }
#
# #efficacy to delta ACH
# efficacy_to_delta <- function(target_efficacy,
#                               baseline_ach,
#                               kD = 0.61,
#                               r = 0.0126,
#                               pi = 397,
#                               I = 1,
#                               RRtv = 1,
#                               t = 1,
#                               V = 50) {
#   A          <- r * I * pi * RRtv * t / V
#   alpha_pre  <- baseline_ach + kD
#   p_pre      <- 1 - exp(-A / alpha_pre)
#   alpha_post <- -A / log(1 - p_pre * (1 - target_efficacy))
#   return(alpha_post - alpha_pre)
# }

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

# #Getting AQI efficacy from ACH
# #calculate_efficacy_from_ach <- function(ach_values, parameters_list, setting) {
#   #determine which ach -> efficacy relationship is being used (need to make sure these are all defined for each setting)
#  # relationship_type <- parameters_list[[paste0("far_uvc_", setting, "_ach_efficacy_relationship")]]
#
# #   #use constant as default
# # if (is.null(relationship_type)) {
# #     relationship_type <- "constant"
# #   }
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
