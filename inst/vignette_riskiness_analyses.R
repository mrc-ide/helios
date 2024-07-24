# Loading required libraries
library(EnvStats); library(reshape2); library(tidyverse)

#### Parameters
pi <- 27       # rate of infective virus emission from infected individuals
kD <- 0.64     # natural decay constant for SARS-CoV-2
r <- 1.37e-2   # the probability that a single inhaled infective virus will initiate an infection
RRtv <- 0.45   # volume breathing rate - assumes 15 breaths per minute and 500ml tidal volume = 0.45m^3 per hour
infection_prevalence <- c(0.0001, 0.001, 0.01, 0.025) # prevalence of infection
time_in_room <- seq(0, 8, 1) # time spent in the room
room_height <- 2.5 # height of the room
setting_occupancy <- 100 # number of people in the room (note this cancels out via I and room_vol in equations below
                         # so not actually required); only put here for completeness

#### Schools
school_m2_person <- 3.33         # 30 people per 100m^2 density (from ANSI/ASHRAE Table 6.1)
school_ACPH_range <- c(0.1, 2.9) # range of school ACPHs considered (from Corsi et al, 2006)
school_matrix <- array(dim = c(length(school_ACPH_range),
                               length(infection_prevalence),
                               length(time_in_room)))
dimnames(school_matrix) <- list(airchanges_per_hour = school_ACPH_range,
                                infection_prevalence = paste0(100 * infection_prevalence, "% Prev."),
                                time = paste0(time_in_room))
for (i in 1:length(school_ACPH_range)) {
  for (j in 1:length(infection_prevalence)) {
    ## Calculating the steady state concentration of virus
    I <- setting_occupancy * infection_prevalence[j]
    room_vol <- setting_occupancy * school_m2_person * room_height
    A <- school_ACPH_range[i]
    alpha <- A + kD
    Css <- (I * pi) / (alpha * room_vol)

    ## Calculating Wells-Riley
    p_inf <- 1 - exp(-r * Css * RRtv * time_in_room)

    ## Storing the output
    school_matrix[i, j, ] <- p_inf
  }
}
school_melt_df <- melt(school_matrix, varnames = c("airchanges_per_hour", "infection_prevalence", "time"), value.name = "prob_inf")
school_melt_df$setting_type <- "School"

#### Workplace
workplace_m2_person <- 10             # 10 people per 100m^2 density (from ANSI/ASHRAE Table 6.1)
workplace_ACPH_range <- c(0.22, 4.84) # range of workplace ACPHs considered (from Corsi et al, 2006)
workplace_matrix <- array(dim = c(length(workplace_ACPH_range),
                                  length(infection_prevalence),
                                  length(time_in_room)))
dimnames(workplace_matrix) <- list(airchanges_per_hour = workplace_ACPH_range,
                                   infection_prevalence = paste0(100 * infection_prevalence, "% Prev."),
                                   time = paste0(time_in_room))
for (i in 1:length(workplace_ACPH_range)) {
  for (j in 1:length(infection_prevalence)) {
    ## Calculating the steady state concentration of virus
    I <- setting_occupancy * infection_prevalence[j]
    room_vol <- setting_occupancy * workplace_m2_person * room_height
    A <- workplace_ACPH_range[i]
    alpha <- A + kD
    Css <- (I * pi) / (alpha * room_vol)

    ## Calculating Wells-Riley
    p_inf <- 1 - exp(-r * Css * RRtv * time_in_room)

    ## Storing the output
    workplace_matrix[i, j, ] <- p_inf
  }
}
workplace_melt_df <- melt(workplace_matrix, varnames = c("airchanges_per_hour", "infection_prevalence", "time"), value.name = "prob_inf")
workplace_melt_df$setting_type <- "Workplace"

#### Leisure Settings
leisure_m2_person <- 2             # 50 people per 100m^2 density (from ANSI/ASHRAE Table 6.1)
leisure_ACPH_range <- c(0.22, 4.84) # range of leisure ACPHs considered (assumed same as workplace)
leisure_matrix <- array(dim = c(length(leisure_ACPH_range),
                                  length(infection_prevalence),
                                  length(time_in_room)))
dimnames(leisure_matrix) <- list(airchanges_per_hour = leisure_ACPH_range,
                                   infection_prevalence = paste0(100 * infection_prevalence, "% Prev."),
                                   time = paste0(time_in_room))
for (i in 1:length(leisure_ACPH_range)) {
  for (j in 1:length(infection_prevalence)) {
    ## Calculating the steady state concentration of virus
    I <- setting_occupancy * infection_prevalence[j]
    room_vol <- setting_occupancy * leisure_m2_person * room_height
    A <- leisure_ACPH_range[i]
    alpha <- A + kD
    Css <- (I * pi) / (alpha * room_vol)

    ## Calculating Wells-Riley
    p_inf <- 1 - exp(-r * Css * RRtv * time_in_room)

    ## Storing the output
    leisure_matrix[i, j, ] <- p_inf
  }
}
leisure_melt_df <- melt(leisure_matrix, varnames = c("airchanges_per_hour", "infection_prevalence", "time"), value.name = "prob_inf")
leisure_melt_df$setting_type <- "Leisure"

#### Household Settings
leisure_m2_person <- 2             # 50 people per 100m^2 density (from ANSI/ASHRAE Table 6.1)
leisure_ACPH_range <- c(0.22, 4.84) # range of leisure ACPHs considered (assumed same as workplace)
leisure_matrix <- array(dim = c(length(leisure_ACPH_range),
                                length(infection_prevalence),
                                length(time_in_room)))
dimnames(leisure_matrix) <- list(airchanges_per_hour = leisure_ACPH_range,
                                 infection_prevalence = paste0(100 * infection_prevalence, "% Prev."),
                                 time = paste0(time_in_room))
for (i in 1:length(leisure_ACPH_range)) {
  for (j in 1:length(infection_prevalence)) {
    ## Calculating the steady state concentration of virus
    I <- setting_occupancy * infection_prevalence[j]
    room_vol <- setting_occupancy * leisure_m2_person * room_height
    A <- leisure_ACPH_range[i]
    alpha <- A + kD
    Css <- (I * pi) / (alpha * room_vol)

    ## Calculating Wells-Riley
    p_inf <- 1 - exp(-r * Css * RRtv * time_in_room)

    ## Storing the output
    leisure_matrix[i, j, ] <- p_inf
  }
}
leisure_melt_df <- melt(leisure_matrix, varnames = c("airchanges_per_hour", "infection_prevalence", "time"), value.name = "prob_inf")
leisure_melt_df$setting_type <- "Leisure"



ggplot(school_melt_df, aes(x = as.numeric(time), y = prob_inf, col = factor(airchanges_per_hour))) +
  geom_line() +
  geom_point() +
  facet_grid(infection_prevalence~.,
             scales = "free_y") +
  theme_bw() +
  labs(x = "Time (Hours)", y = "Prob. Infected (Wells Riley)",
       col = "Air Changes\nPer Hour")

school_melt_df2 <- school_melt_df %>%
  filter(time != 0) %>%
  group_by(time, infection_prevalence, setting_type) %>%
  summarise(ratio = prob_inf[airchanges_per_hour == min(airchanges_per_hour)]/
                    prob_inf[airchanges_per_hour == max(airchanges_per_hour)])

ggplot(school_melt_df2, aes(x = as.numeric(time), y = ratio)) +
  geom_line() +
  geom_point() +
  facet_grid(infection_prevalence~.) +
  lims(y = c(0, 5)) +
  theme_bw() +
  labs(x = "Time (Hours)", y = "Prob. Infected (Wells Riley)",
       col = "Air Changes\nPer Hour")
