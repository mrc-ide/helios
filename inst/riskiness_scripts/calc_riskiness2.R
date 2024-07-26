# Loading required libraries
library(EnvStats)

# Calculating steady state without farUVC in place and using pi of viruses per hour per person
### 30 room occupants, 10% infected, per Blatchley presentation
I <- 3               ## number of infectious people in room
pi <- 10^5           ## per capita virus emission rate
room_vol <- 500
A <- 0.59            ## air changes per hour
kD <- 0.41           ## natural decay constant
alpha <- A + kD
Css <- (I * pi) / (alpha * room_vol)
Css
### this successfully replicates Blatchley slide 8/24

### They then define
# "r" for FFU is 1.26*10-2 i.e. probability that a single FFU will cause infection
# so then question becomes what the steady state of FFUs are - quotes FFU per hour (i.e. pi) of 29
I <- 1               ## number of infectious people in room
pi <- 27             ## per capita virus emission rate
room_vol <- 500
A <- 0.3             ## air changes per hour
kD <- 0.64           ## natural decay constant
alpha <- A + kD
Css <- (I * pi) / (alpha * room_vol)
Css

## Wells-Riley
time <- seq(0, 8, 0.1)
r <- 1.37e-2
RRtv <- 0.45
p_inf <- 1 - exp(-r * Css * RRtv * time)
plot(time, log10(p_inf), ylim = c(-5, 0))


### Sensitivity Analysis examining how variation in air-changes per hour
### influences riskiness of a room. We vary:
### - infection prevalence:  0.1%, 0.5%, 1% and 2.5% infection prevalence
### - air changes per hour:  ranging between 0.1 and 10 air changes per hour
### - occupant density:      ranging between 2m^2 to 10m^2 per person
infection_prevalence <- c(0.0001, 0.001, 0.01)
air_changes_per_hour <- seq(1, 5, 1)
m2_per_person <- seq(2, 8, 2)
setting_sizes <- 100
room_height <- 2.5
output_matrix <- array(dim = c(length(air_changes_per_hour),
                               length(infection_prevalence),
                               length(m2_per_person),
                               4))
for (i in 1:length(air_changes_per_hour)) {
  for (j in 1:length(infection_prevalence)) {
    for (k in 1:length(m2_per_person)) {

      ## Calculating the steady state concentration of virus
      I <- setting_sizes * infection_prevalence[j]
      room_vol <- setting_sizes * m2_per_person[k] * room_height
      pi <- 27
      A <- air_changes_per_hour[i]
      kD <- 0.64
      alpha <- A + kD
      Css <- (I * pi) / (alpha * room_vol)

      ## Calculating Wells-Riley
      time <- seq(0, 8, 1)
      r <- 1.37e-2
      RRtv <- 0.45
      p_inf <- 1 - exp(-r * Css * RRtv * time)

      output_matrix[i, j, k, 1] <- p_inf[which(time == 2)]
      output_matrix[i, j, k, 2] <- p_inf[which(time == 4)]
      output_matrix[i, j, k, 3] <- p_inf[which(time == 6)]
      output_matrix[i, j, k, 4] <- p_inf[which(time == 8)]
    }
  }
}

colours <- c("blue4", "red4", "green4")
par(mfrow = c(length(infection_prevalence), length(m2_per_person)))
for (i in 1:length(infection_prevalence)) {
  for (j in 1:length(m2_per_person)) {
    for (k in 1:length(air_changes_per_hour)) {
      if (k == 1) {
        plot(c(2, 4, 6, 8), output_matrix[k, i, j, ], type = "l",
             xlab = "Time (hours)", ylab = "Prob. infected",
             ylim = c(0, max(output_matrix[, i, j, ])), col = colours[i],
             main = paste0("Prev=", round(100 * infection_prevalence[i], 2), "%,",
                           "Dens=", m2_per_person[j]))
      } else {
        lines(c(2, 4, 6, 8), output_matrix[k, i, j, ], type = "l", col  = colours[i])
      }
    }
  }
}


par(mfrow = c(length(infection_prevalence), length(m2_per_person)))
for (i in 1:length(infection_prevalence)) {
  for (j in 1:length(m2_per_person)) {
      plot(c(2, 4, 6, 8),
           output_matrix[1, i, j, ] / output_matrix[length(air_changes_per_hour), i, j, ],
           type = "l", xlab = "time", ylab = "p_inf",
           ylim = c(0, 15),
           col = colours[i],
           main = paste0(infection_prevalence[i], " ", m2_per_person[j]))
  }
}

dimnames(output_matrix) <- list(
  airchanges_per_hour = air_changes_per_hour,
  infection_prevalence = paste0(100 * infection_prevalence, "% Prev."),
  m2_per_person = paste0(m2_per_person, "m^2 per person"),
  time = paste0(c(2, 4, 6, 8))
)

library(reshape2)
library(tidyverse)
melted_df <- melt(output_matrix,
                  varnames = c("airchanges_per_hour", "infection_prevalence", "m2_per_person", "time"),
                  value.name = "value")

ggplot(melted_df, aes(x = as.numeric(time), y = value, col = factor(airchanges_per_hour))) +
  geom_line() +
  geom_point() +
  facet_grid(infection_prevalence~m2_per_person,
             scales = "free_y") +
  theme_bw() +
  labs(x = "Time (Hours)", y = "Prob. Infected (Wells Riley)",
       col = "Air Changes\nPer Hour")

colnames(melted_df)
melted_df2 <- melted_df %>%
  filter(time == 8) %>%
  group_by(infection_prevalence, m2_per_person) %>%
  summarise(ratio = value[airchanges_per_hour == min(airchanges_per_hour)]/
              value[airchanges_per_hour == max(airchanges_per_hour)])
melted_df2

# ventilation rate * setting size * infection prevalence * time spent
output_matrix <- array(dim = c(,
                               length(infection_prevalence),
                               length(m2_per_person),
                               4))

ratios <- apply(output_matrix[, , , 4], c(2, 3), function(x) {
  ratio <- x[which(air_changes_per_hour == 1.0)] / x
  return(ratio)
})

par(mfrow = c(1, 1))
plot(air_changes_per_hour, ratios[, 1, 1], pch = 20,
     ylab = "Riskiness Relative to 1 Air Change Per Hour",
     xlab = "Air Changes Per Hour")
