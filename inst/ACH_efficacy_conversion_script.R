# Loading required libraries
library(EnvStats)
library(reshape2)
library(tidyverse)

### Sensitivity Analysis examining how variation in air-changes per hour
### influences riskiness of a room. We vary:
### - infection prevalence:  0.1%, 0.5%, 1% and 2.5% infection prevalence
### - air changes per hour:  ranging between 0.1 and 10 air changes per hour
### - occupant density:      ranging between 2m^2 to 10m^2 per person
infection_prevalence <- c(0.0001, 0.001, 0.01)
air_changes_per_hour <- seq(1, 25, 1)
m2_per_person <- seq(2, 8, 2)
setting_sizes <- 100
room_height <- 2.5
output_matrix <- array(
  dim = c(length(air_changes_per_hour), length(infection_prevalence), length(m2_per_person), 4)
)
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
        plot(
          c(2, 4, 6, 8),
          output_matrix[k, i, j, ],
          type = "l",
          xlab = "Time (hours)",
          ylab = "Prob. infected",
          ylim = c(0, max(output_matrix[, i, j, ])),
          col = colours[i],
          main = paste0(
            "Prev=",
            round(100 * infection_prevalence[i], 2),
            "%,",
            "Dens=",
            m2_per_person[j]
          )
        )
      } else {
        lines(c(2, 4, 6, 8), output_matrix[k, i, j, ], type = "l", col = colours[i])
      }
    }
  }
}


par(mfrow = c(length(infection_prevalence), length(m2_per_person)))
for (i in 1:length(infection_prevalence)) {
  for (j in 1:length(m2_per_person)) {
    plot(
      c(2, 4, 6, 8),
      output_matrix[1, i, j, ] / output_matrix[length(air_changes_per_hour), i, j, ],
      type = "l",
      xlab = "time",
      ylab = "p_inf",
      ylim = c(0, 15),
      col = colours[i],
      main = paste0(infection_prevalence[i], " ", m2_per_person[j])
    )
  }
}

dimnames(output_matrix) <- list(
  airchanges_per_hour = air_changes_per_hour,
  infection_prevalence = paste0(100 * infection_prevalence, "% Prev."),
  m2_per_person = paste0(m2_per_person, "m^2 per person"),
  time = paste0(c(2, 4, 6, 8))
)


melted_df <- melt(
  output_matrix,
  varnames = c("airchanges_per_hour", "infection_prevalence", "m2_per_person", "time"),
  value.name = "value"
)

a <- ggplot(
  subset(melted_df, infection_prevalence == "0.01% Prev." & m2_per_person == "2m^2 per person"),
  aes(x = as.numeric(time), y = value, col = factor(airchanges_per_hour))
) +
  geom_line() +
  geom_point() +
  facet_grid(infection_prevalence ~ m2_per_person, scales = "free_y") +
  theme_bw() +
  labs(x = "Time (Hours)", y = "Prob. Infected (Wells Riley)", col = "Air Changes\nPer Hour")

colnames(melted_df)
melted_df2 <- melted_df %>%
  filter(time == 8) %>% # time == 4 |
  group_by(infection_prevalence, m2_per_person, time)

b <- ggplot(
  subset(melted_df2, infection_prevalence == "0.01% Prev." & m2_per_person == "2m^2 per person"),
  aes(x = as.numeric(airchanges_per_hour), y = value, col = factor(time))
) +
  geom_line() +
  geom_point() +
  facet_grid(infection_prevalence ~ m2_per_person, scales = "free_y") +
  theme_bw() +
  labs(x = "ACH", y = "Prob. Infected (Wells Riley)", col = "Time Spent In Room") +
  theme(legend.position = "none")

melted_df3 <- melted_df2 %>%
  group_by(infection_prevalence, m2_per_person, time) %>%
  mutate(efficacy = 1 - (value / value[airchanges_per_hour == 1])) %>%
  mutate(efficacy2 = 1 - (value / value[airchanges_per_hour == 2]))

c <- ggplot(
  subset(melted_df3, infection_prevalence == "0.01% Prev." & m2_per_person == "2m^2 per person"),
  aes(x = as.numeric(airchanges_per_hour), y = efficacy, col = factor(time))
) +
  geom_line() +
  geom_point() +
  geom_line(aes(x = as.numeric(airchanges_per_hour), y = 0.3787860 + efficacy2), col = "blue") +
  facet_grid(infection_prevalence ~ m2_per_person, scales = "free_y") +
  theme_bw() +
  labs(x = "ACH", y = "Efficacy", col = "Time Spent In Room") +
  theme(legend.position = "none")

cowplot::plot_grid(b, c, nrow = 1)

starters <- 1:10
step_max <- 15
pair_grid <- expand_grid(ach_start = starters, step = 1:step_max) %>%
  mutate(ach_target = ach_start + step) %>%
  select(-step)

efficacy_df <- melted_df2 %>%
  # filter(infection_prevalence == "0.01% Prev." & m2_per_person == "2m^2 per person") %>%
  rename(ach_target = airchanges_per_hour) %>%
  right_join(pair_grid, by = "ach_target") %>%
  left_join(
    melted_df2 %>%
      rename(ach_start = airchanges_per_hour, value_start = value),
    by = c("infection_prevalence", "m2_per_person", "time", "ach_start")
  ) %>%
  group_by(infection_prevalence, m2_per_person) %>%
  mutate(
    efficacy = 1 - value / value_start # the definition you gave
  ) %>%
  arrange(infection_prevalence, m2_per_person, time, ach_start, ach_target) %>%
  filter(time == 8) %>%
  mutate(ach_increase = ach_target - ach_start)

# ggplot(subset(efficacy_df, infection_prevalence == "0.01% Prev." & m2_per_person == "2m^2 per person"),
ggplot(efficacy_df, aes(x = as.numeric(ach_increase), y = efficacy, col = factor(ach_start))) +
  facet_grid(infection_prevalence ~ m2_per_person, scales = "free_y") +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(x = "ACH Increase", y = "Efficacy", col = "Baseline ACH") +
  lims(y = c(0, 1), x = c(0, 15))

ggplot(efficacy_df, aes(x = as.numeric(ach_increase), y = efficacy, col = infection_prevalence)) +
  facet_grid(factor(ach_start) ~ m2_per_person, scales = "free_y") +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(x = "ACH Increase", y = "Efficacy", col = "Baseline ACH") +
  lims(y = c(0, 1), x = c(0, 15))

ggplot(efficacy_df, aes(x = as.numeric(ach_increase), y = efficacy, col = m2_per_person)) +
  facet_grid(factor(ach_start) ~ infection_prevalence, scales = "free_y") +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(x = "ACH Increase", y = "Efficacy", col = "Baseline ACH") +
  lims(y = c(0, 1), x = c(0, 15))
