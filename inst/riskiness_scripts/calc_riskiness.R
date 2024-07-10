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

## do this for 0.1%, 0.5%, 1% and 2.5% infection prevalence
infection_prevalence <- c(0.0001, 0.0005, 0.001, 0.0025)
setting_sizes <- 100

schools_density <- 100 / 30 # number of m2 per classroom child
schools_range <- seq(0.1, 3, 0.1)
schools_matrix <- array(dim = c(length(schools_range), length(infection_prevalence), 4))
for (i in 1:length(schools_range)) {
  for (j in 1:length(infection_prevalence)) {

    ## Calculating the steady state concentration of virus
    I <- setting_sizes * infection_prevalence[j]
    room_height <- 3
    room_vol <- setting_sizes * schools_density * room_height
    pi <- 27
    A <- schools_range[i]
    kD <- 0.64
    alpha <- A + kD
    Css <- (I * pi) / (alpha * room_vol)

    ## Calculating Wells-Riley
    time <- seq(0, 8, 1)
    r <- 1.37e-2
    RRtv <- 0.45
    p_inf <- 1 - exp(-r * Css * RRtv * time)

    schools_matrix[i, j, 1] <- p_inf[which(time == 2)]
    schools_matrix[i, j, 2] <- p_inf[which(time == 4)]
    schools_matrix[i, j, 3] <- p_inf[which(time == 6)]
    schools_matrix[i, j, 4] <- p_inf[which(time == 8)]
  }
}

colours <- c("blue4", "red4", "green4", "orange4")
for (i in 1:30) {
  for (j in 1:4) {
    if (i == 1 & j == 1) {
      plot(c(2, 4, 6, 8), schools_matrix[i, j, ], type = "l", xlab = "time", ylab = "p_inf",
           ylim = c(1e-8, 5e-4), col = colours[j])
    } else {
      lines(c(2, 4, 6, 8), schools_matrix[i, j, ], type = "l", col  = colours[j])
    }
  }
}
par(mfrow = c(3, 4))
for (i in 1:4) {
  for (j in 1:30) {
    if (j == 1) {
      plot(c(2, 4, 6, 8), schools_matrix[j, i, ], type = "l", xlab = "time", ylab = "p_inf",
           ylim = c(1e-8, 5e-4), col = colours[i])
    } else {
      lines(c(2, 4, 6, 8), schools_matrix[j, i, ], type = "l", col  = colours[i])
    }
  }
}
for (i in 1:4) {
  for (j in 1:30) {
    if (j == 1) {
      plot(c(2, 4, 6, 8), schools_matrix[j, i, ], type = "l", xlab = "time", ylab = "p_inf",
           ylim = c(0, max(schools_matrix[, i, ])), col = colours[i])
    } else {
      lines(c(2, 4, 6, 8), schools_matrix[j, i, ], type = "l", col  = colours[i])
    }
  }
}
for (i in 1:4) {
  plot(c(2, 4, 6, 8), schools_matrix[1, i, ] / schools_matrix[30, i, ], type = "l", xlab = "time", ylab = "p_inf",
       ylim = c(0, 10), col = colours[i])
}

for (i in 1:30) {
  if (i == 1) {
    plot(c(2, 4, 6, 8), schools_matrix[i, 2, ], type = "l", xlab = "time", ylab = "p_inf",
         ylim = c(1e-7, 2e-5))
  } else {
    lines(c(2, 4, 6, 8), schools_matrix[i, 2, ], type = "l")
  }
}

# ventilation rate * setting size * infection prevalence * time spent
ratios <- apply(schools_matrix, c(2, 3), function(x) {
  ratio <- x[30] / x[1]
  return(ratio)
})
ratios <- apply(schools_matrix, c(2, 3), function(x) {
  ratio <- x[1] / x[30]
  return(ratio)
})



# school_min <- 0.1
# school_max <- 2.9
# school_mean <- 0.8

workplace_density <- 100 / 10 # number of m2 per office worker
workplace_range <- seq(0.2, 5, 0.1)
# workplace_lower <- 0.22
# workplace_upper <- 4.84
# workplace_median <- 0.98
# workplace_mean <- 2.00

leisure_density <- 100 /50
leisure_range <- seq(1, 10, 0.1)
# leisure_min <- 0.7
# leisure_max <- 8.5
# leisure_mean <- 3.8

