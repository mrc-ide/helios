## Exploring ACH (Normal) → p_inf → riskiness via Wells–Riley

set.seed(123)

# 1. Generate NORMAL distribution of ACH values
mu_ach <- 4.8
sd_ach <- 4.8
n_samples <- 3000

ACH_raw <- rnorm(n_samples, mean = mu_ach, sd = sd_ach)

# Remove non-physical values
ACH_samples <- ACH_raw[ACH_raw > 0]

hist(ACH_samples,
     breaks = 500,
     col = "skyblue",
     main = "ACH values (Normal, truncated at 0)",
     xlab = "ACH")

# 2. Wells–Riley parameters
I <- 1                  # number of infectious people
pi <- 27                # emission rate (FFU/hour)
room_vol <- 150         # m^3
kD <- 0.64              # natural decay rate

alpha_samples <- ACH_samples + kD

# Steady-state concentration
Css_samples <- (I * pi) / (alpha_samples * room_vol)

# 3. Infection probability
r <- 1.37e-2            # infection probability per FFU
RRtv <- 0.45
t <- 4                  # hours in room

p_inf_samples <- 1 - exp(-r * Css_samples * RRtv * t)

# Summary
summary(p_inf_samples)

# 4. Plot distribution of infection probability
hist(p_inf_samples,
     breaks = 500,
     col = "grey",
     main = "Distribution of p_inf (Normal ACH)",
     xlab = "p_inf (Wells–Riley)")

# -------------------------------
# 5. Define reference ACH (median)
# -------------------------------

ACH_ref <- median(ACH_samples)

alpha_ref <- ACH_ref + kD
Css_ref <- (I * pi) / (alpha_ref * room_vol)

p_inf_ref <- 1 - exp(-r * Css_ref * RRtv * t)

# -------------------------------
# 6. Riskiness (relative risk)
# -------------------------------

riskiness <- p_inf_samples / p_inf_ref

summary(riskiness)

# Plot riskiness
hist(riskiness,
     breaks = 500,
     col = "darkgreen",
     main = "riskiness (Normal ACH)",
     xlab = "riskiness (p_inf/p_inf_ref)")
