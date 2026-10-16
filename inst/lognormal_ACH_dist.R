## Exploring ACH (Lognormal) → p_inf → riskiness via Wells–Riley

set.seed(123)

n_samples <- 3000

# School ACH empirical quantiles
q10 <- 0.1
q90 <- 2.9

sdlog_ach <- (log(q90) - log(q10)) / (1.2816 - (-1.2816))
meanlog_ach <- log(0.8)

ACH_samples <- rlnorm(
  n = n_samples,
  meanlog = meanlog_ach,
  sdlog = sdlog_ach
)

hist(ACH_samples,
     breaks = 500,
     col = "skyblue",
     main = "ACH values (lognormal)",
     xlab = "ACH",
     xlim = c(min(ACH_samples), 20))

# 2. Wells–Riley parameters
I <- 1
pi <- 27
room_vol <- 150
kD <- 0.64

alpha_samples <- ACH_samples + kD
Css_samples <- (I * pi) / (alpha_samples * room_vol)

# 3. Infection probability
r <- 1.37e-2
RRtv <- 0.45
t <- 4

p_inf_samples <- 1 - exp(-r * Css_samples * RRtv * t)

hist(p_inf_samples,
     breaks = 30,
     col = "grey",
     main = "Distribution of p_inf (Lognormal ACH)",
     xlab = "p_inf (Wells–Riley)")

# -------------------------------
# 4. Define reference ACH (median)
# -------------------------------

ACH_ref <- median(ACH_samples)

alpha_ref <- ACH_ref + kD
Css_ref <- (I * pi) / (alpha_ref * room_vol)

p_inf_ref <- 1 - exp(-r * Css_ref * RRtv * t)

# -------------------------------
# 5. Riskiness (relative risk)
# -------------------------------

riskiness <- p_inf_samples / p_inf_ref

summary(riskiness)

hist(riskiness,
     breaks = 30,
     col = "darkgreen",
     main = "Riskiness (Lognormal ACH)",
     xlab = "Riskiness")
