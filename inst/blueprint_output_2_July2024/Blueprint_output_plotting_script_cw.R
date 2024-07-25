# Load in the requisite packages:
library(helios)
library(ggplot2)
library(tidyverse)

# Loading in the outputs
outputs <- readRDS("inst/blueprint_output_2_July2024/raw_outputs_results1.rds")

# Processing the data
years_to_simulate <- 7 # 6 in newer outputs
dt <- 0.5
timestep_baseline_start <- ((years_to_simulate - 4) * 365) / dt
timestep_baseline_end <- ((years_to_simulate - 2) * 365) / dt
timestep_uvc_start <- ((years_to_simulate - 2) * 365 + 1) / dt
timestep_uvc_end <- (years_to_simulate * 365) / dt

##
outputs_processed <- outputs
index_start <- which(outputs_processed[[1]]$timestep == timestep_baseline_start)
index_end <- which(outputs_processed[[1]]$timestep == timestep_uvc_end)
for (i in 1:length(outputs)) {
  outputs_processed[[i]] <- outputs_processed[[i]][index_start:index_end, ]
  outputs_processed[[i]]$archetype <- simulations_to_run[i, "archetype"]
  outputs_processed[[i]]$coverage <- simulations_to_run[i, "coverage"]
  outputs_processed[[i]]$efficacy <- simulations_to_run[i, "efficacy"]
  outputs_processed[[i]]$iteration <- simulations_to_run[i, "iteration"]
}

overall <- dplyr::bind_rows(outputs_processed)
colnames(overall)

table(overall$archetype)
table(overall$ID)
table(overall$efficacy)
table(overall$coverage)
table(overall$iteration)
table(overall$E_new)

ggplot(overall, aes(x = timestep, y = E_new,
                    col = interaction(factor(iteration), archetype))) +
  geom_line() +
  facet_grid(efficacy ~ coverage)

overall2 <- overall %>%
  group_by(timestep, archetype, efficacy, coverage) %>%
  summarise(S_count = mean(S_count),
            E_new = mean(E_new))

ggplot(overall2, aes(x = timestep, y = S_count, col = archetype)) +
  geom_line() +
  facet_grid(efficacy ~ coverage)

index <- 2 * 365 / 0.5
end <- length(unique(overall$timestep))
overall3 <- overall %>%
  group_by(timestep, archetype, efficacy, coverage) %>%
  summarise(S_count = mean(S_count), E_new = mean(E_new)) %>%
  ungroup() %>%
  group_by(archetype, efficacy, coverage) %>%
  summarise(incidence_pre_uvc = sum(E_new[1:index]),
            incidence_after_uvc = sum(E_new[(index + 1):end])) %>%
  pivot_longer(cols = c(incidence_pre_uvc, incidence_after_uvc),
               values_to = "incidence",
               names_to = "period")

overall3$period <- factor(overall3$period,
                          levels = c("incidence_pre_uvc",
                                     "incidence_after_uvc"))

ggplot(overall3,
       aes(x = efficacy, y = incidence, fill = period)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(archetype ~ coverage, scales = "free_y")

for (i in 1:40) {

  x <- outputs[[i]]
  index_start <- which(x$timestep == timestep_baseline_start)
  index_end <- which(x$timestep == timestep_uvc_end)
  x <- x[index_start:index_end, ]

  layout(matrix(c(1,1,2,3, 1,1,4,5), nrow = 2, byrow = TRUE))
  par(mar = c(3, 3, 3, 3))
  plot(x$timestep * 0.5, x$S_count, ylim = c(0, 100000), type = "l", col = "#333333", xlab = "Time", ylab = "Number")
  lines(x$timestep * 0.5, x$E_new, ylim = c(0, 100000), type = "l", col = "#A0D848")
  lines(x$timestep * 0.5, x$n_external_infections, ylim = c(0, 100000), type = "l", col = "#F15A24")
  lines(x$timestep * 0.5, x$R_count, ylim = c(0, 100000), type = "l", col = "#29ABE2")
  abline(v = timestep_baseline_start * 0.5)
  abline(v = timestep_baseline_end * 0.5)
  abline(v = timestep_uvc_start * 0.5)
  abline(v = timestep_uvc_end * 0.5)

  plot(x$timestep * 0.5, x$S_count, ylim = c(0, max(x$S_count)), type = "l", col = "#333333", xlab = "Time", ylab = "S Count")
  plot(x$timestep * 0.5, x$E_new, ylim = c(0, max(x$E_new)), type = "l", col = "#A0D848", xlab = "Time", ylab = "E Count")
  plot(x$timestep * 0.5, x$n_external_infections, ylim = c(0, max(x$n_external_infections)), type = "l", col = "#F15A24", xlab = "Time", ylab = "I Count")
  plot(x$timestep * 0.5, x$R_count, ylim = c(0, max(x$R_count)), type = "l", col = "#29ABE2", xlab = "Time", ylab = "R Count")

  browser()

}

