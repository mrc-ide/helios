
# Load in requisite packages:
library(tidyverse)

# Load the base parameters:
parameters <- get_parameters(overrides = list(simulation_time = 100))

# Run the model:
simulation_outputs <- run_simulation(parameters_list = parameters)

# Plot the disease dynamics:
simulation_outputs |>
  pivot_longer(cols = c(S_count, E_count, I_count, R_count), names_to = "State", values_to = "Individuals") %>%
  ggplot(aes(x = timestep, y = Individuals, colour = State)) + geom_line(linewidth = 1.2) +
  theme_bw() +
  labs(x = "Time", y = "Individuals", colour = "Disease State") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0))
