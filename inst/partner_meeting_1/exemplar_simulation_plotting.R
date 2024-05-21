#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+++++ Blueprint: Exemplar Model Run Visualisation +++++#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++#

#+++ Introduction +++#
#++++++++++++++++++++#

#----- 1) Preamble ---------------------------------------------------------------------------------

# Load the finalsize package:
library(finalsize)
library(tidyverse)
library(grid)
library(gridExtra)

#----- 2) Load the dataframes ----------------------------------------------------------------------

# Load in the raw model outputs:

#+++ Epidemic +++#
epidemic_baseline_raw <- readRDS("/Users/trb216/OneDrive - Imperial College London/Desktop/helios/data/blueprint_demo_May_raw_outputs/exemplar_baseline_output_raw_3331.rds")
epidemic_random_raw <- readRDS("/Users/trb216/OneDrive - Imperial College London/Desktop/helios/data/blueprint_demo_May_raw_outputs/exemplar_uvc_random_output_raw_33331.rds")
epidemic_targeted_raw <- readRDS("/Users/trb216/OneDrive - Imperial College London/Desktop/helios/data/blueprint_demo_May_raw_outputs/exemplar_uvc_targeted_output_raw_33331.rds")

#+++ Endemic +++#
endemic_baseline_raw <- readRDS("/Users/trb216/OneDrive - Imperial College London/Desktop/helios/data/blueprint_demo_May_raw_outputs/endemic_baseline_2_week_immunity_10_years.rds")
endemic_random_raw <- readRDS("/Users/trb216/OneDrive - Imperial College London/Desktop/helios/data/blueprint_demo_May_raw_outputs/endemic_random_uvc_2_week_immunity_10_years.rds")
endemic_targeted_raw <- readRDS("/Users/trb216/OneDrive - Imperial College London/Desktop/helios/data/blueprint_demo_May_raw_outputs/endemic_targeted_uvc_2_week_immunity_10_years.rds")

#----- 3a) Create single, long-form dataframe ------------------------------------------------------

#+++ Epidemic: Baseline +++#
epidemic_baseline_raw %>%
  rename("S" = S_count,
         "E" = E_count,
         "I" = I_count,
         "R" = R_count) %>%
  mutate(Total_count = S + E + I + R) %>%
  mutate(S = S / Total_count,
         E = E / Total_count,
         I = I / Total_count,
         R = R / Total_count) %>%
  select(timestep, S, E, I, R) %>%
  pivot_longer(cols = c(S, E, I, R), names_to = "State", values_to = "Proportion") %>%
  mutate(State = factor(State, levels = c("S", "E", "I", "R"))) %>%
  mutate(Setting = "Epidemic") %>%
  mutate(Intervention = "Baseline") -> epidemic_baseline_long

#+++ Epidemic: Baseline +++#
epidemic_random_raw %>%
  rename("S" = S_count,
         "E" = E_count,
         "I" = I_count,
         "R" = R_count) %>%
  mutate(Total_count = S + E + I + R) %>%
  mutate(S = S / Total_count,
         E = E / Total_count,
         I = I / Total_count,
         R = R / Total_count) %>%
  select(timestep, S, E, I, R) %>%
  pivot_longer(cols = c(S, E, I, R), names_to = "State", values_to = "Proportion") %>%
  mutate(State = factor(State, levels = c("S", "E", "I", "R"))) %>%
  mutate(Setting = "Epidemic") %>%
  mutate(Intervention = "Random") -> epidemic_random_long

#+++ Epidemic: Baseline +++#
epidemic_targeted_raw %>%
  rename("S" = S_count,
         "E" = E_count,
         "I" = I_count,
         "R" = R_count) %>%
  mutate(Total_count = S + E + I + R) %>%
  mutate(S = S / Total_count,
         E = E / Total_count,
         I = I / Total_count,
         R = R / Total_count) %>%
  select(timestep, S, E, I, R) %>%
  pivot_longer(cols = c(S, E, I, R), names_to = "State", values_to = "Proportion") %>%
  mutate(State = factor(State, levels = c("S", "E", "I", "R"))) %>%
  mutate(Setting = "Epidemic") %>%
  mutate(Intervention = "Targeted") -> epidemic_targeted_long

#+++ Endemic: Baseline +++#
endemic_baseline_raw %>%
  rename("S" = S_count,
         "E" = E_count,
         "I" = I_count,
         "R" = R_count) %>%
  mutate(Total_count = S + E + I + R) %>%
  mutate(S = S / Total_count,
         E = E / Total_count,
         I = I / Total_count,
         R = R / Total_count) %>%
  select(timestep, S, E, I, R) %>%
  pivot_longer(cols = c(S, E, I, R), names_to = "State", values_to = "Proportion") %>%
  mutate(State = factor(State, levels = c("S", "E", "I", "R"))) %>%
  mutate(Setting = "Endemic") %>%
  mutate(Intervention = "Baseline")-> endemic_baseline_long

#+++ Endemic: Baseline +++#
endemic_random_raw %>%
  rename("S" = S_count,
         "E" = E_count,
         "I" = I_count,
         "R" = R_count) %>%
  mutate(Total_count = S + E + I + R) %>%
  mutate(S = S / Total_count,
         E = E / Total_count,
         I = I / Total_count,
         R = R / Total_count) %>%
  select(timestep, S, E, I, R) %>%
  pivot_longer(cols = c(S, E, I, R), names_to = "State", values_to = "Proportion") %>%
  mutate(State = factor(State, levels = c("S", "E", "I", "R"))) %>%
  mutate(Setting = "Endemic") %>%
  mutate(Intervention = "Random")-> endemic_random_long

#+++ Endemic: Baseline +++#
endemic_targeted_raw %>%
  rename("S" = S_count,
         "E" = E_count,
         "I" = I_count,
         "R" = R_count) %>%
  mutate(Total_count = S + E + I + R) %>%
  mutate(S = S / Total_count,
         E = E / Total_count,
         I = I / Total_count,
         R = R / Total_count) %>%
  select(timestep, S, E, I, R) %>%
  pivot_longer(cols = c(S, E, I, R), names_to = "State", values_to = "Proportion") %>%
  mutate(State = factor(State, levels = c("S", "E", "I", "R"))) %>%
  mutate(Setting = "Endemic") %>%
  mutate(Intervention = "Targeted") -> endemic_targeted_long

#+++ Combined +++#
exemplar_long <- bind_rows(epidemic_baseline_long, epidemic_random_long, epidemic_targeted_long,
                            endemic_baseline_long, endemic_random_long, endemic_targeted_long)

#----- 3b) Create single, long-form dataframe ------------------------------------------------------

#+++ Epidemic: Baseline +++#
epidemic_baseline_raw %>%
  rename("Susceptible" = S_count,
         "Exposed" = E_count,
         "Infected" = I_count,
         "Recovered" = R_count) %>%
  mutate(Total_count = Susceptible + Exposed + Infected + Recovered) %>%
  mutate(Susceptible = Susceptible / Total_count,
         Exposed = Exposed / Total_count,
         Infected = Infected / Total_count,
         Recovered = Recovered / Total_count) %>%
  select(timestep, Susceptible, Exposed, Infected, Recovered) %>%
  pivot_longer(cols = c(Susceptible, Exposed, Infected, Recovered), names_to = "State", values_to = "Proportion") %>%
  mutate(State = factor(State, levels = c("Susceptible", "Exposed", "Infected", "Recovered"))) %>%
  mutate(Setting = "Epidemic") %>%
  mutate(Intervention = "Baseline") -> epidemic_baseline_long

#+++ Epidemic: Baseline +++#
epidemic_random_raw %>%
  rename("Susceptible" = S_count,
         "Exposed" = E_count,
         "Infected" = I_count,
         "Recovered" = R_count) %>%
  mutate(Total_count = Susceptible + Exposed + Infected + Recovered) %>%
  mutate(Susceptible = Susceptible / Total_count,
         Exposed = Exposed / Total_count,
         Infected = Infected / Total_count,
         Recovered = Recovered / Total_count) %>%
  select(timestep, Susceptible, Exposed, Infected, Recovered) %>%
  pivot_longer(cols = c(Susceptible, Exposed, Infected, Recovered), names_to = "State", values_to = "Proportion") %>%
  mutate(State = factor(State, levels = c("Susceptible", "Exposed", "Infected", "Recovered"))) %>%
  mutate(Setting = "Epidemic") %>%
  mutate(Intervention = "Random") -> epidemic_random_long

#+++ Epidemic: Baseline +++#
epidemic_targeted_raw %>%
  rename("Susceptible" = S_count,
         "Exposed" = E_count,
         "Infected" = I_count,
         "Recovered" = R_count) %>%
  mutate(Total_count = Susceptible + Exposed + Infected + Recovered) %>%
  mutate(Susceptible = Susceptible / Total_count,
         Exposed = Exposed / Total_count,
         Infected = Infected / Total_count,
         Recovered = Recovered / Total_count) %>%
  select(timestep, Susceptible, Exposed, Infected, Recovered) %>%
  pivot_longer(cols = c(Susceptible, Exposed, Infected, Recovered), names_to = "State", values_to = "Proportion") %>%
  mutate(State = factor(State, levels = c("Susceptible", "Exposed", "Infected", "Recovered"))) %>%
  mutate(Setting = "Epidemic") %>%
  mutate(Intervention = "Targeted") -> epidemic_targeted_long

#+++ Endemic: Baseline +++#
endemic_baseline_raw %>%
  rename("Susceptible" = S_count,
         "Exposed" = E_count,
         "Infected" = I_count,
         "Recovered" = R_count) %>%
  mutate(Total_count = Susceptible + Exposed + Infected + Recovered) %>%
  mutate(Susceptible = Susceptible / Total_count,
         Exposed = Exposed / Total_count,
         Infected = Infected / Total_count,
         Recovered = Recovered / Total_count) %>%
  select(timestep, Susceptible, Exposed, Infected, Recovered) %>%
  pivot_longer(cols = c(Susceptible, Exposed, Infected, Recovered), names_to = "State", values_to = "Proportion") %>%
  mutate(State = factor(State, levels = c("Susceptible", "Exposed", "Infected", "Recovered"))) %>%
  mutate(Setting = "Endemic") %>%
  mutate(Intervention = "Baseline")-> endemic_baseline_long

#+++ Endemic: Baseline +++#
endemic_random_raw %>%
  rename("Susceptible" = S_count,
         "Exposed" = E_count,
         "Infected" = I_count,
         "Recovered" = R_count) %>%
  mutate(Total_count = Susceptible + Exposed + Infected + Recovered) %>%
  mutate(Susceptible = Susceptible / Total_count,
         Exposed = Exposed / Total_count,
         Infected = Infected / Total_count,
         Recovered = Recovered / Total_count) %>%
  select(timestep, Susceptible, Exposed, Infected, Recovered) %>%
  pivot_longer(cols = c(Susceptible, Exposed, Infected, Recovered), names_to = "State", values_to = "Proportion") %>%
  mutate(State = factor(State, levels = c("Susceptible", "Exposed", "Infected", "Recovered"))) %>%
  mutate(Setting = "Endemic") %>%
  mutate(Intervention = "Random")-> endemic_random_long

#+++ Endemic: Baseline +++#
endemic_targeted_raw %>%
  rename("Susceptible" = S_count,
         "Exposed" = E_count,
         "Infected" = I_count,
         "Recovered" = R_count) %>%
  mutate(Total_count = Susceptible + Exposed + Infected + Recovered) %>%
  mutate(Susceptible = Susceptible / Total_count,
         Exposed = Exposed / Total_count,
         Infected = Infected / Total_count,
         Recovered = Recovered / Total_count) %>%
  select(timestep, Susceptible, Exposed, Infected, Recovered) %>%
  pivot_longer(cols = c(Susceptible, Exposed, Infected, Recovered), names_to = "State", values_to = "Proportion") %>%
  mutate(State = factor(State, levels = c("Susceptible", "Exposed", "Infected", "Recovered"))) %>%
  mutate(Setting = "Endemic") %>%
  mutate(Intervention = "Targeted") -> endemic_targeted_long

#+++ Combined +++#
exemplar_long <- bind_rows(epidemic_baseline_long, epidemic_random_long, epidemic_targeted_long,
                           endemic_baseline_long, endemic_random_long, endemic_targeted_long)

# Save the combined long-form dataframe:
#saveRDS(object = exemplar_long, file = "./inst/partner_meeting_1/exemplar_simulations.rds")


#----- 4) Figure 1: Disease State Dynamics ---------------------------------------------------------

# Store colours for plotting:
disease_state_colours <- c("#4cd8ff", "#f8ed5b", "brown2", "#a633ff")

# Try and create blueprint-esqe colour palette:
blueprint_colours <- c("#C0E3FF", "#76CFFF", "#00AFFF","#03113E")
blueprint_colours <- c("#03113E", "#00AFFF", "#76CFFF", "#C0E3FF")
blueprint_colours <- colorRampPalette(c("#00AFFF", "#03113E"))(4)

# Set a plotting window for the x-axis:
plotting_window <- 1000



#+++ All Interventions and Settings +++#
exemplar_long %>%
  filter(timestep <= plotting_window) %>%
  #filter(State %in% c("I", "R")) %>%
  ggplot(aes(x = timestep, y = Proportion, colour = State)) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  labs(x = "Time", y = "Proportion of Population", colour = "Disease State") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_color_discrete(type = disease_state_colours) +
  facet_grid(Setting~Intervention) +
  theme(strip.background = element_rect(fill = "black"),
        strip.text = element_text(colour = "white", size = 12, face = "bold"),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14))

#+++ Single Setting +++#
exemplar_long %>%
  filter(timestep <= plotting_window) %>%
  filter(Setting == "Epidemic") %>%
  #filter(State %in% c("I", "R")) %>%
  ggplot(aes(x = timestep, y = Proportion, colour = State)) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  labs(x = "Time", y = "Proportion of Population", colour = "Disease State") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_color_discrete(type = blueprint_colours) +
  facet_grid(~Intervention) +
  theme(strip.background = element_rect(fill = "#03113E"),
        strip.text = element_text(colour = "white", size = 12),
        legend.title = element_blank(),
        legend.position = c(0.92, 0.85),
        legend.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        axis.text = element_text(colour = "black"))

#+++ Single Setting: Simple +++#
exemplar_long %>%
  filter(timestep <= plotting_window) %>%
  filter(Setting == "Epidemic") %>%
  #filter(State %in% c("I", "R")) %>%
  mutate(Percentage = Proportion * 100) %>%
  ggplot(aes(x = timestep, y = Percentage, colour = State)) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  labs(x = "Days", y = "Percentage of Population", colour = "Disease State") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_color_discrete(type = blueprint_colours) +
  facet_grid(~Intervention) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "#03113E", size = 12),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.92, 0.85),
        legend.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        axis.text = element_text(colour = "black"))

#+++ Single Setting: Simpler +++#
exemplar_long %>%
  filter(Setting == "Epidemic") %>%
  filter(timestep %in% c(1:plotting_window)) %>%
  #filter(State %in% c("I", "R")) %>%
  mutate(Percentage = Proportion * 100) %>%
  ggplot(aes(x = timestep, y = Percentage, colour = State)) +
  geom_line(linewidth = 1.2) +
  theme_bw() +
  labs(x = "Days", y = "Percentage of Population", colour = "Disease State") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_color_discrete(type = blueprint_colours) +
  facet_grid(~Intervention) +
  theme(strip.background = element_blank(),
        strip.text = element_text(colour = "#03113E", size = 12),
        panel.border=element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.93, 0.85),
        legend.background = element_blank(),
        legend.text = element_text(size = 11),
        axis.line = element_line(),
        axis.title = element_text(size = 12),
        axis.text = element_text(colour = "black")) -> exemplar_epidemic_figure

exemplar_long %>%
  filter(timestep %in% c(300:plotting_window)) %>%
  filter(Setting == "Endemic") %>%
  #filter(State %in% c("I", "R")) %>%
  mutate(Percentage = Proportion * 100) %>%
  ggplot(aes(x = timestep, y = Percentage, colour = State)) +
  geom_line(linewidth = 1.2) +
  theme_bw() +
  labs(x = "Days", y = "Percentage of Population", colour = "Disease State") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_color_discrete(type = blueprint_colours) +
  facet_grid(~Intervention) +
  theme(strip.background = element_blank(),
        strip.text = element_text(colour = "#03113E", size = 12),
        panel.border=element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.93, 0.6),
        legend.background = element_blank(),
        legend.text = element_text(size = 11),
        axis.line = element_line(),
        axis.title = element_text(size = 12),
        axis.text = element_text(colour = "black")) -> exemplar_endemic_figure

# Save the figures:
ggsave(filename = "./inst/partner_meeting_1/exemplar_epidemic_figure.pdf")
ggsave(filename = "./inst/partner_meeting_1/exemplar_endemic_figure.pdf")

#----- 5) Figure 2: Cases Averted ------------------------------------------------------------------

##' Here, we want to plot the number of cases averted using a) random, and b) targeted UVC
##'
##' We calculate the cases averted as the difference in the number of recovered individuals through
##' time between the baseline scenario and the UVC scenarios

bind_cols(epidemic_baseline_raw,
          "R_count_random" = epidemic_baseline_raw$R_count,
          "R_count_targeted" = epidemic_targeted_raw$R_count)







