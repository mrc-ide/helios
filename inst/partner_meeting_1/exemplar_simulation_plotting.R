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

#----- 3) Create single, long-form dataframe -------------------------------------------------------

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

#----- 4) Figure 1: Disease State Dynamics ---------------------------------------------------------

# Store colours for plotting:
disease_state_colours <- c("#4cd8ff", "#f8ed5b", "brown2", "#a633ff")

# Set a plotting window for the x-axis:
plotting_window <- 900

# Facet plot of the SEIR dynamics between the different model settings and interventions:
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

#----- 5) Figure 2: Cases Averted ------------------------------------------------------------------

##' Here, we want to plot the number of cases averted using a) random, and b) targeted UVC
##'
##' We calculate the cases averted as the difference in the number of recovered individuals through
##' time between the baseline scenario and the UVC scenarios

bind_cols(epidemic_baseline_raw,
          "R_count_random" = epidemic_baseline_raw$R_count,
          "R_count_targeted" = epidemic_targeted_raw$R_count)







