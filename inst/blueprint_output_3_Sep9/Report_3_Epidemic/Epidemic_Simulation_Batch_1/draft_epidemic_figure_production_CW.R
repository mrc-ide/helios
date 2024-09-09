#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+++++ Visual Checks of Endemic Simulation Outputs +++++#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# Load in the requisite packages:
library(tidyverse)
library(patchwork)

# Turn off scientific notation:
options(scipen = 666)

# Specify the Blueprint colours:
blueprint_colours <- colorRampPalette(c("#00AFFF", "#03113E"))(4)
blueprint_colours <- c(blueprint_colours[1], blueprint_colours[4])

##' Simulations were run varying the following variables:
##'
##' 1. archetype (flu, SARS-CoV-2)
##' 2. coverage type ()
##' 3. coverage
##' 4. efficacy
##'
##' 25 iterations were run for each parameterisation

#----- 1) Load the Outputs -------------------------------------------------------------------------

# Set the working directory to within the Blueprint Report 3 directory:
setwd("./inst/blueprint_output_3_Sep9/")

# List the names of the files in the endemic directory and load them into a single, unified dataframe
output_files <- list.files("./Report_3_Epidemic/Epidemic_Simulation_Batch_1/epidemic_batch_1_outputs/")
output_files <- output_files[which(stringr::str_detect(string = output_files, pattern = "scenario_output"))]

# Check the number of files (should be 30)
length(output_files); length(output_files) == 30

#----- 2) Create the dataframe(s) ------------------------------------------------------------------

# Quick visualisation of key metrics for endemic#
summary_outputs <- data.frame()

# Initiate a counter to reimpose an "iteration" column on the dataframes:
counter <- 1

# Loop through the output files, match the archetype using the beta values, and create a combined dataframe:
for(i in 1: length(output_files)) {

  # Load in the output file:
  temp_obj <- readRDS(file = paste0("./Report_3_Epidemic/Epidemic_Simulation_Batch_1/epidemic_batch_1_outputs/", output_files[i]))

  # Filter out the parameter lists:
  tempdf <- list()
  for(j in 1:length(temp_obj)) {
    tempdf[[j]] <- temp_obj[[j]][[2]]
    if (temp_obj[[j]][[1]]$beta_household == 0.207) {
      tempdf[[j]]$archetype <- "influenza"
    } else {
      tempdf[[j]]$archetype <- "sars_cov_2"
    }
    tempdf[[j]]$iteration <- counter
    counter <- counter + 1
  }

  # Create a combined dataframe of the outputs
  tempdf2 <- bind_rows(tempdf) |>
    mutate(coverage_type = ifelse(is.na(coverage_type), "none", coverage_type)) |>
    mutate(coverage  = ifelse(is.na(coverage), 0, coverage)) |>
    mutate(efficacy  = ifelse(is.na(efficacy), 0, efficacy)) |>
    group_by(iteration, archetype, coverage_type, coverage, efficacy, disease_status) |>
    summarise(peak = max(I_count),
              peak_timing = first(which(I_count == max(I_count))),
              final_size = max(R_count))

  # Bind it to the combined dataframe:
  summary_outputs <- bind_rows(summary_outputs, tempdf2)
}

# Generate the summary metrics for the epidemic report:
avg_summary_outputs <- summary_outputs |>
  group_by(archetype, coverage_type, coverage, efficacy, disease_status) |>
  summarise(mean_peak_timing = mean(peak_timing),
            min_peak_timing = min(peak_timing),
            max_peak_timing = max(peak_timing),
            lq_peak_timing = quantile(peak_timing, 0.25, na.rm = TRUE),
            uq_peak_timing = quantile(peak_timing, 0.75, na.rm = TRUE),
            mean_peak = mean(peak),
            min_peak = min(peak),
            max_peak = max(peak),
            lq_peak = quantile(peak, 0.25, na.rm = TRUE),
            uq_peak = quantile(peak, 0.75, na.rm = TRUE),
            mean_final_size = mean(final_size),
            min_final_size = min(final_size),
            max_final_size = max(final_size),
            lq_final_size = quantile(final_size, 0.25, na.rm = TRUE),
            uq_final_size = quantile(final_size, 0.75, na.rm = TRUE))

# Save the dataframes:
saveRDS(object = summary_outputs, file = "./Report_3_Epidemic/Epidemic_Simulation_Batch_1/epidemic_summary_outputs.rds")
saveRDS(object = avg_summary_outputs, file = "./Report_3_Epidemic/Epidemic_Simulation_Batch_1/epidemic_avg_summary_outputs.rds")

##' NOTE: As the completed runs did not append all column identifiers / parameter values, the number
##' of parameter combinations appears different from the parameter table generated when creating the
##' parameter lists. We have introduced variable values (e.g. coverate_type == "none") that were not
##' present in the original parameter sets. Not a problem necessarily, but needs to be born in mind
##' when running the script.

#----- 3) Figure.2.1.a Mean Final Epidemic Size ----------------------------------------------------

#+++ Figure 2.1.a: Mean final epidemic size +++#
ggplot() +
  geom_line(data = subset(avg_summary_outputs, coverage_type == "random" & coverage <= 0.7),
            aes(x = 100 * coverage, y = mean_final_size / 1000), col = blueprint_colours[1], linewidth = 1) +
  geom_line(data = subset(avg_summary_outputs, coverage_type == "targeted_riskiness" & coverage <= 0.7),
            aes(x = 100 * coverage, y = mean_final_size / 1000), col = blueprint_colours[2], linewidth = 1) +
  theme_bw(base_size = 12) +
  labs(x = "Far UVC Coverage (%)",
       y = "Mean Final Epidemic Size \n (per 1000 individuals)",
       colour = "Coverage\nType") +
  facet_grid(archetype ~ efficacy,
             scales = "free_y",
             labeller = as_labeller(c(`influenza` = "Influenza",
                                     `sars_cov_2` = "SARS-CoV-2",
                                     `0.4` = "40% Efficacy",
                                     `0.6` = "60% Efficacy",
                                     `0.8` = "80% Efficacy"))) +
  theme(axis.text = element_text(colour = "black"),
        axis.title = element_text(size = 12)) +
  lims(y = c(0, NA))

# TB Version:
avg_summary_outputs |>
  filter(coverage <= 0.7, efficacy > 0) |>
  ggplot(aes(x = 100 * coverage, y = mean_final_size / 1000, colour = coverage_type)) +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(values = blueprint_colours, labels = c("Random", "Targeted")) +
  scale_fill_manual(values = blueprint_colours, labels = c("Random", "Targeted")) +
  geom_ribbon(aes(ymin = min_final_size / 1000, ymax = max_final_size / 1000, fill = coverage_type),
              colour = NA, alpha = 0.3, show.legend = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw(base_size = 12) +
  labs(x = "Far UVC Coverage (%)",
       y = "Mean Final Epidemic Size \n (per 1000 individuals)",
       colour = "Coverage\nType") +
  facet_grid(archetype ~ efficacy,
             scales = "free_y",
             labeller = as_labeller(c(`influenza` = "Influenza",
                                      `sars_cov_2` = "SARS-CoV-2",
                                      `0.4` = "40% Efficacy",
                                      `0.6` = "60% Efficacy",
                                      `0.8` = "80% Efficacy"))) +
  theme(axis.text = element_text(colour = "black"),
        axis.title = element_text(size = 12)) +
  lims(y = c(0, NA))

#----- 4) Figure.2.1.b Mean Peak Size --------------------------------------------------------------

#+++ Figure 2.1.b: +++#
ggplot() +
  geom_line(data = subset(avg_summary_outputs, coverage_type == "random" & coverage <= 0.7),
            aes(x = 100 * coverage, y = mean_peak / 1000), col = blueprint_colours[1], linewidth = 1) +
  geom_line(data = subset(avg_summary_outputs, coverage_type == "targeted_riskiness" & coverage <= 0.7),
            aes(x = 100 * coverage, y = mean_peak / 1000), col = blueprint_colours[2], linewidth = 1) +
  facet_grid(archetype ~ efficacy, scales = "free_y") +
  theme_bw(base_size = 12) +
  labs(x = "Far UVC Coverage (%)", y = "Mean Peak Epidemic Size \n (per 1000 individuals)") +
  facet_grid(archetype ~ efficacy,
             scales = "free_y",
             labeller = as_labeller(c(`influenza` = "Influenza",
                                      `sars_cov_2` = "SARS-CoV-2",
                                      `0.4` = "40% Efficacy",
                                      `0.6` = "60% Efficacy",
                                      `0.8` = "80% Efficacy"))) +
  theme(axis.text = element_text(colour = "black"),
        axis.title = element_text(size = 12)) +
  lims(y = c(0, NA))

# TB Version
avg_summary_outputs |>
  filter(coverage <= 0.7, efficacy > 0) |>
  ggplot(aes(x = 100 * coverage, y = mean_peak / 1000, colour = coverage_type)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = blueprint_colours, labels = c("Random", "Targeted")) +
  scale_fill_manual(values = blueprint_colours, labels = c("Random", "Targeted")) +
  geom_ribbon(aes(ymin = min_peak / 1000, ymax = max_peak / 1000, fill = coverage_type),
              colour = NA, alpha = 0.3, show.legend = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw(base_size = 12) +
  labs(x = "Far UVC Coverage (%)",
       y = "Mean Final Epidemic Size \n (per 1000 individuals)",
       colour = "Coverage\nType") +
  facet_grid(archetype ~ efficacy,
             scales = "free_y",
             labeller = as_labeller(c(`influenza` = "Influenza",
                                      `sars_cov_2` = "SARS-CoV-2",
                                      `0.4` = "40% Efficacy",
                                      `0.6` = "60% Efficacy",
                                      `0.8` = "80% Efficacy"))) +
  theme(axis.text = element_text(colour = "black"),
        axis.title = element_text(size = 12),
        legend.text  = element_text(size = 11)) +
  lims(y = c(0, NA))

#----- 5) Figure.2.1.c Mean Peak Timing ------------------------------------------------------------

#+++ Figure 2.1.c: Mean Peak Timing +++#
ggplot() +
  geom_line(data = subset(avg_summary_outputs, coverage_type == "random" & coverage <= 0.7),
            aes(x = 100 * coverage, y = mean_peak_timing), col = blueprint_colours[1], linewidth = 1) +
  geom_line(data = subset(avg_summary_outputs, coverage_type == "targeted_riskiness" & coverage <= 0.7),
            aes(x = 100 * coverage, y = mean_peak_timing), col = blueprint_colours[2], linewidth = 1) +
  theme_bw(base_size = 12) +
  labs(x = "Far UVC Coverage (%)", y = "Time to Peak Infections (days)") +
  facet_grid(archetype ~ efficacy,
             scales = "free_y",
             labeller = as_labeller(c(`influenza` = "Influenza",
                                      `sars_cov_2` = "SARS-CoV-2",
                                      `0.4` = "40% Efficacy",
                                      `0.6` = "60% Efficacy",
                                      `0.8` = "80% Efficacy"))) +
  theme(axis.text = element_text(colour = "black"),
        axis.title = element_text(size = 12),
        legend.text  = element_text(size = 11)) +
  lims(y = c(0, NA))

# TB Version
avg_summary_outputs |>
  filter(coverage <= 0.7, efficacy > 0) |>
  ggplot(aes(x = 100 * coverage, y = mean_peak_timing, colour = coverage_type)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = blueprint_colours, labels = c("Random", "Targeted")) +
  scale_fill_manual(values = blueprint_colours, labels = c("Random", "Targeted")) +
  geom_ribbon(aes(ymin = min_peak_timing, ymax = max_peak_timing, fill = coverage_type),
              colour = NA, alpha = 0.3, show.legend = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw(base_size = 12) +
  labs(x = "Far UVC Coverage (%)",
       y = "Time to Peak Infections (days)",
       colour = "Coverage\nType") +
  facet_grid(archetype ~ efficacy,
             scales = "free_y",
             labeller = as_labeller(c(`influenza` = "Influenza",
                                      `sars_cov_2` = "SARS-CoV-2",
                                      `0.4` = "40% Efficacy",
                                      `0.6` = "60% Efficacy",
                                      `0.8` = "80% Efficacy"))) +
  theme(axis.text = element_text(colour = "black"),
        axis.title = element_text(size = 12),
        legend.text  = element_text(size = 11)) +
  lims(y = c(0, NA))

#----- 6) Figure.2.1.d Epidemic Exemplar -----------------------------------------------------------

##' sars_cov_2
##' targeted_riskiness
##' coverage = 60%
##' efficacy = 60%

# Read in the epidemic parameters table:
epidemic_parameter_table <- readRDS("./Report_3_Epidemic/Epidemic_Simulation_Batch_1/epidemic_simulations_table_batch_1.rds")

# Determine which batch the exemplar simulation is in:
epidemic_parameter_table |>
  mutate(coverage = round(coverage, 2)) |>
  filter(archetype == "sars_cov_2",
         coverage_type == "targeted_riskiness",
         efficacy == 0.6,
         coverage == 0.6) |>
  select(ID) -> batch_of_interest; min(batch_of_interest); max(batch_of_interest)

##' The IDs are 2951-2975

# View the input file names to see which batch we need:
str_sort(list.files("./Report_3_Epidemic/Epidemic_Simulation_Batch_1/epidemic_batch_1_inputs/"), numeric = TRUE)

##' The IDs are split across two batches, 27 and 28, so lets load the 27th:
exemplar_batch <- readRDS("./Report_3_Epidemic/Epidemic_Simulation_Batch_1/epidemic_batch_1_outputs/scenario_output_batch_1_27.rds")

# Store the exemplar simulation:
exemplar_simulation <- exemplar_batch[[110]][[2]]

# Plot the exemplar simulation:
exemplar_simulation |>
  filter(timestep <= 365) |>
  ggplot(aes(x = timestep, y = 1000 * E_new / 100000)) + geom_line(linewidth = 1, colour = "orange") +
  theme_bw(base_size = 12) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(y = "Daily Incidence Per 1,000 Population", x = "Time")

#----- 7) Relative differences between coverage types ----------------------------------------------

# View the column names for reference:
colnames(avg_summary_outputs)

# Peak Size
avg_summary_outputs |>
  mutate(coverage = round(coverage, 2)) |>
  filter(coverage %in% c(0.1, 0.7), efficacy %in% c(0.4, 0.8)) |>
  select(archetype, coverage_type, coverage, efficacy, mean_peak, min_peak, max_peak)

# Peak timing
avg_summary_outputs |>
  mutate(coverage = round(coverage, 2)) |>
  filter(coverage %in% c(0.1, 0.7), efficacy %in% c(0.4, 0.8)) |>
  select(archetype, coverage_type, coverage, efficacy, mean_peak_timing, min_peak_timing, max_peak_timing)

# Final size
avg_summary_outputs |>
  mutate(coverage = round(coverage, 2)) |>
  filter(coverage %in% c(0.1, 0.7), efficacy %in% c(0.4, 0.8)) |>
  select(archetype, coverage_type, coverage, efficacy, mean_final_size, min_final_size, max_final_size)

#----- 8) 10% Coverage Comparison (peak size) ------------------------------------------------------

##' In the absence of far UVC, the epidemic peaked at 6135 (5411-7678) people for influenza and
##' 20407 (19310-21351) for SARS-CoV-2.

# Flu

##' At 10% coverage and 60% efficacy, influenza epidemic peaked at 5426 (4671-6293) (an 11.6% (13.7-18.0%)
##' reduction) for random coverage and 4798 (4363-5695) (an 21.8% (19.4-25.8%) decrease) for targeted.
100 - (100 * (5426/6135))
100 - (100 * (4671/5411))
100 - (100 * (6293/7678))

100 - (100 * (4798/6135))
100 - (100 * (4363/5411))
100 - (100 * (5695/7678))

##' At 10% coverage and 80% efficacy, influenza epidemic peaked at 5274 (4691-5810) (an 14.0% (13.3-24.3%)
##' reduction) for random coverage and 4338 (3859-5416) for targeted (29.3% (28.7-29.5%)).
100 - (100 * (5274/6135))
100 - (100 * (4691/5411))
100 - (100 * (5810/7678))

100 - (100 * (4338/6135))
100 - (100 * (3859/5411))
100 - (100 * (5416/7678))

# SARS

##' At 10% coverage and 60% efficacy, SARS epidemic peaked at 19485 (18946-20153) for random coverage
##' (an 4.5% (1.9-5.6%) reduction) and  18831 (18093-19523) for targeted (an 7.7% (6.3-8.6%) decrease.
100 - (100 * (19485/20407))
100 - (100 * (18946/19310))
100 - (100 * (20153/21351))

100 - (100 * (18831/20407))
100 - (100 * (18093/19310))
100 - (100 * (19523/21351))

##' At 10% coverage and 80% efficacy, SARS epidemic peaked at 19028 (18440-19748) for random coverage
##' (an 6.8% (4.5-7.5%) reduction) and 18151 (17645-19149) for targeted (an 11.1% (8.6-10.3%) reduction).
100 - (100 * (19028/20407))
100 - (100 * (18440/19310))
100 - (100 * (19748/21351))

100 - (100 * (18151/20407))
100 - (100 * (17645/19310))
100 - (100 * (19149/21351))


# Peak Size:
avg_summary_outputs |>
  mutate(coverage = round(coverage, 2)) |>
  filter(coverage == 0.1, efficacy == 0.8) |>
  select(archetype, coverage_type, coverage, efficacy, mean_peak, min_peak, max_peak)

#----- 9) 10% Coverage Comparison (peak timing) ----------------------------------------------------

##' In the absence of far UVC, the epidemic peaked on day 125 (104-145) people for influenza and
##' 104 (98-110) for SARS-CoV-2.

# Peak Size:
avg_summary_outputs |>
  mutate(coverage = round(coverage, 2)) |>
  filter(coverage == 0) |>
  select(archetype, coverage_type, coverage, efficacy, mean_peak_timing, min_peak_timing, max_peak_timing)

# Flu

##' At 10% coverage and 60% efficacy, the influenza epidemic peaked 5 (1-10) days earlier for random coverage
##' and 16 (9-25) for targeted.

130 - 125
114 - 104
146 - 145

141 - 125
129 - 104
154 - 145

##' At 10% coverage and 80% efficacy, influenza epidemic peaked at 7 (3-16) days
##' for random coverage and 20 (17-25) for targeted

132 - 125
120 - 104
142 - 145

145 - 125
129 - 104
162 - 145

# SARS

##' At 10% coverage and 60% efficacy, SARS epidemic peaked at 2 (1-4) for random coverage
##' and 6 (5-8) for targeted

106 - 104
102 - 98
111 - 110

110 - 104
106 - 98
115 - 110

##' At 10% coverage and 80% efficacy, SARS epidemic peaked at 5 (3-6) days for random coverage
##' and 8 (6-9) for targeted

109 - 104
104 - 98
113 - 110

112 - 104
104 - 98
119 - 110

# Peak Size:
avg_summary_outputs |>
  mutate(coverage = round(coverage, 2)) |>
  filter(coverage == 0.1, efficacy == 0.8) |>
  select(archetype, coverage_type, coverage, efficacy, mean_peak_timing, min_peak_timing, max_peak_timing)

#----- 10) 10% Coverage Comparison (final size) ----------------------------------------------------

##' In the absence of far UVC, the epidemic reached a final size of 61262 (range 58127-65451) for influenza and
##' 89652 (range 88724-90327) for SARS.

avg_summary_outputs |>
  mutate(coverage = round(coverage, 2)) |>
  filter(coverage == 0.0) |>
  select(archetype, coverage_type, coverage, efficacy, mean_final_size, min_final_size, max_final_size)

# Flu

##' At 10% coverage and 60% efficacy, influenza epidemic finalised at 57910 (55031-61045) (a 5.5% (5.3-6.7%)
##' reduction) for random coverage and 56286 (54616-58804) (a 8.1% (6.0-10.1%) decrease) for targeted.
100 - (100 * (57910/61262))
100 - (100 * (55031/58127))
100 - (100 * (61045/65451))

100 - (100 * (56286/61262))
100 - (100 * (54616/58127))
100 - (100 * (58804/65451))

##' At 10% coverage and 80% efficacy, influenza epidemic peaked at 56918 (54594-59043) (a 7.1% (6.1-9.8%)
##' reduction) for random coverage and 53516 (50797-58350) for targeted (12.6% (10.8-12.7%)).
100 - (100 * (56918/61262))
100 - (100 * (54594/58127))
100 - (100 * (59043/65451))

100 - (100 * (53516/61262))
100 - (100 * (50797/58127))
100 - (100 * (58350/65451))

# SARS

##' At 10% coverage and 60% efficacy, SARS epidemic peaked at 88582 (87978-89292) for random coverage
##' (an 1.2% (0.8-1.1%) reduction) and 88312 (87720-88735) for targeted (a 1.5% (1.1-1.8%) decrease.
100 - (100 * (88582/89652))
100 - (100 * (87978/88724))
100 - (100 * (89292/90327))

100 - (100 * (88312/89652))
100 - (100 * (87720/88724))
100 - (100 * (88735/90327))

##' At 10% coverage and 80% efficacy, SARS epidemic peaked at 87780 (87320-88298) for random coverage
##' (an 2.1% (1.6-2.2%) reduction) and 87268 (86552-88205) for targeted (an 2.7% (2.3-2.4%) reduction).
100 - (100 * (87780/89652))
100 - (100 * (87320/88724))
100 - (100 * (88298/90327))

100 - (100 * (87268/89652))
100 - (100 * (86552/88724))
100 - (100 * (88205/90327))


# Peak Size:
avg_summary_outputs |>
  mutate(coverage = round(coverage, 2)) |>
  filter(coverage == 0.1, efficacy == 0.6) |>
  select(archetype, coverage_type, coverage, efficacy, mean_final_size, min_final_size, max_final_size)


#--------------------------------------------------------------------------------------------------#
