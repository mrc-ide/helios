#!/usr/bin/env Rscript
#
# Benchmark: tabulate-gather SE process vs original loop-based SE process
#
# This script compares the performance of the refactored create_SE_process()
# (using vectorised tabulate-gather pattern) against the original loop-based
# implementation. It runs both versions on a 100k population for 365 timesteps
# and reports timing, speedup, and a basic comparison of epidemic outputs.
#
# Usage:
#   Rscript inst/benchmark_SE_process.R
#

library(helios)
library(individual)

# ============================================================================
# Configuration
# ============================================================================
POP_SIZE <- 100000
SIM_TIME <- 365
DT <- 1 # 1 day per timestep => 365 timesteps
SEED <- 42
NUM_INITIAL_E <- 5

cat("=== SE Process Benchmark ===\n")
cat(sprintf("Population: %s | Timesteps: %d | dt: %s | Seed: %d\n\n",
            format(POP_SIZE, big.mark = ","), round(SIM_TIME / DT), DT, SEED))

# ============================================================================
# Parameters (shared between both runs)
# ============================================================================
params <- get_parameters(overrides = list(
  human_population = POP_SIZE,
  number_initial_S = POP_SIZE - NUM_INITIAL_E,
  number_initial_E = NUM_INITIAL_E,
  number_initial_I = 0,
  number_initial_R = 0,
  simulation_time = SIM_TIME,
  dt = DT,
  seed = SEED
))

# ============================================================================
# Original (loop-based) create_SE_process — preserved verbatim for comparison
# ============================================================================
create_SE_process_original <- function(
  variables_list,
  events_list,
  parameters_list,
  renderer
) {
  ## Pre-calculating the things that only have to be calculated once
  N <- parameters_list$human_population

  ##### HOUSEHOLDS #####
  # Uses IntegerVariable: $get_index_of(set = i) returns a Bitset
  num_households <- parameters_list$num_households
  household_bitset_list <- vector(mode = "list", length = num_households)
  household_index_list <- vector(mode = "list", length = num_households)
  household_size_list <- vector(mode = "list", length = num_households)
  for (i in seq(num_households)) {
    household_bitset_list[[i]] <- variables_list$household$get_index_of(
      set = i
    )
    household_index_list[[i]] <- household_bitset_list[[i]]$to_vector()
    household_size_list[[i]] <- length(household_index_list[[i]])
  }

  ##### WORKPLACES #####
  num_workplaces <- parameters_list$num_workplaces
  workplace_bitset_list <- vector(mode = "list", length = num_workplaces)
  workplace_index_list <- vector(mode = "list", length = num_workplaces)
  workplace_size_list <- vector(mode = "list", length = num_workplaces)
  for (i in seq(num_workplaces)) {
    workplace_bitset_list[[i]] <- variables_list$workplace$get_index_of(
      set = i
    )
    workplace_index_list[[i]] <- workplace_bitset_list[[i]]$to_vector()
    workplace_size_list[[i]] <- length(workplace_index_list[[i]])
  }

  ##### SCHOOLS #####
  num_schools <- parameters_list$num_schools
  school_bitset_list <- vector(mode = "list", length = num_schools)
  school_index_list <- vector(mode = "list", length = num_schools)
  school_size_list <- vector(mode = "list", length = num_schools)
  for (i in seq(num_schools)) {
    school_bitset_list[[i]] <- variables_list$school$get_index_of(
      set = i
    )
    school_index_list[[i]] <- school_bitset_list[[i]]$to_vector()
    school_size_list[[i]] <- length(school_index_list[[i]])
  }

  ##### LEISURE #####
  # Build per-individual possible visits list from RaggedInteger (unchanged)
  leisure_indvidual_possible_visits_list <- vector(
    mode = "list",
    length = N
  )
  for (i in seq(N)) {
    leisure_indvidual_possible_visits_list[[i]] <- unlist(
      variables_list$leisure$get_values(i)
    )
  }

  # Leisure location metadata for loop-based FOI computation
  actual_leisure_ids <- sort(
    parameters_list$leisure_indices[parameters_list$leisure_indices > 0]
  )
  num_leisure <- length(actual_leisure_ids)
  max_leisure_id <- max(actual_leisure_ids)

  # Build lookup from leisure index position to actual ID
  leisure_id_to_pos <- integer(max_leisure_id)
  leisure_id_to_pos[actual_leisure_ids] <- seq_along(actual_leisure_ids)

  # Plain integer vector for today's leisure assignment (replaces specific_leisure CategoricalVariable)
  leisure_today <- integer(N)

  ## Process Function
  function(t) {
    I <- variables_list$disease_state$get_index_of("I")
    I_vec <- I$to_vector()

    #=== Household FOI ===#
    household_FOI <- vector(
      mode = "numeric",
      length = N
    )
    for (i in seq(num_households)) {
      if (household_size_list[[i]] > 1) {
        spec_household_I_size <- individual:::bitset_count_and(
          I, household_bitset_list[[i]]
        )
        if (parameters_list$far_uvc_household) {
          if (parameters_list$uvc_household[i] == 1 &
                t > parameters_list$far_uvc_household_timestep) {
            spec_household_FOI <- parameters_list$household_specific_riskiness[i] *
              (1 - parameters_list$far_uvc_household_efficacy) *
              (parameters_list$beta_household * spec_household_I_size /
                 household_size_list[[i]])
          } else {
            spec_household_FOI <- parameters_list$household_specific_riskiness[i] *
              parameters_list$beta_household * spec_household_I_size /
              household_size_list[[i]]
          }
        } else {
          spec_household_FOI <- parameters_list$household_specific_riskiness[i] *
            parameters_list$beta_household * spec_household_I_size /
            household_size_list[[i]]
        }
        household_FOI[household_index_list[[i]]] <- spec_household_FOI
      }
    }

    #=== Workplace FOI ===#
    workplace_FOI <- vector(
      mode = "numeric",
      length = N
    )
    for (i in seq(num_workplaces)) {
      spec_workplace_I_size <- individual:::bitset_count_and(
        I, workplace_bitset_list[[i]]
      )
      if (parameters_list$far_uvc_workplace) {
        if (parameters_list$uvc_workplace[i] == 1 &
              t > parameters_list$far_uvc_workplace_timestep) {
          spec_workplace_FOI <- parameters_list$workplace_specific_riskiness[i] *
            (1 - parameters_list$far_uvc_workplace_efficacy) *
            (parameters_list$beta_workplace * spec_workplace_I_size /
               workplace_size_list[[i]])
        } else {
          spec_workplace_FOI <- parameters_list$workplace_specific_riskiness[i] *
            parameters_list$beta_workplace * spec_workplace_I_size /
            workplace_size_list[[i]]
        }
      } else {
        spec_workplace_FOI <- parameters_list$workplace_specific_riskiness[i] *
          parameters_list$beta_workplace * spec_workplace_I_size /
          workplace_size_list[[i]]
      }
      workplace_FOI[workplace_index_list[[i]]] <- spec_workplace_FOI
    }

    #=== School FOI ===#
    school_FOI <- vector(
      mode = "numeric",
      length = N
    )
    for (i in seq(num_schools)) {
      spec_school_I_size <- individual:::bitset_count_and(
        I, school_bitset_list[[i]]
      )
      if (parameters_list$far_uvc_school) {
        if (parameters_list$uvc_school[i] == 1 &
              t > parameters_list$far_uvc_school_timestep) {
          spec_school_FOI <- parameters_list$school_specific_riskiness[i] *
            (1 - parameters_list$far_uvc_school_efficacy) *
            (parameters_list$beta_school * spec_school_I_size /
               school_size_list[[i]])
        } else {
          spec_school_FOI <- parameters_list$school_specific_riskiness[i] *
            parameters_list$beta_school * spec_school_I_size /
            school_size_list[[i]]
        }
      } else {
        spec_school_FOI <- parameters_list$school_specific_riskiness[i] *
          parameters_list$beta_school * spec_school_I_size /
          school_size_list[[i]]
      }
      school_FOI[school_index_list[[i]]] <- spec_school_FOI
    }

    #=== Leisure FOI ===#
    # Daily reassignment using per-individual loop (original approach)
    if ((t * parameters_list$dt) == floor((t * parameters_list$dt))) {
      for (i in seq(N)) {
        leisure_today[i] <<- leisure_indvidual_possible_visits_list[[i]][
          dqrng::dqsample.int(n = 7, size = 1)
        ]
      }
    }

    # Loop over active leisure locations and compute FOI per venue
    leisure_FOI <- vector(
      mode = "numeric",
      length = N
    )
    for (j in seq_along(actual_leisure_ids)) {
      lid <- actual_leisure_ids[j]
      visitors <- which(leisure_today == lid)
      num_visitors <- length(visitors)
      if (num_visitors > 0) {
        # Count infected among visitors
        spec_leisure_I_size <- sum(leisure_today[I_vec] == lid)
        if (parameters_list$far_uvc_leisure) {
          if (parameters_list$uvc_leisure[j] == 1 &
                t > parameters_list$far_uvc_leisure_timestep) {
            spec_leisure_FOI <- parameters_list$leisure_specific_riskiness[j] *
              (1 - parameters_list$far_uvc_leisure_efficacy) *
              (parameters_list$beta_leisure * spec_leisure_I_size /
                 num_visitors)
          } else {
            spec_leisure_FOI <- parameters_list$leisure_specific_riskiness[j] *
              parameters_list$beta_leisure * spec_leisure_I_size /
              num_visitors
          }
        } else {
          spec_leisure_FOI <- parameters_list$leisure_specific_riskiness[j] *
            parameters_list$beta_leisure * spec_leisure_I_size /
            num_visitors
        }
        leisure_FOI[visitors] <- spec_leisure_FOI
      }
    }

    #=== Community FOI ===#
    community_FOI <- parameters_list$beta_community *
      length(I_vec) / N

    #=== Total FOI ===#
    total_FOI <- household_FOI + workplace_FOI + school_FOI +
      leisure_FOI + community_FOI

    if (parameters_list$render_diagnostics) {
      renderer$render('FOI_household', max(household_FOI), t)
      renderer$render('FOI_workplace', max(workplace_FOI), t)
      renderer$render('FOI_school', max(school_FOI), t)
      renderer$render('FOI_leisure', max(leisure_FOI), t)
      renderer$render('FOI_community', max(community_FOI), t)
      renderer$render('FOI_total', max(total_FOI), t)
    }

    p_inf <- 1 - exp(-total_FOI * parameters_list$dt)
    S <- variables_list$disease_state$get_index_of("S")
    p_inf_currently_S <- p_inf[S$to_vector()]
    S$sample(rate = p_inf_currently_S)
    renderer$render('E_new', S$size(), t)
    variables_list$disease_state$queue_update(value = "E", index = S)
  }
}

# ============================================================================
# Helper: run simulation with a specified SE process constructor
# ============================================================================
run_with_SE_process <- function(parameters_list, se_process_fn) {
  variables_list <- create_variables(parameters_list)
  parameters_list <- variables_list$parameters_list
  variables_list <- variables_list$variables_list

  events_list <- create_events(
    variables_list = variables_list,
    parameters_list = parameters_list
  )

  timesteps <- round(parameters_list$simulation_time / parameters_list$dt)
  renderer <- individual::Render$new(timesteps)

  # Build process list manually to control which SE process is used
  processes_list <- list(
    SE_process = se_process_fn(
      variables_list = variables_list,
      events_list = events_list,
      parameters_list = parameters_list,
      renderer = renderer
    ),
    EI_process = create_EI_process(
      variables_list = variables_list,
      events_list = events_list,
      parameters_list = parameters_list
    ),
    IR_process = create_IR_process(
      variables_list = variables_list,
      events_list = events_list,
      parameters_list = parameters_list
    )
  )

  # Add rendering process
  processes_list <- c(
    processes_list,
    renderer = individual::categorical_count_renderer_process(
      renderer,
      variables_list$disease_state,
      c('S', 'E', 'I', 'R')
    )
  )

  individual::simulation_loop(
    variables = variables_list,
    events = unlist(events_list),
    processes = processes_list,
    timesteps = timesteps
  )

  renderer$to_dataframe()
}

# ============================================================================
# Run NEW (tabulate-gather) version
# ============================================================================
cat("--- Running NEW (tabulate-gather) version ---\n")
t_new <- system.time({
  output_new <- run_with_SE_process(params, create_SE_process)
})
cat(sprintf("  Elapsed: %.1f seconds\n\n", t_new["elapsed"]))

# ============================================================================
# Run OLD (loop-based) version
# ============================================================================
cat("--- Running OLD (loop-based) version ---\n")
t_old <- system.time({
  output_old <- run_with_SE_process(params, create_SE_process_original)
})
cat(sprintf("  Elapsed: %.1f seconds\n\n", t_old["elapsed"]))

# ============================================================================
# Timing comparison
# ============================================================================
speedup <- t_old["elapsed"] / t_new["elapsed"]
cat("=== Timing Results ===\n")
cat(sprintf("  OLD (loop-based):       %7.1f s\n", t_old["elapsed"]))
cat(sprintf("  NEW (tabulate-gather):  %7.1f s\n", t_new["elapsed"]))
cat(sprintf("  Speedup:                %7.1fx\n\n", speedup))

# ============================================================================
# Output comparison (epidemic dynamics)
# ============================================================================
cat("=== Epidemic Output Comparison ===\n")
cat("(Outputs differ due to different RNG sequences in leisure sampling,\n")
cat(" but epidemic dynamics should be qualitatively similar.)\n\n")

summarise_output <- function(df, label) {
  peak_I <- max(df$I_count)
  peak_t <- df$timestep[which.max(df$I_count)]
  total_E <- sum(df$E_new)
  final_R <- df$R_count[nrow(df)]
  attack_rate <- final_R / POP_SIZE * 100
  cat(sprintf("  %s:\n", label))
  cat(sprintf("    Peak infected:    %s (at timestep %d)\n",
              format(peak_I, big.mark = ","), peak_t))
  cat(sprintf("    Total exposures:  %s\n", format(total_E, big.mark = ",")))
  cat(sprintf("    Final recovered:  %s (%.1f%% attack rate)\n",
              format(final_R, big.mark = ","), attack_rate))
}

summarise_output(output_new, "NEW")
cat("\n")
summarise_output(output_old, "OLD")

# ============================================================================
# Optional: save outputs for further analysis
# ============================================================================
results <- list(
  timing = data.frame(
    version = c("old_loop", "new_tabulate"),
    elapsed_s = c(t_old["elapsed"], t_new["elapsed"]),
    user_s = c(t_old["user.self"], t_new["user.self"]),
    system_s = c(t_old["sys.self"], t_new["sys.self"])
  ),
  output_new = output_new,
  output_old = output_old,
  params = list(
    pop_size = POP_SIZE,
    sim_time = SIM_TIME,
    dt = DT,
    seed = SEED
  )
)

output_path <- file.path(tempdir(), "SE_process_benchmark_results.rds")
saveRDS(results, output_path)
cat(sprintf("\nFull results saved to: %s\n", output_path))

cat("\n=== Benchmark complete ===\n")
