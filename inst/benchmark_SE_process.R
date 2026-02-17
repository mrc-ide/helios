#!/usr/bin/env Rscript
#
# Benchmark: NEW (IntegerVariable + tabulate-gather SE process)
#        vs  OLD (CategoricalVariable + loop-based SE process)
#
# This script compares the performance of the full optimised pipeline
# (IntegerVariable variables + vectorised tabulate-gather SE process) against
# the original main-branch implementation (CategoricalVariable variables +
# loop-based SE process with bitset_count_and).
#
# Part 1: Memory comparison across population sizes (25k, 50k, 100k)
# Part 2: Timing comparison at 100k population for 365 timesteps
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
MEMORY_POP_SIZES <- c(25000, 50000, 100000)
SIM_TIME <- 365
DT <- 1 # 1 day per timestep => 365 timesteps
SEED <- 42
NUM_INITIAL_E <- 5

# ============================================================================
# Memory measurement helper
# ============================================================================
# Read process RSS from /proc/self/status (Linux).
# Falls back to gc()-based estimate on other platforms.
get_rss_mb <- function() {
  proc_file <- "/proc/self/status"
  if (file.exists(proc_file)) {
    status <- readLines(proc_file, warn = FALSE)
    vmrss_line <- grep("^VmRSS:", status, value = TRUE)
    if (length(vmrss_line) == 1) {
      return(as.numeric(gsub("[^0-9]", "", vmrss_line)) / 1024) # kB -> MB
    }
  }
  # Fallback: sum of R heap memory (won't capture C++ bitset allocations)
  gc_info <- gc()
  sum(gc_info[, 2])
}

# ============================================================================
# convert_to_categorical: convert IntegerVariable-based variables_list back
# to CategoricalVariable-based (reproducing main-branch variable types)
# ============================================================================
convert_to_categorical <- function(variables_list, parameters_list) {
  N <- parameters_list$human_population

  # Household: IntegerVariable -> CategoricalVariable
  hh_vals <- variables_list$household$get_values()
  num_hh <- max(hh_vals)
  variables_list$household <- individual::CategoricalVariable$new(
    categories = sprintf("%d", 1:num_hh),
    initial_values = sprintf("%d", hh_vals)
  )

  # School: IntegerVariable -> CategoricalVariable (includes "0" for unassigned)
  sc_vals <- variables_list$school$get_values()
  num_sc <- max(sc_vals)
  variables_list$school <- individual::CategoricalVariable$new(
    categories = as.character(0:num_sc),
    initial_values = as.character(sc_vals)
  )

  # Workplace: IntegerVariable -> CategoricalVariable (includes "0" for unassigned)
  wp_vals <- variables_list$workplace$get_values()
  num_wp <- max(wp_vals)
  variables_list$workplace <- individual::CategoricalVariable$new(
    categories = as.character(0:num_wp),
    initial_values = as.character(wp_vals)
  )

  # Restore specific_leisure CategoricalVariable (removed from create_variables)
  assigned_leisure <- sort(parameters_list$leisure_indices)
  variables_list$specific_leisure <- individual::CategoricalVariable$new(
    categories = as.character(assigned_leisure),
    initial_values = rep(as.character(0), N)
  )

  variables_list
}

# ============================================================================
# Original (loop-based) create_SE_process — verbatim from master branch
# Uses CategoricalVariable API: $get_categories(), $get_index_of(as.character()),
# specific_leisure$initialize(), bitset_count_and()
# ============================================================================
create_SE_process_original <- function(
  variables_list,
  events_list,
  parameters_list,
  renderer
) {
  ## Pre-calculating the things that only have to be calculated once

  ##### HOUSEHOLDS #####
  num_households <- max(as.numeric(variables_list$household$get_categories()))
  household_bitset_list <- vector(mode = "list", length = num_households)
  household_index_list <- vector(mode = "list", length = num_households)
  household_size_list <- vector(mode = "list", length = num_households)
  for (i in seq(num_households)) {
    household_bitset_list[[i]] <- variables_list$household$get_index_of(
      as.character(i)
    )
    household_index_list[[i]] <- household_bitset_list[[i]]$to_vector()
    household_size_list[[i]] <- length(household_index_list[[i]])
  }

  ##### WORKPLACES #####
  num_workplaces <- max(as.numeric(variables_list$workplace$get_categories()))
  workplace_bitset_list <- vector(mode = "list", length = num_workplaces)
  workplace_index_list <- vector(mode = "list", length = num_workplaces)
  workplace_size_list <- vector(mode = "list", length = num_workplaces)
  for (i in seq(num_workplaces)) {
    workplace_bitset_list[[i]] <- variables_list$workplace$get_index_of(
      as.character(i)
    )
    workplace_index_list[[i]] <- workplace_bitset_list[[i]]$to_vector()
    workplace_size_list[[i]] <- length(workplace_index_list[[i]])
  }

  ##### SCHOOLS #####
  num_schools <- max(as.numeric(variables_list$school$get_categories()))
  school_bitset_list <- vector(mode = "list", length = num_schools)
  school_index_list <- vector(mode = "list", length = num_schools)
  school_size_list <- vector(mode = "list", length = num_schools)
  for (i in seq(num_schools)) {
    school_bitset_list[[i]] <- variables_list$school$get_index_of(
      as.character(i)
    )
    school_index_list[[i]] <- school_bitset_list[[i]]$to_vector()
    school_size_list[[i]] <- length(school_index_list[[i]])
  }

  ##### LEISURE #####
  num_leisure <- length(parameters_list$setting_sizes$leisure)
  leisure_indvidual_possible_visits_list <- vector(
    mode = "list",
    length = parameters_list$human_population
  )
  for (i in seq(parameters_list$human_population)) {
    leisure_indvidual_possible_visits_list[[i]] <- unlist(
      variables_list$leisure$get_values(i)
    )
  }

  ## Process Function
  function(t) {
    I <- variables_list$disease_state$get_index_of("I")

    #=== Household FOI ===#
    household_FOI <- vector(
      mode = "numeric",
      length = parameters_list$human_population
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
      length = parameters_list$human_population
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
      length = parameters_list$human_population
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
    if ((t * parameters_list$dt) == floor((t * parameters_list$dt))) {
      leisure_visit <- vector(
        mode = "numeric",
        length = parameters_list$human_population
      )
      for (i in seq(parameters_list$human_population)) {
        leisure_visit[i] <- leisure_indvidual_possible_visits_list[[i]][
          dqrng::dqsample.int(n = 7, size = 1)
        ]
      }
      variables_list$specific_leisure$initialize(
        categories = as.character(parameters_list$leisure_indices),
        initial_values = as.character(leisure_visit)
      )
    }

    leisure_FOI <- vector(
      mode = "numeric",
      length = parameters_list$human_population
    )
    leisure_locations <- variables_list$specific_leisure$get_categories()
    leisure_locations <- leisure_locations[leisure_locations != "0"]
    for (i in 1:length(leisure_locations)) {
      spec_leisure_location <- as.numeric(leisure_locations[i])
      if (spec_leisure_location != 0) {
        spec_leisure <- variables_list$specific_leisure$get_index_of(
          as.character(spec_leisure_location)
        )
        spec_leisure_I_size <- individual:::bitset_count_and(I, spec_leisure)
        if (parameters_list$far_uvc_leisure) {
          if (parameters_list$uvc_leisure[i] == 1 &
                t > parameters_list$far_uvc_leisure_timestep) {
            spec_leisure_FOI <- parameters_list$leisure_specific_riskiness[i] *
              (1 - parameters_list$far_uvc_leisure_efficacy) *
              (parameters_list$beta_leisure * spec_leisure_I_size /
                 spec_leisure$size())
          } else {
            spec_leisure_FOI <- parameters_list$leisure_specific_riskiness[i] *
              parameters_list$beta_leisure * spec_leisure_I_size /
              spec_leisure$size()
          }
        } else {
          spec_leisure_FOI <- parameters_list$leisure_specific_riskiness[i] *
            parameters_list$beta_leisure * spec_leisure_I_size /
            spec_leisure$size()
        }
        leisure_FOI[spec_leisure$to_vector()] <- spec_leisure_FOI
      }
    }

    #=== Community FOI ===#
    community_FOI <- parameters_list$beta_community *
      variables_list$disease_state$get_size_of("I") /
      parameters_list$human_population

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
run_with_SE_process <- function(parameters_list, se_process_fn,
                                convert_vars_fn = NULL,
                                include_transition_processes = FALSE) {
  variables_list <- create_variables(parameters_list)
  parameters_list <- variables_list$parameters_list
  variables_list <- variables_list$variables_list

  # Optionally convert variables (e.g. back to CategoricalVariable for old benchmark)
  if (!is.null(convert_vars_fn)) {
    variables_list <- convert_vars_fn(variables_list, parameters_list)
  }

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
    )
  )

  # For the OLD benchmark: include the polling-based EI/IR processes.
  # The NEW version uses inline scheduling via event listeners instead.
  if (include_transition_processes) {
    processes_list$EI_process <- create_EI_process(
      variables_list = variables_list,
      events_list = events_list,
      parameters_list = parameters_list
    )
    processes_list$IR_process <- create_IR_process(
      variables_list = variables_list,
      events_list = events_list,
      parameters_list = parameters_list
    )
  }

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
# PART 1: Memory comparison across population sizes
# ============================================================================
cat("=== Part 1: Memory Comparison ===\n")
cat(sprintf("Population sizes: %s\n\n",
            paste(format(MEMORY_POP_SIZES, big.mark = ","), collapse = ", ")))

memory_results <- data.frame(
  pop_size = integer(),
  new_rss_mb = numeric(),
  old_rss_mb = numeric(),
  stringsAsFactors = FALSE
)

for (n in MEMORY_POP_SIZES) {
  cat(sprintf("  N = %s ...\n", format(n, big.mark = ",")))
  mem_params <- get_parameters(overrides = list(
    human_population = n,
    number_initial_S = n - NUM_INITIAL_E,
    number_initial_E = NUM_INITIAL_E,
    number_initial_I = 0,
    number_initial_R = 0,
    seed = SEED
  ))

  # --- NEW (IntegerVariable) ---
  invisible(gc(full = TRUE))
  baseline_new <- get_rss_mb()
  vars_new <- create_variables(mem_params)
  new_rss <- get_rss_mb()
  new_delta <- new_rss - baseline_new
  rm(vars_new)
  invisible(gc(full = TRUE))
  cat(sprintf("    NEW (IntegerVariable):    %7.1f MB (delta from baseline)\n", new_delta))

  # --- OLD (CategoricalVariable) ---
  invisible(gc(full = TRUE))
  baseline_old <- get_rss_mb()
  vars_old <- create_variables(mem_params)
  vars_old$variables_list <- convert_to_categorical(
    vars_old$variables_list, vars_old$parameters_list
  )
  old_rss <- get_rss_mb()
  old_delta <- old_rss - baseline_old
  rm(vars_old)
  invisible(gc(full = TRUE))
  cat(sprintf("    OLD (CategoricalVariable): %6.1f MB (delta from baseline)\n", old_delta))

  memory_results <- rbind(memory_results, data.frame(
    pop_size = n,
    new_rss_mb = new_delta,
    old_rss_mb = old_delta,
    stringsAsFactors = FALSE
  ))
}

# Memory summary table
cat("\n  --- Memory Summary ---\n")
cat(sprintf("  %-12s  %12s  %12s  %10s\n",
            "Pop. size", "NEW (MB)", "OLD (MB)", "Ratio"))
for (i in seq_len(nrow(memory_results))) {
  r <- memory_results[i, ]
  ratio <- if (r$new_rss_mb > 0) r$old_rss_mb / r$new_rss_mb else NA
  cat(sprintf("  %-12s  %12.1f  %12.1f  %9.1fx\n",
              format(r$pop_size, big.mark = ","),
              r$new_rss_mb, r$old_rss_mb, ratio))
}

# Check scaling: ratio of memory increase vs ratio of population increase
if (nrow(memory_results) >= 2) {
  cat("\n  --- Scaling Analysis ---\n")
  cat("  (If OLD scales ~quadratically, doubling N should ~4x memory;\n")
  cat("   if NEW scales ~linearly, doubling N should ~2x memory.)\n\n")
  for (i in 2:nrow(memory_results)) {
    n_ratio <- memory_results$pop_size[i] / memory_results$pop_size[i - 1]
    old_mem_ratio <- memory_results$old_rss_mb[i] / memory_results$old_rss_mb[i - 1]
    new_mem_ratio <- memory_results$new_rss_mb[i] / memory_results$new_rss_mb[i - 1]
    cat(sprintf("  N: %s -> %s (%.1fx pop):\n",
                format(memory_results$pop_size[i - 1], big.mark = ","),
                format(memory_results$pop_size[i], big.mark = ","),
                n_ratio))
    cat(sprintf("    OLD memory: %.1fx    NEW memory: %.1fx\n",
                old_mem_ratio, new_mem_ratio))
  }
}
cat("\n")

# ============================================================================
# PART 2: Timing comparison at full population size
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

cat("=== Part 2: Timing Comparison ===\n")
cat(sprintf("Population: %s | Timesteps: %d | dt: %s | Seed: %d\n\n",
            format(POP_SIZE, big.mark = ","), round(SIM_TIME / DT), DT, SEED))

# --- Run NEW version (inline scheduling via listeners, no EI/IR/RS processes) ---
cat("--- Running NEW (IntegerVariable + tabulate-gather + inline scheduling) ---\n")
t_new <- system.time({
  output_new <- run_with_SE_process(params, create_SE_process)
})
cat(sprintf("  Elapsed: %.1f seconds\n\n", t_new["elapsed"]))

# --- Run OLD version (polling-based EI/IR processes) ---
cat("--- Running OLD (CategoricalVariable + loop-based + polling processes) ---\n")
t_old <- system.time({
  output_old <- run_with_SE_process(
    params,
    create_SE_process_original,
    convert_vars_fn = convert_to_categorical,
    include_transition_processes = TRUE
  )
})
cat(sprintf("  Elapsed: %.1f seconds\n\n", t_old["elapsed"]))

# Timing summary
speedup <- t_old["elapsed"] / t_new["elapsed"]
cat("  --- Timing Summary ---\n")
cat(sprintf("  OLD (CategoricalVariable + loop):       %7.1f s\n", t_old["elapsed"]))
cat(sprintf("  NEW (IntegerVariable + tabulate):       %7.1f s\n", t_new["elapsed"]))
cat(sprintf("  Speedup:                                %7.1fx\n\n", speedup))

# ============================================================================
# PART 3: Output comparison (epidemic dynamics)
# ============================================================================
cat("=== Part 3: Epidemic Output Comparison ===\n")
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
# Save all results
# ============================================================================
results <- list(
  memory = memory_results,
  timing = data.frame(
    version = c("old_categorical_loop", "new_integer_tabulate"),
    elapsed_s = c(t_old["elapsed"], t_new["elapsed"]),
    user_s = c(t_old["user.self"], t_new["user.self"]),
    system_s = c(t_old["sys.self"], t_new["sys.self"])
  ),
  output_new = output_new,
  output_old = output_old,
  params = list(
    pop_size = POP_SIZE,
    memory_pop_sizes = MEMORY_POP_SIZES,
    sim_time = SIM_TIME,
    dt = DT,
    seed = SEED
  )
)

output_path <- file.path(tempdir(), "SE_process_benchmark_results.rds")
saveRDS(results, output_path)
cat(sprintf("\nFull results saved to: %s\n", output_path))

cat("\n=== Benchmark complete ===\n")
