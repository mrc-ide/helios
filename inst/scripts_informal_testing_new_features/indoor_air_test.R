# QUESTIONS
### How to handle contact patterns between age-groups?
### How to handle variable contact rates between age-groups?
### What are elderly doing when not going to school or to workplaces?

# POSSIBLE EFFICIENCY GAINS
## one trick might be to calculate the FOI only for individuals not already infected?
## only calculate household FOI for HHs with size > 1 only
## pre-calculate adults_subset, child_subset, hh_subset etc (i.e. all the static quantities) ahead of simulation (as they're constant and won't change)

# Loading required libraries
library(individual); library(tictoc)

## Setting the seed
set.seed(5000)

## Parameters
initial_infections <- 1000
N <- 50000
age_group_proportions <- c(0.2, 0.6, 0.2)
beta_workplace <- 0.5
beta_school <- 0.5
beta_household <- 0.5
beta_community <- 0.5
dt <- 0.5

## Defining categorical variable inputs

### Disease states
health_var <- CategoricalVariable$new(categories = c("S", "E", "I", "R"),
                                      initial_values = c(rep("S", N - initial_infections),
                                                         rep("E", initial_infections)))

### Age categories
age_var <- CategoricalVariable$new(categories = c("child", "adult", "elderly"),
                                  initial_values = sample(c("child", "adult", "elderly"), size = N,
                                                            replace = TRUE, prob = age_group_proportions))

### Workplaces
num_adults <- age_var$get_size_of("adult") # get number of adults
index_adults <- age_var$get_index_of("adult")$to_vector() # get the index of adults in age_var
workplaces <- vector(mode = "character", length = N) # create an empty vector to be filled with workplace assignments
workplace <- sample(as.character(1:10), size = length(index_adults), replace = TRUE, prob = NULL) # randomly sample workplaces for each adult
workplaces[index_adults] <- workplace # append workplace assignments to main workplaces vector
workplaces[workplaces == ""] <- "0" # replace blanks with 0s (these are children/elderly people)
workplace_var <- CategoricalVariable$new(categories = as.character(0:10), initial_values = workplaces) # create categorical variable

### Schools
num_child <- age_var$get_size_of("child") # get number of children
index_child <- age_var$get_index_of("child")$to_vector() # get the index of children in age_var
schools <- vector(mode = "character", length = N) # create empty vector to be filled with school assignments
school <- sample(as.character(1:10), size = length(index_child), replace = TRUE, prob = NULL) # randomly sample schools for each child
schools[index_child] <- school # append school assignment to main schools vector
schools[schools == ""] <- "0" # replace blanks with 0s (these are adults/elderly people)
schools_var <- CategoricalVariable$new(categories = as.character(0:10), initial_values = schools) # create categorical variable

### Households
households <- vector(mode = "character", length = N) # create empty vector to be filled with household assignments
household <- sample(as.character(1:1000), size = N, replace = TRUE, prob = NULL) # randomly sample households for all individuals
households_var <- CategoricalVariable$new(categories = as.character(1:1000), initial_values = household) # create categorical variable


## Defining exposure process
### NOTE: Really only trying to illustrate general structure we'll use here - lots of things
###       missing/still to add (e.g. leisure settings, location-specific parameters to modify FOI etc)
exposure_process <- function(t){

  ### Getting all infectious individuals
  I <- health_var$get_index_of("I")

  ### Calculate Household FOI (real-valued for all individuals; if HH size = 1, then 0)
  household_FOI <- vector(mode = "numeric", length = N)
  for (i in 1:1000) {
    spec_household <- households_var$get_index_of(as.character(i))
    spec_household_I <- I$and(spec_household)
    spec_household_FOI <- beta_household * spec_household_I$size() / spec_household$size() ## this calculation needs more in it
    household_FOI[spec_household$to_vector()] <- spec_household_FOI
  }

  ### Calculate Workplace FOI (real-valued for adults; 0 otherwise)
  workplace_FOI <- vector(mode = "numeric", length = N)
  adults_subset <- age_var$get_index_of("adult")
  workplace_adults_only_var <- workplace_var$get_index_of(as.character(1:10))
  for (i in 1:10) {
    spec_workplace <- workplace_var$get_index_of(as.character(i))
    spec_workplace_I <- I$and(spec_workplace)
    spec_workplace_FOI <- beta_workplace * spec_workplace_I$size() / spec_workplace$size() ## this calculation needs more in it
    workplace_FOI[spec_workplace$to_vector()] <- spec_workplace_FOI
  }

  ### Calculate School FOI (real-valued for children; 0 otherwise)
  school_FOI <- vector(mode = "numeric", length = N)
  child_subset <- age_var$get_index_of("child")
  school_child_only_var <- schools_var$get_index_of(as.character(1:10))
  for (i in 1:10) {
    spec_school <- schools_var$get_index_of(as.character(i))
    spec_school_I <- I$and(spec_school)
    spec_school_FOI <- beta_school * spec_school_I$size() / spec_school$size() ## this calculation needs more in it
    school_FOI[spec_school$to_vector()] <- spec_school_FOI
  }

  ### Calculate Leisure FOI (real-valued for all individuals)
  ### NOT DONE YET
  leisure_FOI <- 0

  ### Calculate Community FOI (real-valued for all individuals)
  #### NOTE: Double check whether the "/N" is correct here - not sure currently
  community_FOI <- beta_community * health_var$get_size_of("I") / N

  ## Combining all the FOIs together and then sampling whether or not the individual is infected
  total_FOI <- household_FOI + workplace_FOI + school_FOI + leisure_FOI + community_FOI
  p_inf <- 1 - exp(-total_FOI * dt)
  S <- health_var$get_index_of("S")
  p_inf_currently_S <- p_inf[S$to_vector()]
  S$sample(rate = p_inf_currently_S)
  health_var$queue_update(value = "E",index = S)
}

## Define event and process to schedule moves from E->I
EI_event <- TargetedEvent$new(population_size = N)
EI_event$add_listener(function(t, target) {
  health_var$queue_update("I", target)
})
EI_process <- function(t){
  E <- health_var$get_index_of("E")
  EI_already_scheduled <- EI_event$get_scheduled()
  E$and(EI_already_scheduled$not(inplace = TRUE))
  I_times <- round((rgamma(E$size(), 4, 2) + 1) / dt)
  EI_event$schedule(target = E, delay = I_times)
}

## Define event and process to schedule moves from I->R
IR_event <- TargetedEvent$new(population_size = N)
IR_event$add_listener(function(t, target) {
  health_var$queue_update("R", target)
})
IR_process <- function(t){
  I <- health_var$get_index_of("I")
  IR_already_scheduled <- IR_event$get_scheduled()
  I$and(IR_already_scheduled$not(inplace = TRUE))
  R_times <- round((rgamma(I$size(), 4, 1) + 1) / dt)
  IR_event$schedule(target = I, delay = R_times)
}


## Define render
health_render <- Render$new(timesteps = 150 / dt)
health_render_process <- categorical_count_renderer_process(
  renderer = health_render,
  variable = health_var,
  categories =  c("S", "E", "I", "R")
)

## Run simulation loop
tic()
final_state <- simulation_loop(
  variables = list(health_var,
                   age_var,
                   households_var,
                   workplace_var,
                   schools_var),
  events = list(EI_event, IR_event),
  processes = list(exposure_process, EI_process, IR_process, health_render_process),
  timesteps = 150 / dt,
  restore_random_state = FALSE)
toc()

states <- health_render$to_dataframe()
health_cols <-  c("royalblue3","firebrick3","darkorchid3", "orange")
matplot(
  x = states[[1]]*dt, y = states[-1],
  type="l",lwd=2,lty = 1,col = adjustcolor(col = health_cols, alpha.f = 0.85),
  xlab = "Time",ylab = "Count"
)
legend(
  x = "topright",pch = rep(16,3),
  col = health_cols,bg = "transparent",
  legend = c("S", "E", "I", "R"), cex = 1.5
)
