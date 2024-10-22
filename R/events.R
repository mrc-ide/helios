#' Generate a list of model events with listeners
#'
#' Listeners are arbitrary functions called when the event is fired. Their only
#' argument is the current time step.
#'
#' @param variables_list A list of the model variables as generated by [create_variables()]
#' @param parameters_list A list of model parameters as generated by [get_parameters()]
#'
#' @family events
#' @export
create_events <- function(variables_list, parameters_list) {

  # Open a list to store the model events and populate using the event generator functions:
  events_list <- list(

    # Event moving exposed individuals to the infectious state
    EI_event = individual::TargetedEvent$new(population_size = parameters_list$human_population),

    # Event moving exposed individuals to the infectious state
    IR_event = individual::TargetedEvent$new(population_size = parameters_list$human_population)

  )

  # Add listener to the EI event:
  events_list$EI_event$add_listener(
    function(t, target) {
      variables_list$disease_state$queue_update("I", target)
    }
  )

  # Add listener to the IR event:
  events_list$IR_event$add_listener(
    function(t, target) {
      variables_list$disease_state$queue_update("R", target)
    }
  )

  # Add RS_event and listener if endemic pathogen is required (i.e. individuals going R->S)
  if (parameters_list$endemic_or_epidemic == "endemic") {

    RS_event <- individual::TargetedEvent$new(population_size = parameters_list$human_population)
    events_list <- c(events_list, list(RS_event = RS_event))

    events_list$RS_event$add_listener(
      function(t, target) {
        variables_list$disease_state$queue_update("S", target)
      }
    )

  }

  # Return the list of model events:
  events_list
}
