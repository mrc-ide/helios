# Many tests exercise create_variables()/run_simulation() without caring
# about ACH at all. Since this branch removed the silent ach = 1 default
# (get_parameters() now errors if a setting's ACH hasn't been configured),
# tests that don't care about ACH need a baseline wired in explicitly.
with_default_ach <- function(parameters_list, ach = 4) {
  for (setting in c("household", "workplace", "school", "leisure")) {
    parameters_list <- set_default_ach(parameters_list, setting, ach)
  }
  parameters_list
}
