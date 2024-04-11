## Installing hipercow
install.packages("hipercow", repos = c("https://mrc-ide.r-universe.dev", "https://cloud.r-project.org"))

## Loading hipercow
library(hipercow); library(pkgdepends); library(helios); library(individual)

## Set working directory to network path
setwd("Q:/")

## Initialising and configuring
hipercow_init()
hipercow_configure(driver = "windows")
windows_authenticate()

## Check configuration
hipercow_configuration()

## Checking we can run an initial mini-task
hipercow_cluster_info()
id <- task_create_expr(sessionInfo())
id
task_status(id)
task_result(id)
task_cancel(id)

## Having created a pkgdepends.txt file in root of hipercow directory (see: https://mrc-ide.github.io/hipercow/articles/packages.html#a-list-of-packages)
## -> add in relevant packages that you need:
##    -> individual from github mrc-ide/individual@v0.1.15
##    -> helios locally - do devtools build to build .tar.gz version of helios, then move it to network path (Q drive for me) and put this in pkgdepends.txt local::./helios_0.1.0.tar.gz
hipercow_provision(method = "pkgdepends")
hipercow_provision_list()
hipercow_provision_check()
hipercow_provision_compare()
hipercow_resources()

## Testing helios running locally
parameters_list <- helios::get_parameters()
test_run_local <- helios::run_simulation(parameters_list = parameters_list, timesteps = 50)

## Testing helios running on cluster
id <- task_create_expr(helios::run_simulation(parameters_list = parameters_list, timesteps = 50))
task_info(id)$times
task_info(id)$data
task_log_show(id)
task_log_show(id, outer = TRUE)
task_log_watch(id)
task_status(id)
task_result(id)

## Getting parallel version set up
resources <- hipercow_resources(cores = 5)
id2 <- task_create_expr(
  parallel::clusterApply(NULL, 1:10, function(x) helios::run_simulation(parameters_list = parameters_list, timesteps = 50)),
  parallel = hipercow_parallel("parallel"),
  resources = resources)
task_status(id2)
task_result(id2)

resources <- hipercow_resources(cores = 5)
parameters_list_of_lists <- vector(mode = "list", length = 10)
for (i in 1:length(parameters_list_of_lists)) {
  parameters_list_of_lists[[i]] <- parameters_list
  parameters_list_of_lists[[i]]$population_size <- rpois(n = 1, lambda = 10000)
}
id3 <- task_create_expr(
  parallel::parLapply(cl = NULL, X = parameters_list_of_lists, fun = function(i) {
    helios::run_simulation(parameters_list = i, timesteps = 50)
  }),
  parallel = hipercow_parallel("parallel"),
  resources = resources)
task_status(id3)
task_result(id3)



