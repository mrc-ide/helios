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
hipercow_configuration()
hipercow_provision_check()

## Testing helios running locally
parameters_list <- helios::get_parameters()
test_run_local <- helios::run_simulation(parameters_list = parameters_list, timesteps = 50)

## Testing helios running on cluster
hipercow_environment_create(packages = c("helios", "individual"))
id <- task_create_expr(helios::run_simulation(parameters_list = parameters_list, timesteps = 50))
task_status(id)
task_result(id)
task_info(id)
task_log_watch(id)

## Getting parallel version set up
resources <- hipercow_resources(cores = 5)
hipercow_environment_create(packages = c("helios", "individual"))

id2 <- task_create_call(
  fn = run_simulation,
  args = list(parameters_list_of_lists),
  parallel = hipercow_parallel("parallel"),
  resources = resources)
task_status(id2)
task_log_watch(id2)
task_result(id2)

## Create list of parameter lists
params <- helios::get_parameters()
list_params <- vector(mode = "list", length = 10)
for (i in 1:length(list_params)) {
  list_params[[i]] <- params
}

## Using task_create_exp and clusterApply
id <- task_create_expr(
  expr = parallel::clusterApply(cl = NULL, x = 1:10, fun = function(i) {
    helios::run_simulation(parameters_list = list_params[[i]], timesteps = 50)
  }),
  environment = "default",
  parallel = hipercow_parallel("parallel"),
  resources = hipercow_resources(cores = 5))
task_log_watch(id)

## Using task_create_explicit instead to try and export list_params
id2 <- task_create_explicit(
  expr = parallel::clusterApply(cl = NULL, x = 1:10, fun = function(i) {
    helios::run_simulation(parameters_list = list_params[[i]], timesteps = 50)
  }),
  export = "list_params",
  envir = parent.frame(),
  parallel = hipercow_parallel("parallel"),
  resources = hipercow_resources(cores = 5))
task_log_watch(id2)

## Create and submit task using parLapply
id3 <- task_create_expr(
  expr = parallel::parLapply(cl = NULL, X = list_params, fun = function(i) {
    helios::run_simulation(parameters_list = i, timesteps = 50)
  }),
  environment = "default",
  parallel = hipercow_parallel(method = "parallel", environment = "default"),
  resources =  hipercow_resources(cores = 5))
task_log_watch(id3)
