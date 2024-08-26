
# Also use the ncov-ic universe
repo::https://ncov-ic.r-universe.dev

# Specific version of individual, via a tag
mrc-ide/individual

# Local version of helios
# local::./helios_0.1.0.tar.gz

# Specific version of malariasimulation
mrc-ide/site@site2
mrc-ide/postie@postie2
mrc-ide/scene@scene2
# mrc-ide/netz@netz2
mrc-ide/malariasimulation@dev
sf

###################################################
## Loading hipercow
library(hipercow); library(pkgdepends); library(individual); library(malariasimulation); library(sf)

## Set working directory to network path
setwd("Q:/")

## Initialising and configuring
hipercow_init(root = "Q:/")
hipercow_configure(driver = "windows")
windows_authenticate()

## Having created a pkgdepends.txt file in root of hipercow directory (see: https://mrc-ide.github.io/hipercow/articles/packages.html#a-list-of-packages)
hipercow_provision(method = "pkgdepends") # note we're getting some weird dependency issues that might mean netz isn't netz2

## Loading in file and setting up the hipercow environment
?hipercow_cluster_info()
site_file <- readRDS(file = "C:/Users/cw1716/Documents/Q_Drive_Copy2/Active_Research_Projects/estimating_stephensi_density/data/tester_malariasimulation_siteFiles/ETH_calibrated_scaled_site.rds")
hipercow_environment_create(packages = c("individual", "malariasimulation", "site", "postie", "scene", "netz", "sf"))
# globals = "site_file") # don't forget to tell Rich and Wes this doesn't work
hipercow_configuration()

## Running on the cluster in parallel
options(hipercow.max_size_local = 30e6)
id5 <- hipercow::task_create_explicit(
  expr = quote({
    parallel::clusterExport(NULL, c("site_file"))
    parallel::clusterApply(NULL, 1:2, function(x) {
      sub_site <- site::subset_site(site_file, site_file$sites[1, ])
      p <- site::site_parameters(
        interventions = sub_site$interventions,
        demography = sub_site$demography,
        vectors = sub_site$vectors$vector_species,
        seasonality = sub_site$seasonality$seasonality_parameters,
        overrides = list(human_population = 10000),
        species = "pf",
        burnin = 5,
        eir = sub_site$eir$eir[sub_site$eir$sp == "pf"])
      malariasimulation::run_simulation(p$timesteps, p)
    })
  }),
  export = "site_file",
  resources = hipercow_resources(cores = 2),
  parallel = hipercow_parallel("parallel")
)
task_status(id5)
task_result(id5)
