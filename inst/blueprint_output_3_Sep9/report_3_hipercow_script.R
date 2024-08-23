#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+++++ Helios hipercow development script +++#
#++++++++++++++++++++++++++++++++++++++++++++#

# Load packages:
library(hipercow)
library(orderly2)
library(tidyverse)
library(stringr)
library(sf)
library(patchwork)

#----- 1) Cluster set up ---------------------------------------------------------------------------

# Set working directtory:
setwd("./inst/blueprint_output_3_Sep9/")

## Prepare for cluster use
## see https://mrc-ide.github.io/hipercow/
hipercow::hipercow_init(driver = 'windows')

# User-authentication process:
hipercow::windows_authenticate()

## Provision packages required on the cluster (hipercow looks for provision.R by default)
## see https://mrc-ide.github.io/hipercow/articles/packages.html
hipercow::hipercow_provision()

# Check the configuration:
hipercow::hipercow_configuration()
