#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+++++ Blueprint Report 2: Archetype Re-Alignment +++++#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++#

#+++ README +++#

##' In this script, we check that the beta values estimated to produce R0's reflective of the pathogen
##' archetypes we calculated prior to the USA data integration development. Specifically, we check that
##' the beta values for influenza and SARS-CoV-2 (measles will be done later)

#' The R)'s for the archetypes are as follows:
#' Flu:         1.5
#' SARS-CoV-2:  2.5
#' Measles:     9


1.5 for flu, 2.5 for sars_cov_2, and 9 for measles

#----- 1) Preamble ---------------------------------------------------------------------------------

# Load the finalsize package:
library(finalsize)
library(tidyverse)
library(grid)
library(gridExtra)

#----- 2) Parameterise the endemic model exemplar runs for the 3:3:3:3:1 beta ratios ---------------
