# Make any changes to the saved data for the package here

library(readr)

baseline_household_demographics <- read_csv("data-raw/Hinch_et_al_baseline_household_demographics.csv")
usethis::use_data(baseline_household_demographics)

schools_england <- read_csv("data-raw/spc_school_level_underlying_data_23112023.csv")
usethis::use_data(schools_england)
