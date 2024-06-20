# Make any changes to the saved data for the package here
library(readr)

baseline_household_demographics <- read_csv("data-raw/Hinch_et_al_baseline_household_demographics.csv")
usethis::use_data(baseline_household_demographics)

schools_england <- read_csv("data-raw/spc_school_level_underlying_data_23112023.csv")
usethis::use_data(schools_england)

baseline_household_demographics_usa <- read.delim("data-raw/sanFrancisco_RTIsynthPop_ageHH_data.txt") |>
  dplyr::select(sp_id, sp_hh_id, age) |>
  dplyr::rename(person_id = sp_id, household_id = sp_hh_id, age = age) |>
  dplyr::group_by(household_id) |>
  dplyr::summarise(child = sum(age <= 18),
                   adult = sum(age > 18 & age <= 69),
                   elderly = sum(age >= 70),
                   household_size = child + adult + elderly)
usethis::use_data(baseline_household_demographics_usa)
