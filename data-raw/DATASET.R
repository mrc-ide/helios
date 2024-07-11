# Make any changes to the saved data for the package here
library(readr)

baseline_household_demographics_uk <- read_csv("data-raw/Hinch_et_al_baseline_household_demographics.csv")
baseline_household_demographics_uk$child <- baseline_household_demographics_uk$a_0_9 + baseline_household_demographics_uk$a_10_19
baseline_household_demographics_uk$adult <- baseline_household_demographics_uk$a_20_29 + baseline_household_demographics_uk$a_30_39 +
                                            baseline_household_demographics_uk$a_40_49 + baseline_household_demographics_uk$a_50_59 +
                                            baseline_household_demographics_uk$a_60_69
baseline_household_demographics_uk$elderly <- baseline_household_demographics_uk$a_70_79 + baseline_household_demographics_uk$a_80
baseline_household_demographics_uk <- baseline_household_demographics_uk[, c("child", "adult", "elderly")]
usethis::use_data(baseline_household_demographics_uk, overwrite = TRUE)

schools_england <- read_csv("data-raw/spc_school_level_underlying_data_23112023.csv")
usethis::use_data(schools_england)

baseline_household_demographics_usa <- read.delim("data-raw/sanFrancisco_RTIsynthPop_ageHH_data.txt") |>
  dplyr::select(sp_id, sp_hh_id, age) |>
  dplyr::rename(person_id = sp_id, household_id = sp_hh_id, age = age) |>
  dplyr::group_by(household_id) |>
  dplyr::summarise(child = sum(age <= 18),
                   adult = sum(age > 18 & age <= 69),
                   elderly = sum(age >= 70),
                   household_size = child + adult + elderly) |>
  dplyr::select(child, adult, elderly)
usethis::use_data(baseline_household_demographics_usa, overwrite = TRUE)
