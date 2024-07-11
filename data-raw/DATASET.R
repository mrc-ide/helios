# Make any changes to the saved data for the package here
library(readr)
library(readxl)
library(dplyr)
library(janitor)

baseline_household_demographics_uk <- read_csv("data-raw/Hinch_et_al_baseline_household_demographics.csv")
baseline_household_demographics_uk$child <- baseline_household_demographics_uk$a_0_9 + baseline_household_demographics_uk$a_10_19
baseline_household_demographics_uk$adult <- baseline_household_demographics_uk$a_20_29 + baseline_household_demographics_uk$a_30_39 +
                                            baseline_household_demographics_uk$a_40_49 + baseline_household_demographics_uk$a_50_59 +
                                            baseline_household_demographics_uk$a_60_69
baseline_household_demographics_uk$elderly <- baseline_household_demographics_uk$a_70_79 + baseline_household_demographics_uk$a_80
baseline_household_demographics_uk <- baseline_household_demographics_uk[, c("child", "adult", "elderly")]
usethis::use_data(baseline_household_demographics_uk, overwrite = TRUE)

schools_uk <- read_csv("data-raw/spc_school_level_underlying_data_23112023.csv")
usethis::use_data(schools_uk, overwrite = TRUE)

# Just the most recent years data (2019 / 2020)
schools_usa <- readxl::read_xls("data-raw/tabn216.40.xls") |>
  tibble::rowid_to_column("rowid") |>
  filter(rowid %in% c(2, 3, 39:53))

# Generate appropriate column names
schools_usa[is.na(schools_usa)] <- ""
names(schools_usa) <- mapply(paste0, schools_usa[1, ], schools_usa[2, ])

schools_usa <- schools_usa |>
  janitor::clean_names() |>
  dplyr::filter(!x23 %in% c(2, 3)) |>
  select(2:9) |>
  dplyr::filter(!x == "Percent") |>
  tidyr::pivot_longer(cols = -x, values_to = "percent", names_to = "type") |>
  dplyr::mutate(type = forcats::fct_recode(type,
      "total" = "total_1",
      "prekindergarten" = "pre_kinder_garten",
      "elementary" = "elemen_tary",
      "secondary_and_high_all" = "secondary_and_high_all_schools",
      "secondary_and_high_regular" = "regular_schools_1",
      "other" = "other_ungraded_and_not_applicable_not_reported"
    )
  )

schools_usa <- schools_usa |>
  filter(x != "Total") |>
  left_join(
    filter(schools_usa, x == "Total") |>
      select(type, total = percent),
    by = c("type")
  ) |>
  rename(size = x) |>
  mutate(
    size_midpoint = dplyr::case_when(
      size == "Under 100" ~ 50,
      size == "100 to 199" ~ 150,
      size == "200 to 299" ~ 250,
      size == "300 to 399" ~ 350,
      size == "400 to 499" ~ 450,
      size == "500 to 599" ~ 550,
      size == "600 to 699" ~ 650,
      size == "700 to 799" ~ 750,
      size == "800 to 999" ~ 900,
      size == "1,000 to 1,499" ~ 1250,
      size == "1,500 to 1,999" ~ 1750,
      size == "2,000 to 2,999" ~ 2500,
      size == "3,000 or more" ~ 3500
    ),
    percent = as.numeric(percent),
    total = as.numeric(total),
    count = percent / 100 * total,
    year = 2019
  ) |>
  filter(type != "secondary_and_high_regular") |> # category "secondary_and_high_regular" is a subset of "secondary_and_high_all"
  arrange(type) |>
  select(year, type, size, size_midpoint, percent, total, count)
usethis::use_data(schools_usa, overwrite = TRUE)

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
