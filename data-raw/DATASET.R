# Make any changes to the saved data for the package here

library(readr)
library(readxl)
library(dplyr)
library(janitor)

baseline_household_demographics <- read_csv("data-raw/Hinch_et_al_baseline_household_demographics.csv")
usethis::use_data(baseline_household_demographics)

schools_england <- read_csv("data-raw/spc_school_level_underlying_data_23112023.csv")
usethis::use_data(schools_england)

schools_usa <- readxl::read_xls("data-raw/tabn216.40.xls")

# Just the most recent years data (2019 / 2020)
schools_usa_latest <- schools_usa |>
  tibble::rowid_to_column("rowid") |>
  filter(rowid %in% c(2, 3, 39:53))

# Generate appropriate column names
schools_usa_latest[is.na(schools_usa_latest)] <- ""
names(schools_usa_latest) <- mapply(paste0, schools_usa_latest[1, ], schools_usa_latest[2, ])

schools_usa_latest <- schools_usa_latest |>
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

df <- schools_usa_latest |>
  filter(x != "Total") |>
  left_join(
    filter(schools_usa_latest, x == "Total") |>
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
    count = percent / 100 * total
  ) |>
  arrange(type)

df
names(schools_usa_latest)
