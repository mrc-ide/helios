# Make any changes to the saved data for the package here

library(readr); library(dplyr)

baseline_household_demographics <- read_csv("data-raw/Hinch_et_al_baseline_household_demographics.csv")
usethis::use_data(baseline_household_demographics)

schools_england <- read_csv("data-raw/spc_school_level_underlying_data_23112023.csv")
usethis::use_data(schools_england)

# Loading in and processing data on US workplace sizes from https://www.census.gov/data/tables/2019/econ/susb/2019-susb-annual.html

## USA data for small entreprises
workplaces_small_data <- read.csv("data-raw/us_state_naics_detailedsizes_2019.csv") %>%
  filter(NAICS.Description == "Total" & State.Name == "United States") %>%
  select(Enterprise.Size, Firms, Establishments, Employment) %>%
  filter(Enterprise.Size != "01: Total" &
           Enterprise.Size != "06: <20 employees" &
             Enterprise.Size != "19: <500 employees" &
               Enterprise.Size != "26: 5,000+ employees")

## USA data with more detailed granularity on the size of large entreprises
workplaces_large_data <- read.csv("data-raw/us_naicssector_large_emplsize_2019.csv") %>%
  filter(NAICS.Description == "Total") %>%
  select(Enterprise.Size, Firms, Establishments, Employment) %>%
  filter(Enterprise.Size %in% c("09: 5,000-9,999 employees", "10: 10,000-19,999 employees", "11: 20,000+ employees"))

## Combining the above together and extracting out the lower and upper bound as well the midpoint
workplaces_overall <- rbind(workplaces_small_data, workplaces_large_data)
workplace_sizes <- data.frame(midpoint = c(3, 7, 12, 17, 22, 27, 32, 37, 45, 62, 87, 125,
                                           175, 250, 350, 450, 625, 875, 1250, 1750, 2250,
                                           3750, 7500, 15000, 25000),
                              lower = c(1, 5, 10, 15, 20, 25, 30, 35, 40, 50, 75, 100, 150,
                                        200, 300, 400, 500, 750, 1000, 1500, 2000, 2500, 5000,
                                        10000, 20000),
                              upper = c(4, 9, 14, 19, 24, 29, 34, 39, 49, 74, 99, 149, 199, 299,
                                        399, 499, 749, 999, 1499, 1999, 2499, 4999, 9999, 19999, NA))
workplaces_usa <- cbind(workplace_sizes, workplaces_overall)

## Generating the vector of establishments
## Note from https://www.bls.gov/opub/mlr/2016/article/establishment-firm-or-enterprise.htm#:~:text=An%20establishment%20is%20a%20single,Internal%20Revenue%20Service%20(IRS).
## "An establishment is a single physical location where one predominant activity occurs.
##  A firm is an establishment or a combination of establishments". We've therefore got to adjust
##  further to convert Entreprize.Sizes into Establishment.Sizes (which is what we need for our modelling)
workplaces_usa %>%
  mutate(Employment = as.numeric(gsub(",", "", Employment))) %>%
  mutate(Establishments = as.numeric(gsub(",", "", Establishments))) %>%
  mutate(avg_size = Employment / Establishments)
