# install.packages(c("tidycensus", "dplyr", "tidyr", "purrr", "stringr", "readr"))
library(tidycensus)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readr)

# ------------------------------------------------------------
# One-time setup:
# census_api_key("YOUR_CENSUS_API_KEY", install = TRUE, overwrite = TRUE)
# ------------------------------------------------------------

years <- 2016:2019

# ACS age-by-race/ethnicity tables
# NOTE:
# - C01001A-G are race-alone tables
# - C01001H is White alone, not Hispanic or Latino
# - C01001I is Hispanic or Latino
# These are not all mutually exclusive categories.
table_map <- c(
  overall_total_population = "C01001",
  white_alone = "C01001A",
  black_alone = "C01001B",
  aian_alone = "C01001C",
  asian_alone = "C01001D",
  nhpi_alone = "C01001E",
  some_other_race_alone = "C01001F",
  two_or_more_races = "C01001G",
  white_alone_not_hispanic = "C01001H",
  hispanic_or_latino = "C01001I"
)

get_age_counts_for_table <- function(year, table_code, group_name) {
  # Pull variable metadata for this table/year
  vars_meta <- load_variables(year, "acs5", cache = TRUE) %>%
    filter(str_starts(name, paste0(table_code, "_"))) %>%
    select(variable = name, label)

  # Pull tract-level ACS data for Texas
  dat <- get_acs(
    geography = "tract",
    state = "TX",
    survey = "acs5",
    year = year,
    table = table_code,
    cache_table = TRUE
  ) %>%
    left_join(vars_meta, by = "variable") %>%
    mutate(
      is_total = label == "Estimate!!Total:",
      is_pediatric = str_detect(label, "Under 18 years$"),
      is_adult = str_detect(label, "18 to 64 years$|65 years and over$")
    ) %>%
    group_by(GEOID, NAME) %>%
    summarise(
      year = year,
      race_ethnicity_group = group_name,

      overall = estimate[is_total][1],
      overall_moe = moe[is_total][1],

      pediatric = sum(estimate[is_pediatric], na.rm = TRUE),
      pediatric_moe = moe_sum(moe[is_pediatric], estimate[is_pediatric]),

      adult = sum(estimate[is_adult], na.rm = TRUE),
      adult_moe = moe_sum(moe[is_adult], estimate[is_adult]),

      .groups = "drop"
    )

  dat
}

# Build long file for all years and tables
tx_tract_pop_long <- map_dfr(
  years,
  \(yr) {
    imap_dfr(
      table_map,
      \(tbl, grp) get_age_counts_for_table(year = yr, table_code = tbl, group_name = grp)
    )
  }
)

# Wide version: one row per tract-year, many columns
tx_tract_pop_wide <- tx_tract_pop_long %>%
  pivot_wider(
    id_cols = c(GEOID, NAME, year),
    names_from = race_ethnicity_group,
    values_from = c(overall, overall_moe, pediatric, pediatric_moe, adult, adult_moe),
    names_glue = "{race_ethnicity_group}_{.value}"
  ) %>%
  arrange(year, GEOID)

# Optional: rename the "overall_total_population" fields to cleaner names
tx_tract_pop_wide <- tx_tract_pop_wide %>%
  rename(
    total_population = overall_total_population_overall,
    total_population_moe = overall_total_population_overall_moe,
    pediatric_population = overall_total_population_pediatric,
    pediatric_population_moe = overall_total_population_pediatric_moe,
    adult_population = overall_total_population_adult,
    adult_population_moe = overall_total_population_adult_moe
  )

# Write output
write_csv(tx_tract_pop_wide, "tx_tract_population_2016_2019.csv")

# Preview
print(tx_tract_pop_wide, n = 5)
