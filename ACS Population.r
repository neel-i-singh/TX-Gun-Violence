---
title: "TX ACS Population"
author: "Neel Singh"
date: "2026-03-18"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

```{r}
library(tidycensus)
library(dplyr)
library(purrr)
library(writexl)
```


```{r}
# Set your Census API key
census_api_key("1b02ea33fad73affe632747d96d0c5369450473f")#, install = TRUE)

# Total child variables
total_child_vars <- c(
  "B01001_003E", "B01001_004E", "B01001_005E", "B01001_006E",  # Male <18
  "B01001_027E", "B01001_028E", "B01001_029E", "B01001_030E"   # Female <18
)

# Black only child variables
black_child_vars <- c(
  "B01001B_003E", "B01001B_004E", "B01001B_005E", "B01001B_006E",  # Male <18
  "B01001B_018E", "B01001B_019E", "B01001B_020E", "B01001B_021E"   # Female <18
)

# Hispanic child variables
hispanic_child_vars <- c(
  "B01001I_003E", "B01001I_004E", "B01001I_005E", "B01001I_006E",
  "B01001I_018E", "B01001I_019E", "B01001I_020E", "B01001I_021E"
)

# White only child variables
white_child_vars <- c(
  "B01001A_003E", "B01001A_004E", "B01001A_005E", "B01001A_006E",
  "B01001A_018E", "B01001A_019E", "B01001A_020E", "B01001A_021E"
)

# Total adult variables
total_adult_vars <- c(
  "B01001_007E", "B01001_008E", "B01001_009E", "B01001_010E", "B01001_011E",  "B01001_012E", "B01001_013E", "B01001_014E", "B01001_015E", "B01001_016E", "B01001_017E", "B01001_018E", "B01001_019E", "B01001_020E", "B01001_021E", "B01001_022E", "B01001_023E", "B01001_024E", "B01001_025E", # Male > 18 
  "B01001_031E", "B01001_032E", "B01001_033E", "B01001_034E", "B01001_035E", "B01001_036E", "B01001_037E", "B01001_038E", "B01001_039E", "B01001_040E", "B01001_041E", "B01001_042E", "B01001_043E", "B01001_044E", "B01001_045E", "B01001_046E", "B01001_047E", "B01001_048E", "B01001_049E" # Female > 18
)

# Black only adult variables
black_adult_vars <- c(
  "B01001B_007E", "B01001B_008E", "B01001B_009E", "B01001B_010E", "B01001B_011E",  "B01001B_012E", "B01001B_013E", "B01001B_014E", "B01001B_015E", "B01001B_016E", # Male > 18 
  "B01001B_022E", "B01001B_023E", "B01001B_024E", "B01001B_025E", "B01001B_026E", "B01001B_027E", "B01001B_028E", "B01001B_029E", "B01001B_030E", "B01001B_031E" # Female > 18
)

# Hispanic adult variables ###NEED TO ADJUST THESE CODES AND ONWARDS
hispanic_adult_vars <- c(
  "B01001I_007E", "B01001I_008E", "B01001I_009E", "B01001I_010E",  "B01001I_011E", "B01001I_012E", "B01001I_013E", "B01001I_014E", "B01001I_015E", "B01001I_016E", # Male > 18 
  "B01001I_022E", "B01001I_023E", "B01001I_024E", "B01001I_025E", "B01001I_026E", "B01001I_027E", "B01001I_028E", "B01001I_029E", "B01001I_030E", "B01001I_031E" # Female > 18
)

# White only adult variables
white_adult_vars <- c(
  "B01001A_007E", "B01001A_008E", "B01001A_009E", "B01001A_010E", "B01001A_011E",  "B01001A_012E", "B01001A_013E", "B01001A_014E", "B01001A_015E", "B01001A_016E", # Male > 18 
  "B01001A_022E", "B01001A_023E", "B01001A_024E", "B01001A_025E", "B01001A_026E", "B01001A_027E", "B01001A_028E", "B01001A_029E", "B01001A_030E",
  "B01001A_031E" # Female > 18
)

# Combined list
variables <- c(
  total_child_vars,
  black_child_vars,
  hispanic_child_vars,
  white_child_vars,
  total_adult_vars,
  black_adult_vars,
  hispanic_adult_vars,
  white_adult_vars
)
```

```{r}
# 2016 data
ACS2016 <- get_acs(
  geography = "tract",
  state = "TX",
  variables = variables,
  year = 2016,        # ACS 5-year estimates (earliest is 2009–2013 window)
  survey = "acs5",
  output = "wide"
)

```

