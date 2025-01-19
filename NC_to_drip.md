# Natural Cycles to Drip


``` r
library(tidyverse)

# read in my data from Natural Cycles
Daily_Entries <- read_csv("Daily Entries.csv")

# read in example data from Drip
drip_format <- read_csv("drip_example.csv")
drip_format <- drip_format |>
  slice(-c(1:5))
```

## Conversion

``` r
drip <- Daily_Entries |>
  select(Date:Menstruation, `Menstruation Quantity`, `Data Flag`) |> # This is all I was converting
  rename(date = Date, temperature.value = Temperature, bleeding.value = `Menstruation Quantity`) |>
  mutate(bleeding.value = case_when(
    Menstruation == "SPOTTING" ~ 0,
    bleeding.value == "HEAVY" ~ 3,
    bleeding.value == "MEDIUM" ~ 2,
    bleeding.value == "LIGHT" ~ 1,
    !is.na(Menstruation) & is.na(bleeding.value) ~ 2, # if I didn't report, let's call it medium
    TRUE ~ NA_integer_
  )) |>
  mutate(bleeding.exclude = if_else(is.na(bleeding.value), NA, FALSE)) |> # if you have things to exclude, this needs to be changed
  mutate(temperature.exclude = case_when(
    !is.na(temperature.value) ~ FALSE,
    !is.na(temperature.value) & str_detect(`Data Flag`, "DEVIATION") ~ TRUE,
    is.na(temperature.value) ~ NA,
    TRUE ~ NA
  )) |>
  mutate(
    pain.cramps = str_detect(`Data Flag`, "PAIN_CRAMPS"),
    pain.headache = str_detect(`Data Flag`, "PAIN_HEADACHE"),
    pain.backache = str_detect(`Data Flag`, "PAIN_BACKACHE"),
    pain.nausea = str_detect(`Data Flag`, "PAIN_NAUSEOUS")
  ) |>
  select(-Menstruation, -`Data Flag`)

drip <- bind_rows(drip_format, drip)
```

Write it out

``` r
write_csv(drip, "drip.csv", na = "")
```
