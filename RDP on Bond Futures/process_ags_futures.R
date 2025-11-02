# Process AGS Futures
# Dmitry Titkov / MPI / DM
# November 2024

# 0. Preliminaries ####

## 0.1. Packages ####

library(hms)
library(lubridate)
library(tidyverse)

## 0.2. Functions ####

source("define_functions.R")

# 1. Load data ####

load("Inputs/settings_and_metadata.Rdata")

ags_futures_raw <- load_ags_futures(tenor = ags_futures_tenor)

# 2. Process data ####

ags_futures <- ags_futures_raw %>%
  as_tibble() %>%
  mutate(
    date = as_date(times),
    time = as_hms(times),
    type = str_to_lower(type),
    condcode = if_else(condcode == "", as.character(NA), as.character(condcode))
  ) %>%
  rename(
    code = condcode,
    contract = futs_contract
  ) %>%
  select(date, time, contract, type, code, value, size)

## 2.1. Dates ####

ags_futures_dates <- ags_futures %>%
  distinct(date) %>%
  arrange(date) %>%
  filter(date >= date_min & date <= date_max &
           !(wday(date, label = TRUE, abbr = FALSE) %in% c("Saturday", "Sunday")))

## 2.2. Contracts ####

ags_futures_contracts_raw <- ags_futures %>%
  group_by(contract) %>%
  summarise(
    date_contract = as_date(max(date)),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  arrange(date_contract)

ags_futures_contracts <- tibble()
for (i in 1:nrow(ags_futures_contracts_raw)) {
  contract <- ags_futures_contracts_raw[i, ]$contract
  date_contract <- ags_futures_contracts_raw[i, ]$date_contract
  date_narrowing <- ags_futures_dates %>%
    filter(date <= date_contract) %>%
    tail(days_narrowing) %>%
    head(1) %>%
    pull(date)
  ags_futures_contract <- tribble(
    ~contract, ~date_contract, ~date_narrowing,
    contract, date_contract, date_narrowing
  )
  ags_futures_contracts <- bind_rows(ags_futures_contracts, ags_futures_contract)
}
ags_futures_contracts <- ags_futures_contracts %>% filter(date_contract < date_max)

# 3. Produce outputs ####

save(ags_futures_dates, ags_futures_contracts, file = paste0("Inputs/ags_futures_", ags_futures_tenor, ".Rdata"))

rm(ags_futures_raw)
