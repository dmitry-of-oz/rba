# Process Introduction
# Dmitry Titkov / MPI / DM
# March 2025

# 0. Preliminaries ####

## 0.1. Packages ####

library(hms)
library(lubridate)
library(tidyverse)

## 0.2. Functions ####

source("define_functions.R")

# 1. Load data ####

load("Inputs/aofm_turnover.Rdata")

load("Intermediates/trades_3.Rdata")
trades_3 <- trades
rm(trades)

load("Intermediates/trades_10.Rdata")
trades_10 <- trades
rm(trades)

# 2. Process data ####

## 2.1. Average daily turnover ####

### 2.1.1. Turnover by day ####

turnover_3 <- trades_3 %>%
  group_by(date) %>%
  summarise(
    to_3 = sum(size) / 10 / 1e3,
    .groups = "drop"
  ) %>%
  ungroup()

turnover_10 <- trades_10 %>%
  group_by(date) %>%
  summarise(
    to_10 = sum(size) / 10 / 1e3,
    .groups = "drop"
  ) %>%
  ungroup()

### 2.1.2. Turnover by month ####

asx_turnover <- full_join(turnover_3,
                          turnover_10,
                          by = "date"
) %>%
  mutate(date = month_end(date)) %>%
  group_by(date) %>%
  summarise(
    asx_3 = sum(to_3, na.rm = TRUE),
    asx_10 = sum(to_10, na.rm = TRUE),
    days = n(),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  mutate(
    asx_3 = asx_3 / days,
    asx_10 = asx_10 / days
  )

### 2.1.3. Graph data ####

gd_turnover <- aofm_turnover %>%
  filter(date >= min(asx_turnover$date) & date <= max(asx_turnover$date)) %>%
  left_join(
    asx_turnover %>%
      select(date, days),
    by = "date"
  ) %>%
  mutate(
    days = if_else(days <= 15, 20, days),
    aofm_3 = to_3 / days,
    aofm_10 = to_10 / days,
    aofm_other = to_other / days
  ) %>%
  select(-contains("to"), -days) %>%
  left_join(
    asx_turnover %>%
      select(-days),
    by = "date"
  ) %>%
  pivot_longer(-date,
               names_to = "type_tenor",
               values_to = "value"
  ) %>%
  separate(type_tenor, c("type", "tenor")) %>%
  mutate(
    tenor = recode(tenor,
                   "3" = "3-year",
                   "10" = "10-year",
                   "other" = "Other"
    ),
    tenor = factor(tenor,
                   levels = c("Other", "10-year", "3-year")
    )
  )

## 2.2. Average intraday turnover ####

days_3 <- trades_3 %>%
  distinct(date) %>%
  nrow()
days_10 <- trades_10 %>%
  distinct(date) %>%
  nrow()

sessions_3 <- trades_3 %>%
  mutate(time = trunc_hms(time, 300)) %>%
  group_by(time) %>%
  summarise(
    to = sum(size) / 1e3,
    .groups = "drop"
  ) %>%
  ungroup() %>%
  right_join(times(0, 23, 300),
             by = "time"
  ) %>%
  arrange(time) %>%
  mutate_if(is.numeric, ~ replace_na(., 0)) %>%
  mutate(
    to = to / days_3,
    hour = hour(time),
    hour_new = if_else(row_number() == 1, 1, if_else(hour != lag(hour) | is.na(lag(hour)), 1, 0)),
    character = if_else(hour_new == 1, as.character(hour), as.character(time))
  )

sessions_10 <- trades_10 %>%
  mutate(time = trunc_hms(time, 300)) %>%
  group_by(time) %>%
  summarise(
    to = sum(size) / 1e3,
    .groups = "drop"
  ) %>%
  ungroup() %>%
  right_join(times(0, 23, 300),
             by = "time"
  ) %>%
  arrange(time) %>%
  mutate_if(is.numeric, ~ replace_na(., 0)) %>%
  mutate(
    to = to / days_10,
    hour = hour(time),
    hour_new = if_else(row_number() == 1, 1, if_else(hour != lag(hour) | is.na(lag(hour)), 1, 0)),
    character = if_else(hour_new == 1, as.character(hour), as.character(time))
  )

# 3. Produce outputs ####

save(gd_turnover, sessions_3, sessions_10, file = "Intermediates/introduction.Rdata")
