# Process Review
# Dmitry Titkov / MPI / DM
# March 2025

# 0. Preliminaries ####

## 0.1. Packages ####

library(lubridate)
library(tidyverse)

## 0.2. Functions ####

source("define_functions.R")

# 1. Load data ####

load("Inputs/settings_and_metadata.Rdata")
load("Intermediates/indicators.Rdata")

# 2. Process data ####

## 2.1. Daily measures ####

gd_daily <- bind_rows(
  daily_3 %>% mutate(tenor = "3-year"),
  daily_10 %>% mutate(tenor = "10-year")
) %>%
  mutate(
    bo = if_else(is.na(BidAsk), BidAsk1, BidAsk) * 100,
    bd = tDepth / 2 / 1e3,
    to = tTurnover / 1e3,
    pi = beta_comb * 1e6,
    bo = case_when(
      tenor == "3-year" & bo < 0.2 ~ 0.2,
      TRUE ~ bo
    )
  )

## 2.2. Intraday measures ####

intraday_missing <- crossing(
  tribble(~tenor, "3-year", "10-year"),
  tribble(~date, as_date("2021-11-02"), as_date("2021-11-04")),
  temp <- times(10, 16, 300) %>% head(-6)
)

gd_intraday <- bind_rows(
  intraday_3 %>% mutate(tenor = "3-year"),
  intraday_10 %>% mutate(tenor = "10-year"),
  intraday_missing
) %>%
  filter(time >= time_min & time <= time_max) %>%
  mutate(
    datetime = make_datetime(
      year = year(date),
      month = month(date),
      day = day(date),
      hour = hour(time),
      min = minute(time),
      sec = 0
    ),
    character = as.character(datetime),
    day = day(date),
    bo = if_else(is.na(BidAsk), BidAsk1, BidAsk) * 100,
    bd = tDepth / 2 / 1e3,
    to = tTurnover / 1e3,
    pi = beta_comb * 1e6
  ) %>%
  arrange(datetime) %>%
  group_by(tenor) %>%
  mutate(
    day_new = if_else(row_number() == 1, 0, if_else(day != lag(day) | is.na(lag(day)), 1, 0)),
    index = row_number()
  ) %>%
  ungroup() %>%
  mutate(character = if_else(day_new == 1, as.character(day), character))

## 2.3. Autocorrelations of measures ####

### 2.3.1. 3-year ####

gd_autocorrelation_3 <- bind_cols(
  acf(daily_3 %>% filter(roll != 1 & widening == 0) %>% pull(BidAsk1), lag.max = 30)$acf %>% as_tibble() %>% rename(`Bid-asks (pre-widening)` = 1),
  acf(daily_3 %>% filter(roll != 1 & widening == 1) %>% pull(BidAsk1), lag.max = 30)$acf %>% as_tibble() %>% rename(`Bid-asks (post-widening)` = 1),
  acf(daily_3 %>% filter(roll != 1) %>% pull(tDepth1), lag.max = 30)$acf %>% as_tibble() %>% rename(`Best depth` = 1),
  acf(daily_3 %>% filter(roll != 1) %>% pull(tTurnover1), lag.max = 30)$acf %>% as_tibble() %>% rename(`Turnover` = 1),
  acf(daily_3 %>% filter(roll != 1) %>% pull(beta1), lag.max = 30)$acf %>% as_tibble() %>% rename(`Price impact` = 1)
) %>%
  mutate(day = row_number() - 1) %>%
  pivot_longer(-day,
               names_to = "type",
               values_to = "value"
  )

### 2.3.2. 10-year ####

gd_autocorrelation_10 <- bind_cols(
  acf(daily_10 %>% filter(roll != 1) %>% pull(BidAsk1), lag.max = 30)$acf %>% as_tibble() %>% rename(`Bid-asks` = 1),
  acf(daily_10 %>% filter(roll != 1) %>% pull(tDepth1), lag.max = 30)$acf %>% as_tibble() %>% rename(`Best depth` = 1),
  acf(daily_10 %>% filter(roll != 1) %>% pull(tTurnover1), lag.max = 30)$acf %>% as_tibble() %>% rename(`Turnover` = 1),
  acf(daily_10 %>% filter(roll != 1) %>% pull(beta1), lag.max = 30)$acf %>% as_tibble() %>% rename(`Price impact` = 1)
) %>%
  mutate(day = row_number() - 1) %>%
  pivot_longer(-day,
               names_to = "type",
               values_to = "value"
  )

# 3. Produce outputs ####

save(gd_daily, gd_intraday, gd_autocorrelation_3, gd_autocorrelation_10, file = "Intermediates/review.Rdata")
