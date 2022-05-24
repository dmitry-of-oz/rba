# Process Data for Counterfactual Approaches (to Use in a Graph and a Model of the 10-year AGS Yield)
# Dmitry Titkov / MPI / DM
# July 2021

# 0. Preliminaries ####

## 0.1. Packages ####

library(lubridate)
library(tidyverse)
library(zoo)

## 0.2. Functions ####

source("define_functions.R")

# 1. Load data ####

load("Inputs/settings.Rdata")
load("Loaded/bloomberg.Rdata")
load("Loaded/dc.Rdata")
load("Loaded/findur.Rdata")
load("Loaded/spd.Rdata")

# 2. Process data ####

## 2.1. For a graph of the differential between 10-year AGS and US Treasury yields ####

gd_10y_differential <- full_join(yields_ags_generic %>%
                                   filter(type == "10y_Benchmark") %>%
                                   select(date, close) %>%
                                   rename(Australia = close),
                                 yields_ust %>%
                                   select(date, close) %>%
                                   rename(`United States` = close),
                                 by = "date") %>%
  mutate(Differential = (Australia - `United States`) * 100)

## 2.2. For a model of the 10-year AGS yield ####

### 2.2.1. Annual GDP for Australia and the US ####

gdp <- bbg %>%
  select(date, aud_gdp, usd_gdp) %>%
  filter(!is.na(aud_gdp) & !is.na(usd_gdp)) %>%
  mutate(aud_gdp_annual = rollsumr(aud_gdp, 4, fill = NA),
         usd_gdp_annual = rollsumr(usd_gdp * 1e3 / 4, 4, fill = NA)) %>%
  select(date, aud_gdp_annual, usd_gdp_annual)

### 2.2.2. Daily RBA holdings of AGS ####

rba <- holdings %>%
  filter(issuer == "Cth" & subtype == "Nominals") %>%
  group_by(date) %>%
  summarise(rba = sum(value),
            .groups = "drop") %>%
  ungroup()

### 2.2.3. Combined data ####

#### 2.2.3.1. Daily ####

cf_daily <- days(date_min, today()) %>%
  left_join(bbg %>%
              select(-aud_gdp, -usd_gdp),
            by = "date") %>%
  left_join(gdp, by = "date") %>%
  left_join(rba, by = "date") %>%
  filter(!is.na(aud_zcy_10y) | !is.na(usd_zcy_10y) | !is.na(aud_gdp_annual) | !is.na(usd_gdp_annual)) %>%
  mutate_if(is.numeric, ~ na.locf(na.locf(na.approx(., na.rm = FALSE), na.rm = FALSE), na.rm = FALSE, fromLast = TRUE)) %>%
  mutate(rba_to_gdp = rba / aud_gdp_annual * 100,
         fed_to_gdp = fed / usd_gdp_annual * 100,
         bbsw_ois = aud_bbsw_3m - aud_ois_3m,
         libor_ois = usd_libor_3m - usd_ois_3m,
         ust_ois_10y = usd_zcy_10y - usd_ois_10y) %>%
  filter(date <= date_max + 7)

#### 2.2.3.2. Monthly, including expectations for US Fed holdings ####

cf_monthly <- cf_daily %>%
  days_to_month_ends() %>%
  left_join(spd, by = "date") %>%
  arrange(date) %>%
  mutate(fed_exp = na.locf(fed_exp, na.rm = FALSE),
         fed_dcs = cumsum(case_when(date <= min(spd$date) ~ 0,
                                    date > min(spd$date) ~ fed - lag(fed),
                                    TRUE ~ as.numeric(NA))),
         fed_exp_dcs = cumsum(replace_na(fed_exp - lag(fed_exp), 0)),
         fedex = case_when(date <= min(spd$date) ~ fed, # splices actual US Fed holdings with expectations for their holdings
                           date > min(spd$date) ~ fed - fed_dcs + fed_exp_dcs,
                           TRUE ~ as.numeric(NA)),
         fedex_to_gdp = fedex / usd_gdp_annual * 100) %>%
  select(-fed_exp, -fed_dcs, -fed_exp_dcs)

# 3. Produce outputs ####

save(gd_10y_differential,
     cf_daily,
     cf_monthly,
     file = "Processed/counterfactual_approaches.Rdata")