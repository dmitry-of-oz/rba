# Load Yieldbroker Data (on Bid-offer Spreads and Turnover for AGS and Semis)
# Dmitry Titkov / MPI / DM
# July 2021

# 0. Preliminaries ####

library(lubridate)
library(tidyverse)

# 1. Load data ####

## 1.1. From Yieldbroker ####

bo_raw <- readRDS(".../yieldbroker_parcel_cal.RDS") %>%
  as_tibble()

to_raw <- readRDS(".../turnover_his.RDS") %>%
  as_tibble()

## 1.2. From 'mops' package ####

load("Loaded/mops.Rdata")

# 2. Process data ####

## 2.1. Bid-offer spreads ####

bo <- bo_raw %>%
  filter(!is.na(DATE) & !is.na(SPREAD) & SPREAD >= 0 & TYPE_NAME != "SUPRASOVAGCY") %>%
  rename(date = DATE,
         spread = SPREAD,
         parcel = AVERAGE_DAILY_PARCEL_SIZE,
         cusip = ISIN) %>%
  left_join(metadata,
            by = "cusip") %>%
  select(date, spread, parcel, all_of(metacols))

## 2.2. Turnover ####

to <- to_raw %>%
  mutate(type = case_when(product_type == "Aus Gov" ~ "AGS",
                          product_type == "Aus Semi" ~ "Semis",
                          TRUE ~ as.character(NA)),
         outright_value = outright_value / 1e6,
         switch_value = switch_value / 1e6,
         EFP_value = EFP_value / 1e6) %>%
  filter(!is.na(type) & year(date) >= 2019) %>%
  select(date, type, maturity, contains("_no"), contains("_value")) %>%
  left_join(metadata %>%
              filter(subtype == "Nominals" & nchar(cusip) > 1 & !(ticker %in% c("TS3090", "TV3132", # Jun-20
                                                                                "SF5057", "TV3189", # Nov-23
                                                                                "NT2743", "TV3199", # Mar-24
                                                                                "TC2215", "TV3178", "TV3190", # Nov-30
                                                                                "TV3130", "TV3135", # Mar-33
                                                                                "TC2197", "TV3196", # Nov-37
                                                                                "TC2211", "TV3181", "TV3195", # Nov-40
                                                                                "QT4088", "TV3192"))), # Nov-41
            by = c("type", "maturity")) %>%
  filter(!is.na(issuer))

# 3. Produce outputs ####

save(bo, to,
     file = "Loaded/yieldbroker.Rdata")