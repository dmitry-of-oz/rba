# Load Austraclear Data (on Semis Outstanding)
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

## 1.1. From Austraclear ####

load(".../austraclear_master.Rdata")

## 1.2. From master settings and 'mops' package ####

load("Inputs/settings.Rdata")
load("Loaded/mops.Rdata")

# 2. Process data ####

austraclear_semis <- austraclear_master %>%
  as_tibble() %>%
  mutate_if(is.factor, ~ as.character(.)) %>%
  mutate(issuer_prefix = str_sub(issuer, 1, 3)) %>%
  filter((issuer_prefix %in% c('NSW',
                               'TCV',
                               'QLD',
                               'WAT',
                               'SAF',
                               'TAS',
                               'ACT',
                               'NTT') | issuer == "NSSB") & # NSWTC green issuance
           issuer != "SAFC") %>% # Sydney Airport Finance Corp
  mutate(issuer_prefix = str_sub(issuer, 1, 2),
         state = case_when(issuer_prefix == "NS" ~ "NSW",
                           issuer_prefix == "TC" ~ "Vic",
                           issuer_prefix == "QL" ~ "Qld",
                           issuer_prefix == "WA" ~ issuer_prefix,
                           issuer_prefix == "SA" ~ issuer_prefix,
                           issuer_prefix == "TA" ~ "Tas",
                           issuer_prefix == "AC" ~ "ACT",
                           issuer_prefix == "NT" ~ issuer_prefix,
                           TRUE ~ as.character(NA)),
         type = case_when(sec_subtype == "EPN" | issuer %in% c("QLDA", "WATS") ~ "Notes",
                          TRUE ~ "Bonds")) %>%
  select(-drawer, -prime_name, -issuer_prefix)

semis <- austraclear_semis %>%
  filter(date >= date_min & type == "Bonds") %>%
  select(date, series, issue_balance) %>%
  spread(series, issue_balance) %>%
  mutate_if(is.numeric, ~ replace_na(., 0) / 1e6) %>%
  right_join(days(date_min, date_max),
             by = "date") %>%
  arrange(date) %>%
  mutate_if(is.numeric, ~ na.locf(.)) %>%
  gather("ticker", "value", -date) %>%
  left_join(metadata,
            by = "ticker")

# 3. Produce outputs ####

save(semis, file = "Loaded/austraclear.Rdata")