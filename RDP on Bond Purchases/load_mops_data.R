# Load 'mops' Data (on AGS and Semis Metadata and Yields, and OIS Rates)
# Dmitry Titkov / MPI / DM
# July 2021

# 0. Preliminaries ####

## 0.1. Packages ####

library(lubridate)
library(mops)
library(tidyverse)

## 0.2. Functions ####

source("define_functions.R")

interpolate_ags <- function (date_set, maturity_set) {
  ttm_set <- as.numeric(maturity_set - date_set) / days_year
  yields_ags_filtered <- dplyr::filter(yields_ags,
                                       date == date_set & subtype == "Nominals")
  yields_ags_mutated <- dplyr::mutate(yields_ags_filtered,
                                      ttm = as.numeric(maturity - date) / days_year)
  if (nrow(yields_ags_mutated) < 2) {
    return(NA)
  } else {
    return(approx(x = c(yields_ags_mutated$ttm, 0),
                  y = c(yields_ags_mutated$value, 0),
                  xout = ttm_set,
                  rule = 2)$y)
  }
}

# 1. Load data ####

## 1.1. From Findur ####

load("Loaded/findur.Rdata")

dates_purchases_ags <- purchases %>% filter(type == "AGS") %>% distinct(date)
dates_purchases_semis <- purchases %>% filter(type == "Semis") %>% distinct(date)

## 1.2. From master settings ####

load("Inputs/settings.Rdata")

## 1.3. From 'mops' package ####

### 1.3.1. Metadata ####

metadata <- load_ags_semis_metadata() # also used in load_findur_data.R, but without attaching the 'mops' package
metacols <- colnames(metadata)

### 1.3.2. Daily yields ####

#### 1.3.2.1. At the close ####

yields_ags <- load_ags_yields_close()
yields_semis <- load_semis_yields_close()

message(crayon::green(paste0("The latest AGS and semis closing yields loaded using the 'mops' package were for ", str_trim(format(min(max(yields_ags$date), max(yields_semis$date)), "%e %B %Y")))))

#### 1.3.2.2. At the open ####

yields_ags_open <- load_ags_yields_open(time_set = "08:40")
yields_semis_open <- load_semis_yields_open(time_set = "08:40")

### 1.3.3. Intraday yields/rates ####

#### 1.3.3.1. AGS yields ####

yields_ags_intraday_purchases <- load_ags_yields_intraday_hist(dates = dates_purchases_ags$date)

yields_ags_intraday_es_mf <- load_ags_yields_intraday_hist(dates = dates_es_mf$date)
yields_ags_intraday_es_sl <- load_ags_yields_intraday_hist(dates = dates_es_sl$date)
yields_ags_intraday_es_yt <- load_ags_yields_intraday_hist(dates = dates_es_yt$date)

#### 1.3.3.2. Semis yields ####

yields_semis_intraday_purchases_qe <- load_semis_yields_intraday_hist(dates = dates_purchases_semis$date) # intraday semis yields were captured in Findur starting from 11 November 2020

#### 1.3.3.3. OIS rates ####

ois_intraday <- load_ois_rates_intraday()

# 2. Process data ####

## 2.1. Semis spreads ####

spreads_semis <- yields_semis %>%
  filter(date >= date_mf_before & subtype == "Nominals" & ticker != "TC2XXX" & coupon != 0 & nchar(cusip) > 1 & gg == FALSE) %>%
  mutate(yield_ags = mapply(interpolate_ags, date, maturity),
         spread = (yield - yield_ags) * 100) %>%
  filter(!is.na(spread))

## 2.2. Daily OIS rates ####

ois <- ois_intraday %>%
  mutate(time = case_when(hour(datetime) == 16 & minute(datetime) == 30 ~ "close",
                          hour(datetime) == 8 & minute(datetime) == 40 ~ "open",
                          TRUE ~ as.character(NA))) %>%
  select(-datetime) %>%
  filter(!is.na(time)) %>%
  spread(time, ois) %>%
  mutate(change = (close - open) * 100)

# 3. Produce outputs ####

save(metadata, metacols, dates_purchases_ags, dates_purchases_semis,
     yields_ags, yields_semis, yields_ags_open, yields_semis_open,
     yields_ags_intraday_purchases, yields_semis_intraday_purchases_qe,
     yields_ags_intraday_es_mf, yields_ags_intraday_es_sl, yields_ags_intraday_es_yt,
     ois,
     ois_intraday,
     spreads_semis,
     file = "Loaded/mops.Rdata")