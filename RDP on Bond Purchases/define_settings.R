# Define Settings (for Dates, Bonds and Issuers)
# Dmitry Titkov / MPI / DM
# August 2021

# 0. Preliminaries ####

## 0.1. Packages ####

library(bizdays)
library(lubridate)
library(tidyverse)

## 0.2. Functions ####

source("define_functions.R")

## 0.3. Settings ####

calendar <- create.calendar("bd", weekdays = c("saturday", "sunday"))

# 1. Date settings ####

## 1.1. For loading and processing data ####

date_min <- as_date("2014-12-31") # AOFM and Austraclear data are not loaded before this date
date_max <- as_date("2022-02-28") # most outputs are cut off after this date

date_min_bd <- preceding(date_min, "bd")
date_max_bd <- preceding(date_max, "bd")

date_mf_begin <- as_date("2020-03-20")
date_mf_before <- date_mf_begin - 1
date_mf_end <- as_date("2020-05-06")

date_qe_announcement <- as_date("2020-11-03")
date_qe_begin <- as_date("2020-11-05")
date_qe_before <- date_qe_begin - 1
date_qe_end <- date_max

date_old <- as_date("1990-01-01") # historical Bloomberg data are loaded from this date
date_new <- as_date("2020-01-01") # recent Bloomberg data are loaded from this date

## 1.2. For model sample periods ####

### 1.2.1. Yield target purchases ####

date_yt_begin <- as_date("2020-08-05")
date_yt_end <- as_date("2021-11-02")

week_yt_begin <- week_ags(date_yt_begin)
week_yt_end <- week_ags(date_yt_end)

### 1.2.2. 10-year AGS yield ####

date_10y_begin <- as_date("2017-12-31")
date_10y_end <- as_date("2020-08-31")

### 1.2.3. Bid-offer spreads and turnover ####

#### 1.2.3.1. Weeks ending on Fridays (for AGS purchases) #####

week_ags_min <- week_ags(date_min_bd) + 7
week_ags_max <- week_ags(date_max_bd) - 7

week_ags_mf_begin <- week_ags(date_mf_begin)
week_ags_mf_end <- week_ags(date_mf_end)

week_ags_qe_begin <- week_ags(date_qe_begin)
week_ags_qe_end <- week_ags_max

#### 1.2.3.2. Weeks ending on Tuesdays (for semis purchases) #####

week_semis_min <- week_semis(date_min_bd) + 7
week_semis_max <- week_semis(date_max_bd) - 7

week_semis_mf_begin <- week_semis(date_mf_begin)
week_semis_mf_end <- week_semis(date_mf_end)

week_semis_qe_begin <- week_semis(date_qe_begin)
week_semis_qe_end <- week_semis_max

## 1.3. For event studies ####

dates_es_mf <- tribble(~date, ~window, ~event,
                       "2020-03-16", "open_to_close", "board", # https://www.rba.gov.au/media-releases/2020/mr-20-07.html
                       "2020-03-19", "open_to_close", "board") %>% # https://www.rba.gov.au/media-releases/2020/mr-20-08.html
  mutate(date = as_date(date))

dates_es_qe <- tribble(~date, ~window, ~event,
                       "2020-09-14", "close_to_close", "article", # on Sunday, 13 September: https://www.afr.com/policy/economy/rba-and-markets-are-out-of-tune-20200910-p55udq
                       "2020-09-22", "open_to_close", "speech", # https://www.rba.gov.au/speeches/2020/sp-dg-2020-09-22.html
                       "2020-09-23", "open_to_close", "report", # https://westpaciq.westpac.com.au/Article/45182
                       "2020-09-28", "open_to_close", "report", # Westpac: 'RBA policy changes pushed back to November 3'
                       "2020-10-06", "open_to_close", "board", # https://www.rba.gov.au/media-releases/2020/mr-20-24.html
                       "2020-10-07", "close_to_close", "article", # after close on 6 October: https://www.afr.com/markets/equity-markets/rba-rate-shift-hangs-on-just-one-word-20201006-p562c9
                       "2020-10-15", "open_to_close", "speech", # https://www.rba.gov.au/speeches/2020/sp-gov-2020-10-15.html
                       "2020-10-26", "close_to_close", "article", # on Saturday, 24 October: https://www.heraldsun.com.au/business/terry-mccrann/terry-mccrann-what-you-can-expect-from-the-rba-on-cup-day/news-story/15325e27c9d54f2568293bcfbf0afb2f
                       "2020-11-03", "open_to_close", "board") %>% # https://www.rba.gov.au/media-releases/2020/mr-20-28.html
  mutate(date = as_date(date))

dates_es_sl <- c("2021-03-09", "2021-03-10",
                 "2021-05-07", "2021-05-10",
                 "2021-10-19", "2021-10-20") %>%
  enframe(name = NULL, value = "date") %>%
  mutate(date = as_date(date))

dates_es_yt <- days(as_date("2021-10-25"), date_yt_end) %>%
  filter(!(wday(date, label = TRUE) %in% c("Sat", "Sun")))

# 2. Non-date settings ####

## 2.1. Bonds ####

bond_yt <- days(date_mf_before, date_yt_end) %>%
  mutate(ticker = case_when(date <= as_date("2020-10-20") ~ "TB133",
                            TRUE ~ "TB137"))

bonds_yt <- bond_yt %>% distinct(ticker) %>% pull(ticker)
bonds_yt_plus <- c(bonds_yt, "TB153")

## 2.2. Issuers ####

issuers_drop_es <- c("ACT", "NT")
issuers_drop_ts <- c("ACT", "NT", "Tas")
issuers_drop_cs <- c("ACT", "NT", "Tas")
issuers_drop_ls <- c("ACT", "NT")
issuers_drop_bo <- c("ACT", "NT", "Tas")
issuers_drop_to <- c("ACT", "NT", "Tas")

## 2.3. Other ####

days_year <- 365.24
days_adjacent <- 370 # threshold in days for difference in maturities to determine adjacent bonds for estimates of flow effects

tenors <- c(1:10, 12, 15, 20, 25, 30)
to_e <- 1e-3

# 3. Produce outputs ####

save(date_min, date_max, date_min_bd, date_max_bd,
     date_mf_begin, date_mf_before, date_mf_end,
     date_qe_begin, date_qe_before, date_qe_end, date_qe_announcement,
     date_old, date_new,
     date_yt_begin, date_yt_end,
     week_yt_begin, week_yt_end,
     date_10y_begin, date_10y_end,
     week_ags_min, week_ags_max,
     week_ags_mf_begin, week_ags_mf_end,
     week_ags_qe_begin, week_ags_qe_end,
     week_semis_min, week_semis_max,
     week_semis_mf_begin, week_semis_mf_end,
     week_semis_qe_begin, week_semis_qe_end,
     dates_es_mf, dates_es_qe, dates_es_sl, dates_es_yt,
     bond_yt,
     bonds_yt, bonds_yt_plus,
     issuers_drop_es, issuers_drop_ts, issuers_drop_cs, issuers_drop_ls, issuers_drop_bo, issuers_drop_to,
     days_year, days_adjacent,
     tenors,
     to_e,
     file = "Inputs/settings.Rdata")