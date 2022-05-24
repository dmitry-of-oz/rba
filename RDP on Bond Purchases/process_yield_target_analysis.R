# Process Data for Yield Target Analysis (to Use in Graphs and a Model of Yield Target Purchases)
# Dmitry Titkov / MPI / DM
# July 2021

# 0. Preliminaries ####

## 0.1. Packages ####

library(hms)
library(lubridate)
library(tidyverse)
library(zoo)

## 0.2. Functions ####

source("define_functions.R")

### 0.2.1. To interpolate daily OIS rates from Bloomberg ####

interpolate_ois_daily <- function (date_set, maturity_set) {
  ttm_set <- as.numeric(maturity_set - date_set) / days_year
  bbg_data_ois_filtered <- dplyr::filter(bbg_data_ois,
                                         date == date_set)
  bbg_data_ois_mutated <- dplyr::mutate(bbg_data_ois_filtered,
                                        ttm = as.numeric(stringr::str_remove(stringr::str_remove(bbg_code, "ADSO"), " Curncy")))
  if (nrow(bbg_data_ois_mutated) < 2) {
    return(NA)
  } else {
    return(approx(x = c(bbg_data_ois_mutated$ttm, 0),
                  y = c(bbg_data_ois_mutated$PX_LAST, 0),
                  xout = ttm_set,
                  rule = 2)$y)
  }
}

### 0.2.2. To interpolate intraday OIS rates from Findur ####

interpolate_ois_intraday <- function (date_set, datetime_set, maturity_set) {
  ttm_set <- as.numeric(maturity_set - date_set) / days_year
  ois_intraday_filtered <- dplyr::filter(ois_intraday,
                                         datetime == datetime_set)
  ois_intraday_mutated <- dplyr::mutate(ois_intraday_filtered,
                                        ttm = maturity_months / 12)
  if (nrow(ois_intraday_mutated) < 2) {
    return(NA)
  } else {
    return(approx(x = c(ois_intraday_mutated$ttm, 0),
                  y = c(ois_intraday_mutated$ois, 0),
                  xout = ttm_set,
                  rule = 2)$y)
  }
}

# 1. Load data ####

load("Inputs/settings.Rdata")
load("Loaded/aofm.Rdata")
load("Loaded/auctions.Rdata")
load("Loaded/bloomberg.Rdata")
load("Loaded/findur.Rdata")
load("Loaded/mops.Rdata")
load("Loaded/tick.Rdata")

# 2. Process data ####

## 2.1. For a model of yield target purchases ####

### 2.1.1. AGS yields ####

yt_ags <- full_join(yields_ags %>%
                      filter(ticker %in% bonds_yt) %>%
                      rename(close = value) %>%
                      select(date, all_of(metacols), close),
                    yields_ags_open %>%
                      filter(ticker %in% bonds_yt) %>%
                      rename(open = value) %>%
                      select(date, all_of(metacols), open),
                    by = c("date", metacols)) %>%
  left_join(ois %>%
              filter(maturity_months == 3) %>%
              select(date, close, open) %>%
              rename(close_ois_3m = close,
                     open_ois_3m = open),
            by = "date") %>%
  left_join(yields_ags_generic %>%
              filter(type == "10y_Benchmark") %>%
              select(date, close, open) %>%
              rename(close_ags_10y = close,
                     open_ags_10y = open),
            by = "date") %>%
  filter((!is.na(close) & !is.na(open)) & date >= date_mf_begin) %>%
  left_join(purchases %>%
              filter(ticker %in% bonds_yt_plus) %>%
              mutate(value = value / 1e3) %>%
              select(date, ticker, value) %>%
              spread(ticker, value) %>%
              rename_if(str_detect(names(.), "TB"), ~ paste0(., "_purchases")),
            by = "date") %>%
  left_join(holdings %>%
              filter(ticker %in% bonds_yt_plus) %>%
              arrange(ticker, date) %>%
              group_by(ticker) %>%
              mutate(value = lag(value) / 1e3) %>%
              ungroup() %>%
              select(date, ticker, value) %>%
              spread(ticker, value) %>%
              rename_if(str_detect(names(.), "TB"), ~ paste0(., "_holdings")),
            by = "date") %>%
  left_join(ags %>%
              filter(ticker %in% bonds_yt_plus) %>%
              arrange(ticker, date) %>%
              group_by(ticker) %>%
              mutate(value = lag(value) / 1e3) %>%
              ungroup() %>%
              select(date, ticker, value) %>%
              spread(ticker, value) %>%
              rename_if(str_detect(names(.), "TB"), ~ paste0(., "_outstanding")),
            by = "date") %>%
  mutate_if(is.numeric, ~ replace_na(., 0)) %>%
  mutate(purchases_yt = TB133_purchases + TB137_purchases + TB153_purchases,
         purchases_that = case_when(ticker == "TB133" ~ TB133_purchases,
                                    ticker == "TB137" ~ TB137_purchases,
                                    TRUE ~ as.numeric(NA)),
         purchases_other = purchases_yt - purchases_that,
         holdings_that = case_when(ticker == "TB133" ~ TB133_holdings,
                                   ticker == "TB137" ~ TB137_holdings,
                                   TRUE ~ as.numeric(NA)),
         outstanding_that = case_when(ticker == "TB133" ~ TB133_outstanding,
                                      ticker == "TB137" ~ TB137_outstanding,
                                      TRUE ~ as.numeric(NA)),
         share_freefloat_that = purchases_that / (outstanding_that - holdings_that) * 100,
         share_freefloat_other = purchases_other / (outstanding_that - holdings_that) * 100) %>%
  select(-all_of(metacols[-4])) %>%
  arrange(ticker, date) %>%
  group_by(ticker) %>%
  mutate(close = close * 100,
         close_ois_3m = close_ois_3m * 100,
         close_ags_10y = close_ags_10y * 100,
         open = open * 100,
         open_ois_3m = open_ois_3m * 100,
         open_ags_10y = open_ags_10y * 100,
         ags_3y_1d = close - open,
         ags_3y_2d = lead(close) - open,
         ags_3y_3d = lead(close, 2) - open,
         ags_3y_4d = lead(close, 3) - open,
         ois_3m_1d = close_ois_3m - open_ois_3m,
         ois_3m_2d = lead(close_ois_3m) - open_ois_3m,
         ois_3m_3d = lead(close_ois_3m, 2) - open_ois_3m,
         ois_3m_4d = lead(close_ois_3m, 3) - open_ois_3m,
         ags_10y_1d = close_ags_10y - open_ags_10y,
         ags_10y_2d = lead(close_ags_10y) - open_ags_10y,
         ags_10y_3d = lead(close_ags_10y, 2) - open_ags_10y,
         ags_10y_4d = lead(close_ags_10y, 3) - open_ags_10y,
         purchases_yt_p1 = replace_na(lead(purchases_yt), 0),
         purchases_yt_p2 = purchases_yt_p1 + replace_na(lead(purchases_yt, 2), 0),
         purchases_yt_p3 = purchases_yt_p2 + replace_na(lead(purchases_yt, 3), 0),
         purchased_yt_p1 = if_else(purchases_yt_p1 == 0, FALSE, TRUE),
         purchased_yt_p2 = if_else(purchases_yt_p2 == 0, FALSE, TRUE),
         purchased_yt_p3 = if_else(purchases_yt_p3 == 0, FALSE, TRUE)) %>%
  ungroup()

### 2.1.2. OIS rates ####

yt_ois <- ois %>%
  mutate(type = case_when(maturity_months == 36 ~ "ois_3y",
                          maturity_months == 3 ~ "ois_3m",
                          TRUE ~ as.character(NA))) %>%
  filter(!is.na(type)) %>%
  select(-maturity_months, -change) %>%
  gather("value_type", "value", -date, -type) %>%
  unite("type", value_type, type) %>%
  spread(type, value) %>%
  left_join(yields_ags_generic %>%
              filter(type == "10y_Benchmark") %>%
              select(date, close, open) %>%
              rename(close_ags_10y = close,
                     open_ags_10y = open),
            by = "date") %>%
  filter(date %in% yt_ags$date) %>%
  left_join(purchases %>%
              filter(ticker %in% bonds_yt_plus) %>%
              left_join(bond_yt %>%
                          mutate(target = TRUE),
                        by = c("date", "ticker")) %>%
              group_by(date, target) %>%
              summarise(purchases = sum(value) / 1e3,
                        .groups = "drop") %>%
              ungroup() %>%
              spread(target, purchases) %>%
              rename(purchases_that = `TRUE`,
                     purchases_other = `<NA>`),
            by = "date") %>%
  left_join(holdings %>%
              left_join(bond_yt %>%
                          mutate(target = TRUE),
                        by = c("date", "ticker")) %>%
              filter(target == TRUE) %>%
              arrange(date) %>%
              mutate(holdings_that = lag(value) / 1e3) %>%
              select(date, holdings_that),
            by = "date") %>%
  left_join(ags %>%
              left_join(bond_yt %>%
                          mutate(target = TRUE),
                        by = c("date", "ticker")) %>%
              filter(target == TRUE) %>%
              arrange(date) %>%
              mutate(outstanding_that = lag(value) / 1e3) %>%
              select(date, outstanding_that),
            by = "date") %>%
  mutate_if(is.numeric, ~ replace_na(., 0)) %>%
  mutate(share_freefloat_that = purchases_that / (outstanding_that - holdings_that) * 100,
         share_freefloat_other = purchases_other / (outstanding_that - holdings_that) * 100) %>%
  arrange(date) %>%
  mutate(close_ois_3y = close_ois_3y * 100,
         close_ois_3m = close_ois_3m * 100,
         close_ags_10y = close_ags_10y * 100,
         open_ois_3y = open_ois_3y * 100,
         open_ois_3m = open_ois_3m * 100,
         open_ags_10y = open_ags_10y * 100,
         ois_3y_1d = close_ois_3y - open_ois_3y,
         ois_3y_2d = lead(close_ois_3y) - open_ois_3y,
         ois_3y_3d = lead(close_ois_3y, 2) - open_ois_3y,
         ois_3y_4d = lead(close_ois_3y, 3) - open_ois_3y,
         ois_3m_1d = close_ois_3m - open_ois_3m,
         ois_3m_2d = lead(close_ois_3m) - open_ois_3m,
         ois_3m_3d = lead(close_ois_3m, 2) - open_ois_3m,
         ois_3m_4d = lead(close_ois_3m, 3) - open_ois_3m,
         ags_10y_1d = close_ags_10y - open_ags_10y,
         ags_10y_2d = lead(close_ags_10y) - open_ags_10y,
         ags_10y_3d = lead(close_ags_10y, 2) - open_ags_10y,
         ags_10y_4d = lead(close_ags_10y, 3) - open_ags_10y,
         purchases_yt = purchases_that + purchases_other,
         purchases_yt_p1 = replace_na(lead(purchases_yt), 0),
         purchases_yt_p2 = purchases_yt_p1 + replace_na(lead(purchases_yt, 2), 0),
         purchases_yt_p3 = purchases_yt_p2 + replace_na(lead(purchases_yt, 3), 0),
         purchased_yt_p1 = if_else(purchases_yt_p1 == 0, FALSE, TRUE),
         purchased_yt_p2 = if_else(purchases_yt_p2 == 0, FALSE, TRUE),
         purchased_yt_p3 = if_else(purchases_yt_p3 == 0, FALSE, TRUE))

## 2.2. For graphs ####

### 2.2.1. Introduction of the yield target ####

gd_yt_begin <- yields_ags_intraday_es_mf %>%
  mutate(time = as_hms(datetime)) %>%
  filter(date == date_mf_before & ticker == "TB133" & hour(time) > 7 & !(hour(time) == 8 & minute(time) < 30) & hour(time) < 17 & !(hour(time) == 16 & minute(time) > 30)) %>%
  mutate(time = str_sub(as.character(time), 1, 5),
         `AGS yield` = value * 100,
         `3-year OIS rate` = mapply(interpolate_ois_intraday, date, datetime, maturity) * 100)

### 2.2.2. Yield target and bond purchases ####

gd_yt_purchases <- days(date_min, today()) %>%
  left_join(bbg %>%
              select(date, aud_zcy_3y, aud_ois_3y),
            by = "date") %>%
  left_join(bond_yt,
            by = "date") %>%
  mutate(ticker = na.locf(ticker, na.rm = FALSE)) %>%
  left_join(yields_ags,
            by = c("date", "ticker")) %>%
  mutate(ois = mapply(interpolate_ois_daily, date, maturity),
         `AGS yield` = case_when(date < min(bond_yt$date) ~ aud_zcy_3y,
                                 date >= min(bond_yt$date) ~ value,
                                 TRUE ~ as.numeric(NA)),
         `OIS rate` = case_when(date < min(bond_yt$date) ~ aud_ois_3y,
                                date >= min(bond_yt$date) ~ ois,
                                TRUE ~ as.numeric(NA))) %>%
  select(date, `AGS yield`, `OIS rate`) %>%
  gather("type", "value", -date)

### 2.2.3. On-the-day changes in target bond yields on purchase days vs other days ####

gd_yt_otd <- yt_ags %>%
  filter(date >= date_yt_begin & date <= date_yt_end) %>%
  mutate(type = case_when(purchases_yt > 0 ~ "Purchase days",
                          purchases_yt == 0 ~ "Other days",
                          TRUE ~ as.character(NA))) %>%
  group_by(type) %>%
  summarise(change_mean = mean(ags_3y_1d),
            change_sd = sd(ags_3y_1d),
            .groups = "drop") %>%
  ungroup() %>%
  arrange(desc(type)) %>%
  mutate(change_lower = change_mean - change_sd,
         change_upper = change_mean + change_sd,
         order = row_number())

### 2.2.4. Yield impact of purchase announcements ####

gd_announcements <- bind_rows(yields_ags_intraday_purchases,
                              yields_semis_intraday_purchases) %>%
  left_join(auctions, by = c("date", metacols)) %>%
  left_join(purchases %>% select(-value), by = c("date", metacols)) %>%
  mutate(category = case_when(purpose == "YT" ~ purpose,
                              purpose_eligible == "QE" & date == as_date("2021-03-01") ~ "MF",
                              !is.na(purpose_eligible) ~ purpose_eligible,
                              TRUE ~ as.character(NA)),
         subcategory = case_when(purpose == "YT" ~ "Yield target purchases",
                                 purpose_eligible == "QE" & date == as_date("2021-03-01") ~ "QE on\n1 March 2021",
                                 !is.na(purpose_eligible) ~ type,
                                 TRUE ~ as.character(NA))) %>%
  filter(!(is.na(category) & is.na(subcategory))) %>%
  left_join(bind_rows(yields_ags_intraday_purchases,
                      yields_semis_intraday_purchases) %>%
              filter(hour(datetime) == 11 & minute(datetime) == 15) %>%
              rename(value_announcement = value) %>%
              select(-datetime, -value_type),
            by = c("date", metacols)) %>%
  mutate(time = as_hms(datetime),
         value_diff = (value - value_announcement) * 100) %>%
  group_by(time, category, subcategory) %>%
  summarise(value_diff_mean = mean(value_diff),
            .groups = "drop") %>%
  ungroup() %>%
  filter(hour(time) > 8 & !(hour(time) == 9 & minute(time) < 40) & hour(time) < 17 & !(hour(time) == 16 & minute(time) > 0)) %>%
  mutate(time = str_sub(as.character(time), 1, 5))

### 2.2.5. Stock lending rate changes ####

gd_sl <- yields_ags_intraday_es_sl %>%
  filter(date %in% dates_es_sl$date & ticker == "TB137" & hour(datetime) >= 8 & !(hour(datetime) == 8 & minute(datetime) < 15) & !(hour(datetime) == 16 & minute(datetime) > 30)) %>%
  mutate(time = case_when(hour(datetime) == 16 & minute(datetime) == 30 ~ format(datetime, "%e %b"),
                          TRUE ~ format(datetime, "%e %b %H:%M")),
         `AGS yield` = value * 100,
         `OIS rate` = mapply(interpolate_ois_intraday, date, datetime, maturity) * 100,
         row = row_number())

### 2.2.6. Removal of the yield target ####

gd_yt_end <- yields_ags_intraday_es_yt %>%
  filter(date %in% dates_es_yt$date & ticker == "TB137" & hour(datetime) >= 8 & !(hour(datetime) == 8 & minute(datetime) < 15) & !(hour(datetime) == 16 & minute(datetime) > 30)) %>%
  mutate(time = case_when(hour(datetime) == 16 & minute(datetime) == 30 ~ format(datetime, "%e %b"),
                          TRUE ~ format(datetime, "%e %b %H:%M")),
         `AGS yield` = value * 100,
         `OIS rate` = mapply(interpolate_ois_intraday, date, datetime, maturity) * 100,
         row = row_number())

# 3. Produce outputs ####

save(yt_ags, yt_ois,
     gd_yt_begin, gd_yt_purchases, gd_yt_otd, gd_announcements, gd_sl, gd_yt_end,
     file = "Processed/yield_target_analysis.Rdata")