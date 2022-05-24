# Process Data for Market Functioning Impacts (to Use in Graphs and Models of Bid-offer Spreads and Turnover)
# Dmitry Titkov / MPI / DM
# September 2021

# 0. Preliminaries ####

## 0.1. Packages ####

library(lubridate)
library(tidyverse)
library(zoo)

## 0.2. Functions ####

source("define_functions.R")

# 1. Load data ####

load("Inputs/settings.Rdata")
load("Loaded/aofm.Rdata")
load("Loaded/auctions.Rdata")
load("Loaded/austraclear.Rdata")
load("Loaded/findur.Rdata")
load("Loaded/iv.Rdata")
load("Loaded/mops.Rdata")
load("LOaded/yieldbroker.Rdata")

# 2. Process data ####

## 2.1. For graphs ####

### 2.1.1. Bid-offer spreads ####

gd_bo <- bo %>%
  mutate(ttm = as.numeric(maturity - date) / days_year,
         tenor = case_when(ticker %in% bonds_yt ~ "Yield target bonds",
                           type == "AGS" & !(ticker %in% bonds_yt) ~ paste0(round(ttm, 0), "-year"),
                           type == "Semis" & ttm >= 4 & ttm <= 6 ~ "5-year",
                           TRUE ~ as.character(NA))) %>%
  filter(subtype == "Nominals" & gg == FALSE & !is.na(tenor)) %>%
  group_by(date, issuer, tenor) %>%
  summarise(bo = mean(spread),
            .groups = "drop") %>%
  ungroup()

### 2.1.2. Yield curve fitting errors ####

gd_ycfe <- rmse %>%
  mutate(type = if_else(issuer == "Cth", "AGS", "Semis")) %>%
  group_by(date, type) %>%
  summarise(ycfe = mean(rmse),
            .groups = "drop") %>%
  ungroup() %>%
  spread(type, ycfe) %>%
  arrange(date) %>%
  mutate_if(is.numeric, ~ rollmeanr(., 5, na.rm = TRUE, fill = NA)) %>%
  gather("type", "ycfe", -date)

## 2.2. For models of bid-offer spreads and turnover ####

### 2.2.1. Holdings of AGS and semis at the start of each week ####

mf_holdings <- full_join(bind_rows(ags, semis) %>%
                           filter(value != 0) %>%
                           rename(outstanding = value),
                         holdings %>%
                           filter(value != 0) %>%
                           rename(holdings = value),
                         by = c("date", metacols)) %>%
  filter((type == "AGS" & wday(date, label = TRUE) == "Fri") | (type == "Semis" & wday(date, label = TRUE) == "Tue") &
           !is.na(outstanding) & subtype == "Nominals" & ticker != "TC2XXX" & coupon != 0 & nchar(cusip) > 1 & gg == FALSE) %>%
  mutate(week = date + 7,
         holdings = replace_na(holdings, 0),
         share_holdings = holdings / outstanding * 100) %>%
  select(-date) %>%
  relocate(week) %>%
  filter((type == "AGS" & week >= week_ags_min & week <= week_ags_max) | (type == "Semis" & week >= week_semis_min & week <= week_semis_max))

### 2.2.2. Free-float of AGS and semis purchased in each week ####

mf_purchases <- mf_holdings %>%
  left_join(purchases %>%
              mutate(week = case_when(type == "AGS" ~ week_ags(date),
                                      type == "Semis" ~ week_semis(date),
                                      TRUE ~ as_date(NA))) %>%
              group_by(week, ticker) %>%
              summarise(purchases = sum(value, na.rm = TRUE),
                        .groups = "drop") %>%
              ungroup(),
            by = c("week", "ticker")) %>%
  mutate(purchases = replace_na(purchases, 0),
         share_purchases = purchases / (outstanding - holdings) * 100)

### 2.2.3. Combined data, including measures of market functioning and a dummy for an elevated stock lending rate ####

mf_data <- mf_purchases %>%
  left_join(bo %>%
              mutate(week = case_when(type == "AGS" ~ week_ags(date),
                                      type == "Semis" ~ week_semis(date),
                                      TRUE ~ as_date(NA))) %>%
              group_by(week, ticker) %>%
              summarise(bo = mean(spread, na.rm = TRUE),
                        .groups = "drop") %>%
              ungroup(),
            by = c("week", "ticker")) %>%
  left_join(to %>%
              mutate(week = case_when(type == "AGS" ~ week_ags(date),
                                      type == "Semis" ~ week_semis(date),
                                      TRUE ~ as_date(NA)),
                     value = outright_value + switch_value + EFP_value) %>%
              group_by(week, ticker) %>%
              summarise(to = sum(value, na.rm = TRUE),
                        .groups = "drop") %>%
              ungroup(),
            by = c("week", "ticker")) %>%
  mutate(elevated_sl_rate = case_when(ticker %in% bonds_yt & week >= as_date("2021-03-05") & week <= as_date("2021-05-07") ~ TRUE,
                                      ticker %in% bonds_yt & week >= as_date("2021-10-22") & week <= week_yt_end ~ TRUE,
                                      TRUE ~ FALSE),
         share_holdings_10pc = share_holdings / 10,
         share_holdings_squared = share_holdings ^ 2,
         share_purchases_10pc = share_purchases / 10,
         type = if_else((ticker %in% bonds_yt) & week >= week_yt_begin & week <= week_yt_end, "Yield target purchases", type))

# 3. Produce outputs ####

save(gd_bo, gd_ycfe,
     mf_data,
     auctions_mf_ags, auctions_mf_semis,
     auctions_qe_ags, auctions_qe_semis, auctions_qe,
     file = "Processed/market_functioning_impacts.Rdata")