# Process Data for Flow Effects (to Use in Time-series and Cross-sectional Models)
# Michelle Xiang and Dmitry Titkov / MPI / DM
# July 2021

# 0. Preliminaries ####

library(lubridate)
library(tidyverse)

# 1. Load data ####

load("Inputs/settings.Rdata")
load("Loaded/aofm.Rdata")
load("Loaded/auctions.Rdata")
load("Loaded/austraclear.Rdata")
load("Loaded/findur.Rdata")
load("Loaded/iv.Rdata")
load("Loaded/mops.Rdata")

# 2. Process data ####

## 2.1. For time-series models ####

### 2.1.1. Change in AGS yields and semis yields/spreads over periods from 1 to 4 days ####

ts_change <- bind_rows(yields_ags %>%
                         select(-calc_type, -value_type) %>%
                         mutate(value = value * 100,
                                value_type = "yield"),
                       spreads_semis %>%
                         select(-yield_ags) %>%
                         mutate(yield = yield * 100) %>%
                         gather("value_type", "value", -date, -all_of(metacols))) %>%
  filter(date >= date_mf_before & subtype == "Nominals") %>%
  arrange(ticker, value_type, date) %>%
  group_by(ticker, value_type) %>%
  mutate(change_1d = value - lag(value),
         change_2d = lead(value) - lag(value),
         change_3d = lead(value, 2) - lag(value),
         change_4d = lead(value, 3) - lag(value)) %>%
  ungroup() %>%
  filter(date <= date_max & !is.na(change_1d) & !is.na(change_2d) & !is.na(change_3d) & !is.na(change_4d))

### 2.1.2. Share of the free-float of AGS and semis purchased on each day, plus instrumental variables ####

#### 2.1.2.1. For the 'target' bond ####

ts_freefloat_target <- bind_rows(ags, semis) %>%
  filter(date >= date_mf_before & subtype == "Nominals" & value != 0) %>%
  rename(outstanding_tar = value) %>%
  full_join(holdings %>%
              arrange(ticker, date) %>%
              group_by(ticker) %>%
              mutate(holdings_tar = lag(value)) %>%
              ungroup() %>%
              filter(date >= date_mf_before & subtype == "Nominals" & value != 0),
            by = c("date", metacols)) %>%
  full_join(purchases %>%
              rename(purchases_tar = value),
            by = c("date", metacols)) %>%
  full_join(auctions,
            by = c("date", metacols)) %>%
  mutate(holdings_tar = replace_na(holdings_tar, 0),
         purchases_tar = replace_na(purchases_tar, 0),
         excluded_tar = if_else(is.na(purpose_eligible), 1, 0)) %>%
  filter(date <= date_max)

#### 2.1.2.2. For the 'adjacent' bond ####

ts_freefloat_adjacent <- ts_freefloat_target %>%
  select(date, issuer, maturity, outstanding_tar, holdings_tar, purchases_tar, excluded_tar) %>%
  rename(maturity_adjacent = maturity,
         outstanding_adj = outstanding_tar,
         holdings_adj = holdings_tar,
         purchases_adj = purchases_tar,
         excluded_adj = excluded_tar)

#### 2.1.2.3. For each target bond and its adjacent bonds together ####

ts_freefloat <- left_join(ts_freefloat_target,
                          ts_freefloat_adjacent,
                          by = c("date", "issuer")) %>%
  mutate(maturity_diff = abs(as.numeric(maturity - maturity_adjacent) / days_adjacent),
         outstanding_adj = if_else(maturity_diff <= 1 & maturity_diff != 0, outstanding_adj, 0),
         holdings_adj = if_else(maturity_diff <= 1 & maturity_diff != 0, holdings_adj, 0),
         purchases_adj = if_else(maturity_diff <= 1 & maturity_diff != 0, purchases_adj, 0),
         excluded_adj = if_else(maturity_diff <= 1 & maturity_diff != 0, excluded_adj, as.numeric(NA))) %>%
  group_by(date, across(all_of(metacols))) %>%
  summarise(outstanding = max(outstanding_tar),
            holdings = max(holdings_tar),
            purchases = max(purchases_tar),
            excluded = max(excluded_tar),
            outstanding_adjacent = sum(outstanding_adj),
            holdings_adjacent = sum(holdings_adj),
            purchases_adjacent = sum(purchases_adj),
            excluded_adjacent = mean(excluded_adj, na.rm = TRUE),
            .groups = "drop") %>%
  ungroup() %>%
  mutate(share_freefloat = purchases / (outstanding - holdings) * 100,
         share_freefloat_adjacent = purchases_adjacent / (outstanding - holdings) * 100) %>%
  mutate_if(is.numeric, ~ replace_na(., 0)) %>%
  filter(outstanding != 0)

### 2.1.3. Combined data ####

ts_data <- left_join(ts_change,
                     ts_freefloat,
                     by = c("date", metacols)) %>%
  filter(!is.na(outstanding)) %>% # drops a few days of yields/spreads for a handful of semis where outstandings were not available
  arrange(ticker, date) %>%
  group_by(ticker) %>%
  mutate(purchases_p1 = replace_na(lead(purchases), 0),
         purchases_p2 = purchases_p1 + replace_na(lead(purchases, 2), 0),
         purchases_p3 = purchases_p2 + replace_na(lead(purchases, 3), 0),
         purchased_p1 = if_else(purchases_p1 == 0, FALSE, TRUE),
         purchased_p2 = if_else(purchases_p2 == 0, FALSE, TRUE),
         purchased_p3 = if_else(purchases_p3 == 0, FALSE, TRUE)) %>%
  ungroup()

## 2.2. For cross-sectional models ####

### 2.2.1. Change in AGS yields and semis yields/spreads over the periods of market functioning and QE purchases ####

cs_change <- bind_rows(yields_ags %>%
                         select(-calc_type, -value_type) %>%
                         mutate(value = value * 100,
                                value_type = "yield"),
                       spreads_semis %>%
                         select(-yield_ags) %>%
                         mutate(yield = yield * 100) %>%
                         gather("value_type", "value", -date, -all_of(metacols))) %>%
  filter(date %in% c(date_mf_before, date_mf_end,
                     date_qe_before, date_max_bd) &
           subtype == "Nominals") %>%
  mutate(purpose = case_when(date %in% c(date_mf_before, date_mf_end) ~ "MF",
                             TRUE ~ "QE"),
         date = case_when(date %in% c(date_mf_before, date_qe_before) ~ "start",
                          TRUE ~ "end")) %>%
  spread(date, value) %>%
  mutate(change = end - start) %>%
  filter(!is.na(change))

### 2.2.2. Share of the free-float of AGS and semis purchased in each of the periods, plus instrumental variables ####

#### 2.2.2.1. For the 'target' bond ####

cs_freefloat_target <- bind_rows(ags, semis) %>%
  filter(date %in% c(date_mf_before, date_qe_before) & subtype == "Nominals" & value != 0) %>%
  mutate(purpose = case_when(date == date_mf_before ~ "MF",
                             date == date_qe_before ~ "QE",
                             TRUE ~ as.character(NA))) %>%
  rename(outstanding_start = value) %>%
  select(-date) %>%
  full_join(holdings %>%
              filter(date %in% c(date_mf_before, date_qe_before) & subtype == "Nominals" & value != 0) %>%
              mutate(purpose = case_when(date == date_mf_before ~ "MF",
                                         date == date_qe_before ~ "QE",
                                         TRUE ~ as.character(NA))) %>%
              rename(holdings_start = value) %>%
              select(-date),
            by = c("purpose", metacols)) %>%
  full_join(purchases %>%
              filter(date <= date_max) %>%
              group_by(purpose, ticker) %>%
              summarise(purchases_sum = sum(value), # includes yield target bonds purchased during the market functioning period
                        .groups = "drop") %>%
              ungroup(),
            by = c("purpose", "ticker")) %>%
  filter(!is.na(type)) %>% # drops later yield target purchases and some semis purchases for QE, where the semis were issued after the start of QE
  mutate(holdings_start = replace_na(holdings_start, 0),
         purchases_sum = replace_na(purchases_sum, 0)) %>%
  left_join(baskets %>%
              distinct(purpose, ticker) %>%
              mutate(futures_basket = TRUE),
            by = c("purpose", "ticker")) %>%
  left_join(ycfe %>%
              select(-date),
            by = c("purpose", metacols)) %>%
  mutate(futures_basket = if_else(is.na(futures_basket), 0, 1),
         ycfe_abs = abs(ycfe))

#### 2.2.2.2. For the 'adjacent' bond ####

cs_freefloat_adjacent <- cs_freefloat_target %>%
  select(purpose, issuer, maturity, outstanding_start, holdings_start, purchases_sum, futures_basket, ycfe_abs) %>%
  rename(maturity_adjacent = maturity,
         outstanding_start_adjacent = outstanding_start,
         holdings_start_adjacent = holdings_start,
         purchases_sum_adjacent = purchases_sum,
         futures_basket_adjacent = futures_basket,
         ycfe_abs_adjacent = ycfe_abs)

#### 2.2.2.3. For each target bond and its adjacent bonds together ####

cs_freefloat <- left_join(cs_freefloat_target,
                          cs_freefloat_adjacent,
                          by = c("purpose", "issuer")) %>%
  mutate(maturity_diff = abs(as.numeric(maturity - maturity_adjacent) / days_adjacent),
         outstanding_start_adjacent = if_else(maturity_diff <= 1 & maturity_diff != 0, outstanding_start_adjacent, 0),
         holdings_start_adjacent = if_else(maturity_diff <= 1 & maturity_diff != 0, holdings_start_adjacent, 0),
         purchases_sum_adjacent = if_else(maturity_diff <= 1 & maturity_diff != 0, purchases_sum_adjacent, 0),
         futures_basket_adjacent = if_else(maturity_diff <= 1 & maturity_diff != 0, futures_basket_adjacent, as.numeric(NA)),
         ycfe_abs_adjacent = if_else(maturity_diff <= 1 & maturity_diff != 0, ycfe_abs_adjacent, as.numeric(NA))) %>%
  group_by(purpose, across(all_of(metacols))) %>%
  summarise(outstanding = max(outstanding_start),
            holdings = max(holdings_start),
            purchases = max(purchases_sum),
            futures = max(futures_basket),
            ycfe = max(ycfe_abs),
            outstanding_adjacent = sum(outstanding_start_adjacent),
            holdings_adjacent = sum(holdings_start_adjacent),
            purchases_adjacent = sum(purchases_sum_adjacent),
            futures_adjacent = mean(futures_basket_adjacent, na.rm = TRUE),
            ycfe_adjacent = mean(ycfe_abs_adjacent, na.rm = TRUE),
            .groups = "drop") %>%
  ungroup() %>%
  mutate(share_freefloat = purchases / (outstanding - holdings) * 100,
         share_freefloat_adjacent = purchases_adjacent / (outstanding_adjacent - holdings_adjacent) * 100,
         share_holdings = holdings / outstanding * 100,
         share_holdings_adjacent = holdings_adjacent / outstanding_adjacent * 100) %>%
  mutate_if(is.numeric, ~ replace_na(., 0))

### 2.2.3. Combined data ####

cs_data <- left_join(cs_change,
                     cs_freefloat,
                     by = c("purpose", metacols)) %>%
  mutate(ttm = case_when(purpose == "MF" ~ as.numeric(maturity - date_mf_end) / days_year,
                         TRUE ~ as.numeric(maturity - date_max_bd) / days_year),
         ttm_squared = ttm ^ 2)

# 3. Produce outputs ####

save(ts_data, cs_data, metacols,
     auctions, auctions_metadata,
     auctions_qe_ags_unexcluded,
     auctions_qe_semis_unexcluded,
     auctions_mf_ags, auctions_mf_semis,
     auctions_qe_ags, auctions_qe_semis, auctions_qe,
     file = "Processed/flow_effects.Rdata")