# Models of Bid-offer Spreads and Turnover (to Estimate the Market Functioning Impacts of the RBA's Purchases and Holdings of AGS and Semis)
# Dmitry Titkov / MPI / DM
# September 2021

# 0. Preliminaries ####

## 0.1. Packages ####

library(fixest)
library(tidyverse)

## 0.2. Functions ####

source("define_functions.R")

drop_na_all <- function(mf_data, mf_type) {
  mf_data_grouped <- dplyr::group_by(mf_data,
                                     ticker)
  mf_data_summarised <- suppressWarnings(dplyr::summarise(mf_data_grouped,
                                                          infinite_if_na_all = min(!!rlang::sym(mf_type), na.rm = TRUE),
                                                          .groups = "drop"))
  mf_data_ungrouped <- dplyr::ungroup(mf_data_summarised)
  mf_data_na_all <- dplyr::filter(mf_data_ungrouped,
                                  is.infinite(infinite_if_na_all))
  mf_data_filtered <- dplyr::filter(mf_data,
                                    !(ticker %in% mf_data_na_all$ticker))
  return(mf_data_filtered)
}

# 1. Load data ####

load("Inputs/settings.Rdata")
load("Processed/market_functioning_impacts.Rdata")

# 2. Process data ####

## 2.1. Select model data ####

### 2.1.1. General data ####

#### 2.1.1.1. Market functioning ####

md_mf_ags <- mf_data %>%
  filter(type == "AGS" &
           week >= week_ags_mf_begin & week <= week_ags_mf_end &
           ticker %in% auctions_mf_ags$ticker)

md_mf_semis <- mf_data %>%
  filter(type == "Semis" &
           week >= week_semis_mf_begin & week <= week_semis_mf_end &
           ticker %in% auctions_mf_semis$ticker)

#### 2.1.1.2. QE ####

md_qe_ags <- mf_data %>%
  filter(type == "AGS" &
           week >= week_ags_qe_begin & week <= week_ags_qe_end) %>%
  left_join(auctions_qe %>%
              mutate(week = week_ags(date),
                     included = TRUE) %>%
              distinct(week, ticker, included),
            by = c("week", "ticker")) %>%
  filter(included == TRUE) %>%
  select(-included)

md_qe_semis <- mf_data %>%
  filter(type == "Semis" &
           week >= week_semis_qe_begin & week <= week_semis_qe_end) %>%
  left_join(auctions_qe %>%
              mutate(week = week_semis(date),
                     included = TRUE) %>%
              distinct(week, ticker, included),
            by = c("week", "ticker")) %>%
  filter(included == TRUE) %>%
  select(-included)

#### 2.1.1.3. Yield target ####

md_yt <- mf_data %>%
  filter(type == "Yield target purchases" &
           week >= week_yt_begin & week <= week_yt_end)

### 2.1.2. Specific data ####

#### 2.1.2.1. For bid-offer spreads ####

md_mf_ags_bo <- md_mf_ags %>% drop_na_all("bo")
md_qe_ags_bo <- md_qe_ags %>% drop_na_all("bo")

md_mf_semis_bo <- md_mf_semis %>% drop_na_all("bo") %>% filter(!(issuer %in% issuers_drop_bo))
md_qe_semis_bo <- md_qe_semis %>% drop_na_all("bo") %>% filter(!(issuer %in% issuers_drop_bo))

md_yt_bo <- md_yt %>% drop_na_all("bo")

#### 2.1.2.2. For turnover ####

md_mf_ags_to <- md_mf_ags %>% drop_na_all("to") %>% mutate(to_log = log(replace_na(to, to_e)))
md_qe_ags_to <- md_qe_ags %>% drop_na_all("to") %>% mutate(to_log = log(replace_na(to, to_e)))

md_mf_semis_to <- md_mf_semis %>% drop_na_all("to") %>% mutate(to_log = log(replace_na(to, to_e))) %>% filter(!(issuer %in% issuers_drop_to))
md_qe_semis_to <- md_qe_semis %>% drop_na_all("to") %>% mutate(to_log = log(replace_na(to, to_e))) %>% filter(!(issuer %in% issuers_drop_to))

md_yt_to <- md_yt %>% drop_na_all("to") %>% mutate(to_log = log(replace_na(to, to_e)))

## 2.2. Define model equations ####

### 2.2.1. AGS ####

me_ags_bo <- bo ~
  share_purchases_10pc +
  share_holdings_10pc |
  week +
  ticker

me_ags_to <- to_log ~
  share_purchases_10pc +
  share_holdings_10pc |
  week +
  ticker

### 2.2.2. Semis ####

me_semis_bo <- bo ~
  share_purchases_10pc +
  share_holdings_10pc |
  week +
  issuer

me_semis_to <- to_log ~
  share_purchases_10pc +
  share_holdings_10pc |
  week +
  issuer

### 2.2.3. Yield target bonds ####

me_yt_bo <- bo ~
  share_purchases_10pc +
  share_holdings_10pc +
  elevated_sl_rate |
  ticker

me_yt_to <- to_log ~
  share_purchases_10pc +
  share_holdings_10pc +
  elevated_sl_rate |
  ticker

## 2.3. Estimate models ####

### 2.3.1. AGS ####

m_mf_ags_bo <- feols(me_ags_bo, md_mf_ags_bo)
m_qe_ags_bo <- feols(me_ags_bo, md_qe_ags_bo)

m_mf_ags_to <- feols(me_ags_to, md_mf_ags_to)
m_qe_ags_to <- feols(me_ags_to, md_qe_ags_to)

### 2.3.2. Semis ####

m_mf_semis_bo <- feols(me_semis_bo, md_mf_semis_bo)
m_qe_semis_bo <- feols(me_semis_bo, md_qe_semis_bo)

m_mf_semis_to <- feols(me_semis_to, md_mf_semis_to)
m_qe_semis_to <- feols(me_semis_to, md_qe_semis_to)

### 2.3.3. Yield target bonds ####

m_yt_bo <- feols(me_yt_bo, md_yt_bo)
m_yt_to <- feols(me_yt_to, md_yt_to)

# 3. Produce outputs ####

save(md_mf_ags_bo, md_qe_ags_bo, md_mf_semis_bo, md_qe_semis_bo, md_yt_bo,
     md_mf_ags_to, md_qe_ags_to, md_mf_semis_to, md_qe_semis_to, md_yt_to,
     me_ags_bo, me_ags_to,
     me_semis_bo, me_semis_to,
     me_yt_bo, me_yt_to,
     m_mf_ags_bo, m_qe_ags_bo, m_mf_semis_bo, m_qe_semis_bo, m_yt_bo,
     m_mf_ags_to, m_qe_ags_to, m_mf_semis_to, m_qe_semis_to, m_yt_to,
     file = "Outputs/Model Data/bid_offer_spreads_and_turnover.Rdata")