# Cross-sectional Model of Flows (to Estimate the Overall Implementation Effect of the RBA's Bond Purchases)
# Michelle Xiang and Dmitry Titkov / MPI / DM
# September 2021

# 0. Preliminaries ####

library(fixest)
library(tidyverse)

# 1. Load data ####

load("Inputs/settings.Rdata")
load("Processed/flow_effects.Rdata")

# 2. Process data ####

## 2.1. Select model data ####

md_mf_ags_cs <- cs_data %>% filter(purpose == "MF" & type == "AGS" & ticker %in% auctions_mf_ags$ticker)
md_qe_ags_cs <- cs_data %>% filter(purpose == "QE" & type == "AGS") %>%
  left_join(auctions_qe %>%
              filter(date == min(auctions_qe$date)) %>%
              distinct(ticker) %>%
              mutate(included = TRUE),
            by = c("ticker")) %>%
  filter(included == TRUE) %>%
  select(-included)

md_mf_semis_spreads_cs <- cs_data %>% filter(purpose == "MF" & type == "Semis" & value_type == "spread" & !(issuer %in% issuers_drop_cs) & ticker %in% auctions_mf_semis$ticker)
md_qe_semis_spreads_cs <- cs_data %>% filter(purpose == "QE" & type == "Semis" & value_type == "spread" & !(issuer %in% issuers_drop_cs)) %>%
  left_join(auctions_qe %>%
              filter(date == min(auctions_qe$date)) %>%
              distinct(ticker) %>%
              mutate(included = TRUE),
            by = c("ticker")) %>%
  filter(included == TRUE) %>%
  select(-included)

## 2.2. Define model equations ####

### 2.2.1. OLS ####

me_ags_cs_ols <- change ~
  share_freefloat +
  share_freefloat_adjacent +
  ttm +
  ttm_squared +
  coupon

me_semis_cs_ols <- change ~
  share_freefloat +
  share_freefloat_adjacent +
  ttm +
  ttm_squared +
  coupon |
  issuer

### 2.2.2. IV ####

#### 2.2.2.1. Market functioning ####

me_mf_ags_cs_iv <- change ~
  ttm +
  ttm_squared +
  coupon |
  share_freefloat +
  share_freefloat_adjacent ~
  futures +
  futures_adjacent +
  ycfe +
  ycfe_adjacent

me_mf_semis_cs_iv <- change ~
  ttm +
  ttm_squared +
  coupon |
  issuer |
  share_freefloat +
  share_freefloat_adjacent ~
  share_holdings +
  share_holdings_adjacent +
  ycfe +
  ycfe_adjacent

#### 2.2.2.2. QE ####

me_qe_ags_cs_iv <- change ~
  ttm +
  ttm_squared +
  coupon |
  share_freefloat +
  share_freefloat_adjacent ~
  share_holdings +
  share_holdings_adjacent +
  futures +
  futures_adjacent +
  ycfe +
  ycfe_adjacent

me_qe_semis_cs_iv <- change ~
  ttm +
  ttm_squared +
  coupon |
  issuer |
  share_freefloat +
  share_freefloat_adjacent ~
  share_holdings +
  share_holdings_adjacent +
  ycfe +
  ycfe_adjacent

## 2.3. Estimate models ####

m_mf_ags_cs_ols <- feols(me_ags_cs_ols, md_mf_ags_cs, vcov = "hetero")
m_qe_ags_cs_ols <- feols(me_ags_cs_ols, md_qe_ags_cs, vcov = "hetero")
m_mf_ags_cs_iv <- feols(me_mf_ags_cs_iv, md_mf_ags_cs %>% filter(ycfe != 0 & ycfe_adjacent != 0), vcov = "hetero")
m_qe_ags_cs_iv <- feols(me_qe_ags_cs_iv, md_qe_ags_cs %>% filter(ycfe != 0 & ycfe_adjacent != 0), vcov = "hetero")

m_mf_semis_spreads_cs_ols <- feols(me_semis_cs_ols, md_mf_semis_spreads_cs, vcov = "hetero")
m_qe_semis_spreads_cs_ols <- feols(me_semis_cs_ols, md_qe_semis_spreads_cs, vcov = "hetero")
m_mf_semis_spreads_cs_iv <- feols(me_mf_semis_cs_iv, md_mf_semis_spreads_cs %>% filter(ycfe != 0 & ycfe_adjacent != 0), vcov = "hetero")
m_qe_semis_spreads_cs_iv <- feols(me_qe_semis_cs_iv, md_qe_semis_spreads_cs %>% filter(ycfe != 0 & ycfe_adjacent != 0), vcov = "hetero")

# 3. Produce outputs ####

save(md_mf_ags_cs, md_qe_ags_cs,
     md_mf_semis_spreads_cs, md_qe_semis_spreads_cs,
     me_ags_cs_ols, me_semis_cs_ols,
     me_mf_ags_cs_iv, me_mf_semis_cs_iv,
     me_qe_ags_cs_iv, me_qe_semis_cs_iv,
     m_mf_ags_cs_ols, m_mf_ags_cs_iv,
     m_qe_ags_cs_ols, m_qe_ags_cs_iv,
     m_mf_semis_spreads_cs_ols, m_mf_semis_spreads_cs_iv,
     m_qe_semis_spreads_cs_ols, m_qe_semis_spreads_cs_iv,
     file = "Outputs/Model Data/cross_sectional_flows.Rdata")