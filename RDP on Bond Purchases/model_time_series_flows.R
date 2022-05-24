# Time-series Model of Flows (to Estimate the Marginal Implementation Effect of the RBA's Bond Purchases)
# Michelle Xiang and Dmitry Titkov / MPI / DM
# September 2021

# 0. Preliminaries ####

library(fixest)
library(lubridate)
library(sandwich)
library(tidyverse)

# 1. Load data ####

load("Inputs/settings.Rdata")
load("Processed/flow_effects.Rdata")

# 2. Process data ####

## 2.1. Select model data ####

md_mf_ags_ts <- ts_data %>% filter(date >= date_mf_begin & date <= date_mf_end & type == "AGS" & ticker %in% auctions_mf_ags$ticker)
md_qe_ags_ts <- ts_data %>% filter(date >= date_qe_begin & date <= date_qe_end & type == "AGS") %>%
  left_join(auctions_qe %>%
              mutate(included = TRUE),
            by = c("date", "ticker")) %>%
  filter(included == TRUE) %>%
  select(-included)

md_mf_semis_spreads_ts <- ts_data %>% filter(date >= date_mf_begin & date <= date_mf_end & type == "Semis" & value_type == "spread" & !(issuer %in% issuers_drop_ts) & ticker %in% auctions_mf_semis$ticker)
md_qe_semis_spreads_ts <- ts_data %>% filter(date >= date_qe_begin & date <= date_qe_end & type == "Semis" & value_type == "spread" & !(issuer %in% issuers_drop_ts)) %>%
  left_join(auctions_qe %>%
              mutate(included = TRUE),
            by = c("date", "ticker")) %>%
  filter(included == TRUE) %>%
  select(-included)

## 2.2. Define model equations ####

### 2.2.1. OLS ####

me_ts_ols <- change_1d ~
  share_freefloat +
  share_freefloat_adjacent |
  ticker +
  date

### 2.2.2. IV ####

me_ts_iv <- change_1d ~
  1 |
  ticker +
  date |
  share_freefloat +
  share_freefloat_adjacent ~
  excluded +
  excluded_adjacent

### 2.2.3. ANOVA ####

me_ts_anova_1d <- change_1d ~
  eligible |
  ticker +
  date

me_ts_anova_2d <- change_2d ~
  eligible +
  purchased_p1 |
  ticker +
  date

me_ts_anova_3d <- change_3d ~
  eligible +
  purchased_p2 |
  ticker +
  date

me_ts_anova_4d <- change_4d ~
  eligible +
  purchased_p3 |
  ticker +
  date

## 2.3. Estimate models ####

### 2.3.1. AGS ####

#### 2.3.1.1. Market functioning ####

##### 2.3.1.1.1. OIS/IV ####

m_mf_ags_ts_ols <- feols(me_ts_ols, md_mf_ags_ts)
m_mf_ags_ts_iv <- feols(me_ts_iv, md_mf_ags_ts)

##### 2.3.1.1.2. ANOVA together ####

m_mf_ags_ts_anova_1d <- feols(me_ts_anova_1d, md_mf_ags_ts %>% mutate(eligible = if_else(excluded == 1, FALSE, TRUE)), vcov = vcovHAC)
m_mf_ags_ts_anova_2d <- feols(me_ts_anova_2d, md_mf_ags_ts %>% mutate(eligible = if_else(excluded == 1, FALSE, TRUE)), vcov = vcovHAC)
m_mf_ags_ts_anova_3d <- feols(me_ts_anova_3d, md_mf_ags_ts %>% mutate(eligible = if_else(excluded == 1, FALSE, TRUE)), vcov = vcovHAC)
m_mf_ags_ts_anova_4d <- feols(me_ts_anova_4d, md_mf_ags_ts %>% mutate(eligible = if_else(excluded == 1, FALSE, TRUE)), vcov = vcovHAC)

#### 2.3.1.2. QE ####

##### 2.3.1.2.1. OIS/IV ####

m_qe_ags_ts_ols <- feols(me_ts_ols, md_qe_ags_ts)
m_qe_ags_ts_iv <- feols(me_ts_iv, md_qe_ags_ts)

##### 2.3.1.2.2. ANOVA separately ####

m_qe_ags_ts_anova_short <- feols(me_ts_anova_1d, md_qe_ags_ts %>%
                                   left_join(auctions_qe_ags_unexcluded %>%
                                               mutate(unexcluded = TRUE),
                                             by = c("date", metacols)) %>%
                                   filter(wday(date, label = TRUE) == "Mon" & unexcluded == TRUE) %>%
                                   mutate(eligible = if_else(excluded == 1, FALSE, TRUE)),
                                 vcov = "hetero")

m_qe_ags_ts_anova_long <- feols(me_ts_anova_1d, md_qe_ags_ts %>%
                                  left_join(auctions_qe_ags_unexcluded %>%
                                              mutate(unexcluded = TRUE),
                                            by = c("date", metacols)) %>%
                                  filter(wday(date, label = TRUE) == "Thu" & unexcluded == TRUE) %>%
                                  mutate(eligible = if_else(excluded == 1, FALSE, TRUE)),
                                vcov = "hetero")

##### 2.3.1.2.3. ANOVA together ####

m_qe_ags_ts_anova_1d <- feols(me_ts_anova_1d, md_qe_ags_ts %>% mutate(eligible = if_else(excluded == 1, FALSE, TRUE)), vcov = vcovHAC)
m_qe_ags_ts_anova_2d <- feols(me_ts_anova_2d, md_qe_ags_ts %>% mutate(eligible = if_else(excluded == 1, FALSE, TRUE)), vcov = vcovHAC)
m_qe_ags_ts_anova_3d <- feols(me_ts_anova_3d, md_qe_ags_ts %>% mutate(eligible = if_else(excluded == 1, FALSE, TRUE)), vcov = vcovHAC)
m_qe_ags_ts_anova_4d <- feols(me_ts_anova_4d, md_qe_ags_ts %>% mutate(eligible = if_else(excluded == 1, FALSE, TRUE)), vcov = vcovHAC)

### 2.3.2. Semis spreads ####

#### 2.3.2.1. Market functioning ####

##### 2.3.2.1.1. OIS/IV ####

m_mf_semis_spreads_ts_ols <- feols(me_ts_ols, md_mf_semis_spreads_ts)
m_mf_semis_spreads_ts_iv <- feols(me_ts_iv, md_mf_semis_spreads_ts)

##### 2.3.2.1.2. ANOVA together ####

m_mf_semis_spreads_ts_anova_1d <- feols(me_ts_anova_1d, md_mf_semis_spreads_ts %>% mutate(eligible = if_else(excluded == 1, FALSE, TRUE)), vcov = vcovHAC)
m_mf_semis_spreads_ts_anova_2d <- feols(me_ts_anova_2d, md_mf_semis_spreads_ts %>% mutate(eligible = if_else(excluded == 1, FALSE, TRUE)), vcov = vcovHAC)
m_mf_semis_spreads_ts_anova_3d <- feols(me_ts_anova_3d, md_mf_semis_spreads_ts %>% mutate(eligible = if_else(excluded == 1, FALSE, TRUE)), vcov = vcovHAC)
m_mf_semis_spreads_ts_anova_4d <- feols(me_ts_anova_4d, md_mf_semis_spreads_ts %>% mutate(eligible = if_else(excluded == 1, FALSE, TRUE)), vcov = vcovHAC)

#### 2.3.2.2. QE ####

##### 2.3.2.2.1. OIS/IV ####

m_qe_semis_spreads_ts_ols <- feols(me_ts_ols, md_qe_semis_spreads_ts)
m_qe_semis_spreads_ts_iv <- feols(me_ts_iv, md_qe_semis_spreads_ts)

##### 2.3.2.2.2. ANOVA separately ####

m_qe_semis_spreads_ts_anova_short <- feols(me_ts_anova_1d, md_qe_semis_spreads_ts %>%
                                             left_join(auctions_qe_semis_unexcluded %>%
                                                         mutate(unexcluded = TRUE),
                                                       by = c("date", metacols)) %>%
                                             filter(date <= as_date("2021-03-17") & year(maturity_min) <= 2025) %>%
                                             mutate(eligible = if_else(excluded == 1, FALSE, TRUE)),
                                           vcov = "hetero")

m_qe_semis_spreads_ts_anova_long <- feols(me_ts_anova_1d, md_qe_semis_spreads_ts %>%
                                            left_join(auctions_qe_semis_unexcluded %>%
                                                        mutate(unexcluded = TRUE),
                                                      by = c("date", metacols)) %>%
                                            filter(date <= as_date("2021-03-17") & year(maturity_min) >= 2028) %>%
                                            mutate(eligible = if_else(excluded == 1, FALSE, TRUE)),
                                          vcov = "hetero")

m_qe_semis_spreads_ts_anova_combined <- feols(me_ts_anova_1d, md_qe_semis_spreads_ts %>%
                                                left_join(auctions_qe_semis_unexcluded %>%
                                                            mutate(unexcluded = TRUE),
                                                          by = c("date", metacols)) %>%
                                                filter(date > as_date("2021-03-17") & wday(date, label = TRUE) == "Wed" & unexcluded == TRUE) %>%
                                                mutate(eligible = if_else(excluded == 1, FALSE, TRUE)),
                                              vcov = "hetero")

##### 2.3.2.2.3. ANOVA together ####

m_qe_semis_spreads_ts_anova_1d <- feols(me_ts_anova_1d, md_qe_semis_spreads_ts %>% mutate(eligible = if_else(excluded == 1, FALSE, TRUE)), vcov = vcovHAC)
m_qe_semis_spreads_ts_anova_2d <- feols(me_ts_anova_2d, md_qe_semis_spreads_ts %>% mutate(eligible = if_else(excluded == 1, FALSE, TRUE)), vcov = vcovHAC)
m_qe_semis_spreads_ts_anova_3d <- feols(me_ts_anova_3d, md_qe_semis_spreads_ts %>% mutate(eligible = if_else(excluded == 1, FALSE, TRUE)), vcov = vcovHAC)
m_qe_semis_spreads_ts_anova_4d <- feols(me_ts_anova_4d, md_qe_semis_spreads_ts %>% mutate(eligible = if_else(excluded == 1, FALSE, TRUE)), vcov = vcovHAC)

# 3. Produce outputs ####

save(md_mf_ags_ts, md_qe_ags_ts,
     md_mf_semis_spreads_ts, md_qe_semis_spreads_ts,
     me_ts_ols, me_ts_iv,
     me_ts_anova_1d, me_ts_anova_2d, me_ts_anova_3d, me_ts_anova_4d,
     m_mf_ags_ts_ols, m_mf_ags_ts_iv,
     m_qe_ags_ts_ols, m_qe_ags_ts_iv,
     m_mf_ags_ts_anova_1d, m_mf_ags_ts_anova_2d, m_mf_ags_ts_anova_3d, m_mf_ags_ts_anova_4d,
     m_qe_ags_ts_anova_1d, m_qe_ags_ts_anova_2d, m_qe_ags_ts_anova_3d, m_qe_ags_ts_anova_4d,
     m_qe_ags_ts_anova_short, m_qe_ags_ts_anova_long,
     m_mf_semis_spreads_ts_ols, m_mf_semis_spreads_ts_iv,
     m_qe_semis_spreads_ts_ols, m_qe_semis_spreads_ts_iv,
     m_mf_semis_spreads_ts_anova_1d, m_mf_semis_spreads_ts_anova_2d, m_mf_semis_spreads_ts_anova_3d, m_mf_semis_spreads_ts_anova_4d,
     m_qe_semis_spreads_ts_anova_1d, m_qe_semis_spreads_ts_anova_2d, m_qe_semis_spreads_ts_anova_3d, m_qe_semis_spreads_ts_anova_4d,
     m_qe_semis_spreads_ts_anova_short, m_qe_semis_spreads_ts_anova_long, m_qe_semis_spreads_ts_anova_combined,
     file = "Outputs/Model Data/time_series_flows.Rdata")