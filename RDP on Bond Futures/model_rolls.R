# Model ASX Rolls
# Dmitry Titkov
# January 2025

# 0. Preliminaries ####

library(fixest)
library(rdlocrand)
library(tidyverse)

# 1. Load data ####

load("Inputs/settings_and_metadata.Rdata")
load("Intermediates/dates.Rdata")
load("Intermediates/indicators.Rdata")

# 2. Process data ####

## 2.1. Relative day estimates ####

### 2.1.1. Model data ####

md_rolls_3 <- daily_3 %>%
  left_join(dates_rolls, by = "date") %>%
  left_join(dates_events %>% select(-day) %>% mutate_if(is.logical, ~ case_when(. == TRUE ~ 1, TRUE ~ 0)), by = "date") %>%
  filter(!is.na(date_roll) & !is.na(BidAsk))
md_rolls_3_wide <- md_rolls_3 %>% filter(date_roll < date_narrow)
md_rolls_3_narrow <- md_rolls_3 %>% filter(date_roll >= date_narrow & date_roll < date_widening)
md_rolls_3_widening <- md_rolls_3 %>% filter(date_roll >= date_widening & date_roll < date_delinked)
md_rolls_3_delinked <- md_rolls_3 %>% filter(date_roll >= date_delinked)

md_rolls_10 <- daily_10 %>%
  left_join(dates_rolls, by = "date") %>%
  left_join(dates_events %>% select(-day) %>% mutate_if(is.logical, ~ case_when(. == TRUE ~ 1, TRUE ~ 0)), by = "date") %>%
  filter(!is.na(date_roll) & !is.na(BidAsk))
md_rolls_10_wide <- md_rolls_10 %>% filter(date_roll < date_narrow)
md_rolls_10_narrow <- md_rolls_10 %>% filter(date_roll >= date_narrow & date_roll < date_delinked)
md_rolls_10_delinked <- md_rolls_10 %>% filter(date_roll >= date_delinked)

### 2.1.2. Models ####

#### 2.1.2.1. 3-year ####

m_rolls_bo_3_wide <- feols(bo ~ factor(day) | factor(date_roll) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(purchase) + factor(rbnz), data = md_rolls_3_wide %>% mutate(bo = BidAsk * 100))
m_rolls_bo_3_narrow <- feols(bo ~ factor(day) | factor(date_roll) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(minutes) + factor(purchase) + factor(rbnz), data = md_rolls_3_narrow %>% mutate(bo = BidAsk * 100))
m_rolls_bo_3_widening <- feols(bo ~ factor(day) | factor(date_roll) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(minutes) + factor(purchase) + factor(rbnz), data = md_rolls_3_widening %>% mutate(bo = BidAsk * 100))
m_rolls_bo_3_delinked <- feols(bo ~ factor(day) | factor(date_roll) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(purchase) + factor(rbnz), data = md_rolls_3_delinked %>% mutate(bo = BidAsk * 100))

m_rolls_bd_3_wide <- feols(bd ~ factor(day) | factor(date_roll) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(purchase) + factor(rbnz), data = md_rolls_3_wide %>% mutate(bd = log(tDepth / 2 / 1e3)))
m_rolls_bd_3_narrow <- feols(bd ~ factor(day) | factor(date_roll) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(minutes) + factor(purchase) + factor(rbnz), data = md_rolls_3_narrow %>% mutate(bd = log(tDepth / 2 / 1e3)))
m_rolls_bd_3_widening <- feols(bd ~ factor(day) | factor(date_roll) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(minutes) + factor(purchase) + factor(rbnz), data = md_rolls_3_widening %>% mutate(bd = log(tDepth / 2 / 1e3)))
m_rolls_bd_3_delinked <- feols(bd ~ factor(day) | factor(date_roll) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(purchase) + factor(rbnz), data = md_rolls_3_delinked %>% mutate(bd = log(tDepth / 2 / 1e3)))

m_rolls_to_3_wide <- feols(to ~ factor(day) | factor(date_roll) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(purchase) + factor(rbnz), data = md_rolls_3_wide %>% mutate(to = tTurnover / 1e3))
m_rolls_to_3_narrow <- feols(to ~ factor(day) | factor(date_roll) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(minutes) + factor(purchase) + factor(rbnz), data = md_rolls_3_narrow %>% mutate(to = tTurnover / 1e3))
m_rolls_to_3_widening <- feols(to ~ factor(day) | factor(date_roll) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(minutes) + factor(purchase) + factor(rbnz), data = md_rolls_3_widening %>% mutate(to = tTurnover / 1e3))
m_rolls_to_3_delinked <- feols(to ~ factor(day) | factor(date_roll) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(purchase) + factor(rbnz), data = md_rolls_3_delinked %>% mutate(to = tTurnover / 1e3))

m_rolls_pi_3_wide <- feols(pi ~ factor(day) | factor(date_roll) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(purchase) + factor(rbnz), data = md_rolls_3_wide %>% mutate(pi = beta_comb * 1e6))
m_rolls_pi_3_narrow <- feols(pi ~ factor(day) | factor(date_roll) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(minutes) + factor(purchase) + factor(rbnz), data = md_rolls_3_narrow %>% mutate(pi = beta_comb * 1e6))
m_rolls_pi_3_widening <- feols(pi ~ factor(day) | factor(date_roll) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(minutes) + factor(purchase) + factor(rbnz), data = md_rolls_3_widening %>% mutate(pi = beta_comb * 1e6))
m_rolls_pi_3_delinked <- feols(pi ~ factor(day) | factor(date_roll) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(purchase) + factor(rbnz), data = md_rolls_3_delinked %>% mutate(pi = beta_comb * 1e6))

#### 2.1.2.2. 10-year ####

m_rolls_bo_10_wide <- feols(bo ~ factor(day) | factor(date_roll) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(purchase) + factor(rbnz), data = md_rolls_10_wide %>% mutate(bo = BidAsk * 100))
m_rolls_bo_10_narrow <- feols(bo ~ factor(day) | factor(date_roll) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(minutes) + factor(purchase) + factor(rbnz), data = md_rolls_10_narrow %>% mutate(bo = BidAsk * 100))
m_rolls_bo_10_delinked <- feols(bo ~ factor(day) | factor(date_roll) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(purchase) + factor(rbnz), data = md_rolls_10_delinked %>% mutate(bo = BidAsk * 100))

m_rolls_bd_10_wide <- feols(bd ~ factor(day) | factor(date_roll) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(purchase) + factor(rbnz), data = md_rolls_10_wide %>% mutate(bd = log(tDepth / 2 / 1e3)))
m_rolls_bd_10_narrow <- feols(bd ~ factor(day) | factor(date_roll) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(minutes) + factor(purchase) + factor(rbnz), data = md_rolls_10_narrow %>% mutate(bd = log(tDepth / 2 / 1e3)))
m_rolls_bd_10_delinked <- feols(bd ~ factor(day) | factor(date_roll) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(purchase) + factor(rbnz), data = md_rolls_10_delinked %>% mutate(bd = log(tDepth / 2 / 1e3)))

m_rolls_to_10_wide <- feols(to ~ factor(day) | factor(date_roll) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(purchase) + factor(rbnz), data = md_rolls_10_wide %>% mutate(to = tTurnover / 1e3))
m_rolls_to_10_narrow <- feols(to ~ factor(day) | factor(date_roll) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(minutes) + factor(purchase) + factor(rbnz), data = md_rolls_10_narrow %>% mutate(to = tTurnover / 1e3))
m_rolls_to_10_delinked <- feols(to ~ factor(day) | factor(date_roll) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(purchase) + factor(rbnz), data = md_rolls_10_delinked %>% mutate(to = tTurnover / 1e3))

m_rolls_pi_10_wide <- feols(pi ~ factor(day) | factor(date_roll) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(purchase) + factor(rbnz), data = md_rolls_10_wide %>% mutate(pi = beta_comb * 1e6))
m_rolls_pi_10_narrow <- feols(pi ~ factor(day) | factor(date_roll) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(minutes) + factor(purchase) + factor(rbnz), data = md_rolls_10_narrow %>% mutate(pi = beta_comb * 1e6))
m_rolls_pi_10_delinked <- feols(pi ~ factor(day) | factor(date_roll) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(purchase) + factor(rbnz), data = md_rolls_10_delinked %>% mutate(pi = beta_comb * 1e6))

## 2.2. Local randomisation estimates ####

### 2.2.1. Model data ####

md_rolls_3_wide_lr <- md_rolls_3_wide %>% mutate(bo = BidAsk * 100, bd = log(tDepth / 2 / 1e3), to = tTurnover / 1e3, pi = beta_comb * 1e6)
md_rolls_3_narrow_lr <- md_rolls_3_narrow %>% mutate(bo = BidAsk * 100, bd = log(tDepth / 2 / 1e3), to = tTurnover / 1e3, pi = beta_comb * 1e6)
md_rolls_3_widening_lr <- md_rolls_3_widening %>% mutate(bo = BidAsk * 100, bd = log(tDepth / 2 / 1e3), to = tTurnover / 1e3, pi = beta_comb * 1e6)
md_rolls_3_delinked_lr <- md_rolls_3_delinked %>% mutate(bo = BidAsk * 100, bd = log(tDepth / 2 / 1e3), to = tTurnover / 1e3, pi = beta_comb * 1e6)

md_rolls_10_wide_lr <- md_rolls_10_wide %>% mutate(bo = BidAsk * 100, bd = log(tDepth / 2 / 1e3), to = tTurnover / 1e3, pi = beta_comb * 1e6)
md_rolls_10_narrow_lr <- md_rolls_10_narrow %>% mutate(bo = BidAsk * 100, bd = log(tDepth / 2 / 1e3), to = tTurnover / 1e3, pi = beta_comb * 1e6)
md_rolls_10_delinked_lr <- md_rolls_10_delinked %>% mutate(bo = BidAsk * 100, bd = log(tDepth / 2 / 1e3), to = tTurnover / 1e3, pi = beta_comb * 1e6)

### 2.2.2. Models ####

#### 2.2.2.1. 3-year ####

m_rolls_bo_3_wide_lr <- rdrandinf(md_rolls_3_wide_lr$bo, md_rolls_3_wide_lr$day, cutoff = -4.5, wl = -9.5, wr = 0.5, interfci = 0.1)
m_rolls_bo_3_narrow_lr <- rdrandinf(md_rolls_3_narrow_lr$bo, md_rolls_3_narrow_lr$day, cutoff = -4.5, wl = -9.5, wr = 0.5, interfci = 0.1)
m_rolls_bo_3_widening_lr <- rdrandinf(md_rolls_3_widening_lr$bo, md_rolls_3_widening_lr$day, cutoff = -4.5, wl = -9.5, wr = 0.5, interfci = 0.1)
m_rolls_bo_3_delinked_lr <- rdrandinf(md_rolls_3_delinked_lr$bo, md_rolls_3_delinked_lr$day, cutoff = -4.5, wl = -9.5, wr = 0.5, interfci = 0.1)

m_rolls_bd_3_wide_lr <- rdrandinf(md_rolls_3_wide_lr$bd, md_rolls_3_wide_lr$day, cutoff = -4.5, wl = -9.5, wr = 0.5, interfci = 0.1)
m_rolls_bd_3_narrow_lr <- rdrandinf(md_rolls_3_narrow_lr$bd, md_rolls_3_narrow_lr$day, cutoff = -4.5, wl = -9.5, wr = 0.5, interfci = 0.1)
m_rolls_bd_3_widening_lr <- rdrandinf(md_rolls_3_widening_lr$bd, md_rolls_3_widening_lr$day, cutoff = -4.5, wl = -9.5, wr = 0.5, interfci = 0.1)
m_rolls_bd_3_delinked_lr <- rdrandinf(md_rolls_3_delinked_lr$bd, md_rolls_3_delinked_lr$day, cutoff = -4.5, wl = -9.5, wr = 0.5, interfci = 0.1)

m_rolls_to_3_wide_lr <- rdrandinf(md_rolls_3_wide_lr$to, md_rolls_3_wide_lr$day, cutoff = -4.5, wl = -9.5, wr = 0.5, interfci = 0.1)
m_rolls_to_3_narrow_lr <- rdrandinf(md_rolls_3_narrow_lr$to, md_rolls_3_narrow_lr$day, cutoff = -4.5, wl = -9.5, wr = 0.5, interfci = 0.1)
m_rolls_to_3_widening_lr <- rdrandinf(md_rolls_3_widening_lr$to, md_rolls_3_widening_lr$day, cutoff = -4.5, wl = -9.5, wr = 0.5, interfci = 0.1)
m_rolls_to_3_delinked_lr <- rdrandinf(md_rolls_3_delinked_lr$to, md_rolls_3_delinked_lr$day, cutoff = -4.5, wl = -9.5, wr = 0.5, interfci = 0.1)

m_rolls_pi_3_wide_lr <- rdrandinf(md_rolls_3_wide_lr$pi, md_rolls_3_wide_lr$day, cutoff = -4.5, wl = -9.5, wr = 0.5, interfci = 0.1)
m_rolls_pi_3_narrow_lr <- rdrandinf(md_rolls_3_narrow_lr$pi, md_rolls_3_narrow_lr$day, cutoff = -4.5, wl = -9.5, wr = 0.5, interfci = 0.1)
m_rolls_pi_3_widening_lr <- rdrandinf(md_rolls_3_widening_lr$pi, md_rolls_3_widening_lr$day, cutoff = -4.5, wl = -9.5, wr = 0.5, interfci = 0.1)
m_rolls_pi_3_delinked_lr <- rdrandinf(md_rolls_3_delinked_lr$pi, md_rolls_3_delinked_lr$day, cutoff = -4.5, wl = -9.5, wr = 0.5, interfci = 0.1)

#### 2.2.2.2. 10-year ####

m_rolls_bo_10_wide_lr <- rdrandinf(md_rolls_10_wide_lr$bo, md_rolls_10_wide_lr$day, cutoff = -4.5, wl = -9.5, wr = 0.5, interfci = 0.1)
m_rolls_bo_10_narrow_lr <- rdrandinf(md_rolls_10_narrow_lr$bo, md_rolls_10_narrow_lr$day, cutoff = -4.5, wl = -9.5, wr = 0.5, interfci = 0.1)
m_rolls_bo_10_delinked_lr <- rdrandinf(md_rolls_10_delinked_lr$bo, md_rolls_10_delinked_lr$day, cutoff = -4.5, wl = -9.5, wr = 0.5, interfci = 0.1)

m_rolls_bd_10_wide_lr <- rdrandinf(md_rolls_10_wide_lr$bd, md_rolls_10_wide_lr$day, cutoff = -4.5, wl = -9.5, wr = 0.5, interfci = 0.1)
m_rolls_bd_10_narrow_lr <- rdrandinf(md_rolls_10_narrow_lr$bd, md_rolls_10_narrow_lr$day, cutoff = -4.5, wl = -9.5, wr = 0.5, interfci = 0.1)
m_rolls_bd_10_delinked_lr <- rdrandinf(md_rolls_10_delinked_lr$bd, md_rolls_10_delinked_lr$day, cutoff = -4.5, wl = -9.5, wr = 0.5, interfci = 0.1)

m_rolls_to_10_wide_lr <- rdrandinf(md_rolls_10_wide_lr$to, md_rolls_10_wide_lr$day, cutoff = -4.5, wl = -9.5, wr = 0.5, interfci = 0.1)
m_rolls_to_10_narrow_lr <- rdrandinf(md_rolls_10_narrow_lr$to, md_rolls_10_narrow_lr$day, cutoff = -4.5, wl = -9.5, wr = 0.5, interfci = 0.1)
m_rolls_to_10_delinked_lr <- rdrandinf(md_rolls_10_delinked_lr$to, md_rolls_10_delinked_lr$day, cutoff = -4.5, wl = -9.5, wr = 0.5, interfci = 0.1)

m_rolls_pi_10_wide_lr <- rdrandinf(md_rolls_10_wide_lr$pi, md_rolls_10_wide_lr$day, cutoff = -4.5, wl = -9.5, wr = 0.5, interfci = 0.1)
m_rolls_pi_10_narrow_lr <- rdrandinf(md_rolls_10_narrow_lr$pi, md_rolls_10_narrow_lr$day, cutoff = -4.5, wl = -9.5, wr = 0.5, interfci = 0.1)
m_rolls_pi_10_delinked_lr <- rdrandinf(md_rolls_10_delinked_lr$pi, md_rolls_10_delinked_lr$day, cutoff = -4.5, wl = -9.5, wr = 0.5, interfci = 0.1)

# 3. Produce outputs ####

save(m_rolls_bo_3_wide, m_rolls_bo_3_narrow, m_rolls_bo_3_widening, m_rolls_bo_3_delinked,
     m_rolls_bo_3_wide_lr, m_rolls_bo_3_narrow_lr, m_rolls_bo_3_widening_lr, m_rolls_bo_3_delinked_lr,
     m_rolls_bd_3_wide, m_rolls_bd_3_narrow, m_rolls_bd_3_widening, m_rolls_bd_3_delinked,
     m_rolls_bd_3_wide_lr, m_rolls_bd_3_narrow_lr, m_rolls_bd_3_widening_lr, m_rolls_bd_3_delinked_lr,
     m_rolls_to_3_wide, m_rolls_to_3_narrow, m_rolls_to_3_widening, m_rolls_to_3_delinked,
     m_rolls_to_3_wide_lr, m_rolls_to_3_narrow_lr, m_rolls_to_3_widening_lr, m_rolls_to_3_delinked_lr,
     m_rolls_pi_3_wide, m_rolls_pi_3_narrow, m_rolls_pi_3_widening, m_rolls_pi_3_delinked,
     m_rolls_pi_3_wide_lr, m_rolls_pi_3_narrow_lr, m_rolls_pi_3_widening_lr, m_rolls_pi_3_delinked_lr,
     m_rolls_bo_10_wide, m_rolls_bo_10_narrow, m_rolls_bo_10_delinked,
     m_rolls_bo_10_wide_lr, m_rolls_bo_10_narrow_lr, m_rolls_bo_10_delinked_lr,
     m_rolls_bd_10_wide, m_rolls_bd_10_narrow, m_rolls_bd_10_delinked,
     m_rolls_bd_10_wide_lr, m_rolls_bd_10_narrow_lr, m_rolls_bd_10_delinked_lr,
     m_rolls_to_10_wide, m_rolls_to_10_narrow, m_rolls_to_10_delinked,
     m_rolls_to_10_wide_lr, m_rolls_to_10_narrow_lr, m_rolls_to_10_delinked_lr,
     m_rolls_pi_10_wide, m_rolls_pi_10_narrow, m_rolls_pi_10_delinked,
     m_rolls_pi_10_wide_lr, m_rolls_pi_10_narrow_lr, m_rolls_pi_10_delinked_lr,
     file = "Intermediates/rolls.Rdata"
)
