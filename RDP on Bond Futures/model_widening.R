# Model ASX Widening
# Dmitry Titkov
# February 2025

# 0. Preliminaries ####

## 0.1. Packages ####

library(fixest)
library(lubridate)
library(tidyverse)

## 0.2. Functions ####

source("define_functions.R")

# 1. Load data ####

load("Inputs/settings_and_metadata.Rdata")
load("Intermediates/dates.Rdata")
load("Intermediates/indicators.Rdata")

# 2. Process data ####

## 2.1. Graph data ####

gd_widening <- bind_rows(
  daily_3 %>% mutate(tenor = "3-year"),
  daily_10 %>% mutate(tenor = "10-year")
) %>%
  filter(date >= month_start(date_widening_min) & date <= month_end(date_widening_max)) %>%
  mutate(
    bo = if_else(is.na(BidAsk), BidAsk1, BidAsk) * 100,
    bd = tDepth / 2 / 1e3,
    to = tTurnover / 1e3,
    pi = beta_comb * 1e6
  )

## 2.2. Model data ####

md_widening <- bind_rows(
  intraday_3 %>% mutate(tenor = "3-year"),
  intraday_10 %>% mutate(tenor = "10-year")
) %>%
  left_join(
    dates_events %>%
      select(-day) %>%
      mutate_if(is.logical, ~ case_when(
        . == TRUE ~ 1,
        TRUE ~ 0
      )),
    by = "date"
  ) %>%
  filter(date >= date_widening_min & date <= date_widening_max & time >= time_min & time <= time_max & metadata == 0) %>%
  mutate(
    treatment = case_when(
      tenor == "3-year" ~ 1,
      tenor == "10-year" ~ 0,
      TRUE ~ as.numeric(NA)
    ),
    post = case_when(
      date >= date_widening ~ 1,
      TRUE ~ 0
    ),
    posttreatment = post * treatment
  ) %>%
  mutate(
    bo = BidAsk1 * 100,
    bd = log(tDepth1 / 2 / 1e3),
    to = tTurnover1 / 1e3,
    pi = beta1 * 1e6
  )

## 2.3. Models ####

m_date_widening <- date_widening - 1 # https://www.asxonline.com/public/notices/2022/august/0863.22.08.html

### 2.3.1. Difference in differences ####

m_widening_bo_did <- feols(bo ~ i(date, treatment, m_date_widening) | factor(date) + factor(time) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(minutes) + factor(rbnz), cluster = c("date", "time"), data = md_widening)
m_widening_bd_did <- feols(bd ~ i(date, treatment, m_date_widening) | factor(date) + factor(time) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(minutes) + factor(rbnz), cluster = c("date", "time"), data = md_widening)
m_widening_to_did <- feols(to ~ i(date, treatment, m_date_widening) | factor(date) + factor(time) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(minutes) + factor(rbnz), cluster = c("date", "time"), data = md_widening)
m_widening_pi_did <- feols(pi ~ i(date, treatment, m_date_widening) | factor(date) + factor(time) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(minutes) + factor(rbnz), cluster = c("date", "time"), data = md_widening)

### 2.3.2. Average treatment effect on the treated ####

m_widening_bo_att <- feols(bo ~ posttreatment | factor(date) + factor(time) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(minutes) + factor(rbnz), cluster = c("date", "time"), data = md_widening)
m_widening_bo_att_20 <- feols(bo ~ posttreatment | factor(date) + factor(time) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(minutes) + factor(rbnz), cluster = c("date", "time"), data = md_widening %>% filter(date >= date_widening - 20 & date <= date_widening + 20))
m_widening_bo_att_10 <- feols(bo ~ posttreatment | factor(date) + factor(time) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(minutes) + factor(rbnz), cluster = c("date", "time"), data = md_widening %>% filter(date >= date_widening - 10 & date <= date_widening + 10))

m_widening_bd_att <- feols(bd ~ posttreatment | factor(date) + factor(time) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(minutes) + factor(rbnz), cluster = c("date", "time"), data = md_widening)
m_widening_bd_att_20 <- feols(bd ~ posttreatment | factor(date) + factor(time) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(minutes) + factor(rbnz), cluster = c("date", "time"), data = md_widening %>% filter(date >= date_widening - 20 & date <= date_widening + 20))
m_widening_bd_att_10 <- feols(bd ~ posttreatment | factor(date) + factor(time) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(minutes) + factor(rbnz), cluster = c("date", "time"), data = md_widening %>% filter(date >= date_widening - 10 & date <= date_widening + 10))

m_widening_to_att <- feols(to ~ posttreatment | factor(date) + factor(time) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(minutes) + factor(rbnz), cluster = c("date", "time"), data = md_widening)
m_widening_to_att_20 <- feols(to ~ posttreatment | factor(date) + factor(time) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(minutes) + factor(rbnz), cluster = c("date", "time"), data = md_widening %>% filter(date >= date_widening - 20 & date <= date_widening + 20))
m_widening_to_att_10 <- feols(to ~ posttreatment | factor(date) + factor(time) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(minutes) + factor(rbnz), cluster = c("date", "time"), data = md_widening %>% filter(date >= date_widening - 10 & date <= date_widening + 10))

m_widening_pi_att <- feols(pi ~ posttreatment | factor(date) + factor(time) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(minutes) + factor(rbnz), cluster = c("date", "time"), data = md_widening)
m_widening_pi_att_20 <- feols(pi ~ posttreatment | factor(date) + factor(time) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(minutes) + factor(rbnz), cluster = c("date", "time"), data = md_widening %>% filter(date >= date_widening - 20 & date <= date_widening + 20))
m_widening_pi_att_10 <- feols(pi ~ posttreatment | factor(date) + factor(time) + factor(abs) + factor(syndication_pricing) + factor(syndication_announcement) + factor(tender) + factor(decision) + factor(minutes) + factor(rbnz), cluster = c("date", "time"), data = md_widening %>% filter(date >= date_widening - 10 & date <= date_widening + 10))

# 3. Produce outputs ####

save(m_widening_bo_did, m_widening_bo_att, m_widening_bo_att_20, m_widening_bo_att_10,
     m_widening_bd_did, m_widening_bd_att, m_widening_bd_att_20, m_widening_bd_att_10,
     m_widening_to_did, m_widening_to_att, m_widening_to_att_20, m_widening_to_att_10,
     m_widening_pi_did, m_widening_pi_att, m_widening_pi_att_20, m_widening_pi_att_10,
     file = "Intermediates/widening.Rdata"
)
