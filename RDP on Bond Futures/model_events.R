# Model ABS, AOFM and RBA Events
# Dmitry Titkov
# November 2024

# 0. Preliminaries ####

library(fixest)
library(hms)
library(tidyverse)

# 1. Load data ####

load("Inputs/settings_and_metadata.Rdata")
load("Inputs/aofm_events.Rdata")
load("Inputs/rba_events.Rdata")
load("Intermediates/dates.Rdata")
load("Intermediates/indicators.Rdata")

# 2. Process data ####

## 2.1. Model data ####

md_3 <- intraday_3 %>%
  filter(time >= time_min & time <= time_max) %>%
  left_join(dates_samples, by = "date")
md_10 <- intraday_10 %>%
  filter(time >= time_min & time <= time_max) %>%
  left_join(dates_samples, by = "date")

md_decisions_3 <- md_3 %>%
  mutate(treatment = case_when(type == "decision" ~ 1, type == "control" ~ 0, TRUE ~ as.numeric(NA))) %>%
  filter(!is.na(treatment))
md_decisions_10 <- md_10 %>%
  mutate(treatment = case_when(type == "decision" ~ 1, type == "control" ~ 0, TRUE ~ as.numeric(NA))) %>%
  filter(!is.na(treatment))

md_releases_3 <- md_3 %>%
  mutate(treatment = case_when(type == "abs" ~ 1, type == "control" ~ 0, TRUE ~ as.numeric(NA))) %>%
  filter(!is.na(treatment))
md_releases_10 <- md_10 %>%
  mutate(treatment = case_when(type == "abs" ~ 1, type == "control" ~ 0, TRUE ~ as.numeric(NA))) %>%
  filter(!is.na(treatment))

md_purchases_3 <- md_3 %>%
  left_join(rba_purchases %>% filter(str_detect(subtype, "qe_ags")), by = "date") %>%
  mutate(treatment = case_when(type == "purchase" & str_detect(subtype, "qe_ags") ~ 1, type == "control" ~ 0, TRUE ~ as.numeric(NA)), amount = replace_na(amount, 0), tenorthree = replace_na(tenorthree, 0), tenorthree = 1 - tenorthree, amount_adj = amount * tenorthree) %>%
  filter(!is.na(treatment))
md_purchases_10 <- md_10 %>%
  left_join(rba_purchases %>% filter(str_detect(subtype, "qe_ags")), by = "date") %>%
  mutate(treatment = case_when(type == "purchase" & str_detect(subtype, "qe_ags") ~ 1, type == "control" ~ 0, TRUE ~ as.numeric(NA)), amount = replace_na(amount, 0), tenorthree = replace_na(tenorthree, 0), amount_adj = amount * tenorthree) %>%
  filter(!is.na(treatment))

md_tenders_3 <- md_3 %>%
  left_join(aofm_tenders, by = "date") %>%
  mutate(treatment = case_when(type == "tender" ~ 1, type == "control" ~ 0, TRUE ~ as.numeric(NA)), amount = replace_na(amount, 0), tenorthree = replace_na(tenorthree, 0), tenorthree = 1 - tenorthree, amount_adj = amount * tenorthree) %>%
  filter(!is.na(treatment))
md_tenders_10 <- md_10 %>%
  left_join(aofm_tenders, by = "date") %>%
  mutate(treatment = case_when(type == "tender" ~ 1, type == "control" ~ 0, TRUE ~ as.numeric(NA)), amount = replace_na(amount, 0), tenorthree = replace_na(tenorthree, 0), amount_adj = amount * tenorthree) %>%
  filter(!is.na(treatment))

## 2.2. Models ####

m_time_decision <- as_hms(time_decision - time_offset)
m_time_release <- as_hms(time_release + time_offset)
m_time_purchase <- as_hms(time_purchase - time_offset)
m_time_tender <- as_hms(time_tender + time_offset)

### 2.2.1. RBA decisions ####

m_decisions_bo_3 <- feols(bo ~ i(time, treatment, m_time_decision) | factor(date) + factor(time), cluster = c("date", "time"), data = md_decisions_3 %>% mutate(bo = BidAsk1 * 100))
m_decisions_bo_10 <- feols(bo ~ i(time, treatment, m_time_decision) | factor(date) + factor(time), cluster = c("date", "time"), data = md_decisions_10 %>% mutate(bo = BidAsk1 * 100))

m_decisions_bd_3 <- feols(bd ~ i(time, treatment, m_time_decision) | factor(date) + factor(time), cluster = c("date", "time"), data = md_decisions_3 %>% mutate(bd = log(tDepth1 / 2 / 1e3)))
m_decisions_bd_10 <- feols(bd ~ i(time, treatment, m_time_decision) | factor(date) + factor(time), cluster = c("date", "time"), data = md_decisions_10 %>% mutate(bd = log(tDepth1 / 2 / 1e3)))

m_decisions_to_3 <- feols(to ~ i(time, treatment, m_time_decision) | factor(date) + factor(time), cluster = c("date", "time"), data = md_decisions_3 %>% mutate(to = tTurnover1 / 1e3))
m_decisions_to_10 <- feols(to ~ i(time, treatment, m_time_decision) | factor(date) + factor(time), cluster = c("date", "time"), data = md_decisions_10 %>% mutate(to = tTurnover1 / 1e3))

m_decisions_pi_3 <- feols(pi ~ i(time, treatment, m_time_decision) | factor(date) + factor(time), cluster = c("date", "time"), data = md_decisions_3 %>% mutate(pi = beta1 * 1e6))
m_decisions_pi_10 <- feols(pi ~ i(time, treatment, m_time_decision) | factor(date) + factor(time), cluster = c("date", "time"), data = md_decisions_10 %>% mutate(pi = beta1 * 1e6))

### 2.2.2. ABS releases ####

m_releases_bo_3 <- feols(bo ~ i(time, treatment, m_time_release) | factor(date) + factor(time), cluster = c("date", "time"), data = md_releases_3 %>% mutate(bo = BidAsk1 * 100))
m_releases_bo_10 <- feols(bo ~ i(time, treatment, m_time_release) | factor(date) + factor(time), cluster = c("date", "time"), data = md_releases_10 %>% mutate(bo = BidAsk1 * 100))

m_releases_bd_3 <- feols(bd ~ i(time, treatment, m_time_release) | factor(date) + factor(time), cluster = c("date", "time"), data = md_releases_3 %>% mutate(bd = log(tDepth1 / 2 / 1e3)))
m_releases_bd_10 <- feols(bd ~ i(time, treatment, m_time_release) | factor(date) + factor(time), cluster = c("date", "time"), data = md_releases_10 %>% mutate(bd = log(tDepth1 / 2 / 1e3)))

m_releases_to_3 <- feols(to ~ i(time, treatment, m_time_release) | factor(date) + factor(time), cluster = c("date", "time"), data = md_releases_3 %>% mutate(to = tTurnover1 / 1e3))
m_releases_to_10 <- feols(to ~ i(time, treatment, m_time_release) | factor(date) + factor(time), cluster = c("date", "time"), data = md_releases_10 %>% mutate(to = tTurnover1 / 1e3))

m_releases_pi_3 <- feols(pi ~ i(time, treatment, m_time_release) | factor(date) + factor(time), cluster = c("date", "time"), data = md_releases_3 %>% mutate(pi = beta1 * 1e6))
m_releases_pi_10 <- feols(pi ~ i(time, treatment, m_time_release) | factor(date) + factor(time), cluster = c("date", "time"), data = md_releases_10 %>% mutate(pi = beta1 * 1e6))

### 2.2.3. RBA purchases ####

m_purchases_bd_3 <- feols(bd ~ i(time, amount_adj, m_time_purchase) | factor(date) + factor(time), cluster = c("date", "time"), data = md_purchases_3 %>% mutate(bd = log(tDepth1 / 2 * 10), amount_adj = amount_adj / 1e3))
m_purchases_bd_10 <- feols(bd ~ i(time, amount_adj, m_time_purchase) | factor(date) + factor(time), cluster = c("date", "time"), data = md_purchases_10 %>% mutate(bd = log(tDepth1 / 2 * 10), amount_adj = amount_adj / 1e3))

m_purchases_to_3 <- feols(to ~ i(time, amount_adj, m_time_purchase) | factor(date) + factor(time), cluster = c("date", "time"), data = md_purchases_3 %>% mutate(to = tTurnover1 * 10))
m_purchases_to_10 <- feols(to ~ i(time, amount_adj, m_time_purchase) | factor(date) + factor(time), cluster = c("date", "time"), data = md_purchases_10 %>% mutate(to = tTurnover1 * 10))

m_purchases_pi_3 <- feols(pi ~ i(time, amount_adj, m_time_purchase) | factor(date) + factor(time), cluster = c("date", "time"), data = md_purchases_3 %>% mutate(pi = beta1 * 1e6, amount_adj = amount_adj / 1e3))
m_purchases_pi_10 <- feols(pi ~ i(time, amount_adj, m_time_purchase) | factor(date) + factor(time), cluster = c("date", "time"), data = md_purchases_10 %>% mutate(pi = beta1 * 1e6, amount_adj = amount_adj / 1e3))

### 2.2.4. AOFM tenders ####

m_tenders_bd_3 <- feols(bd ~ i(time, amount_adj, m_time_tender) | factor(date) + factor(time), cluster = c("date", "time"), data = md_tenders_3 %>% mutate(bd = log(tDepth1 / 2 * 10), amount_adj = amount_adj / 1e3))
m_tenders_bd_10 <- feols(bd ~ i(time, amount_adj, m_time_tender) | factor(date) + factor(time), cluster = c("date", "time"), data = md_tenders_10 %>% mutate(bd = log(tDepth1 / 2 * 10), amount_adj = amount_adj / 1e3))

m_tenders_to_3 <- feols(to ~ i(time, amount_adj, m_time_tender) | factor(date) + factor(time), cluster = c("date", "time"), data = md_tenders_3 %>% mutate(to = tTurnover1 * 10))
m_tenders_to_10 <- feols(to ~ i(time, amount_adj, m_time_tender) | factor(date) + factor(time), cluster = c("date", "time"), data = md_tenders_10 %>% mutate(to = tTurnover1 * 10))

m_tenders_pi_3 <- feols(pi ~ i(time, amount_adj, m_time_tender) | factor(date) + factor(time), cluster = c("date", "time"), data = md_tenders_3 %>% mutate(pi = beta1 * 1e6, amount_adj = amount_adj / 1e3))
m_tenders_pi_10 <- feols(pi ~ i(time, amount_adj, m_time_tender) | factor(date) + factor(time), cluster = c("date", "time"), data = md_tenders_10 %>% mutate(pi = beta1 * 1e6, amount_adj = amount_adj / 1e3))

# 3. Produce outputs ####

save(m_decisions_bo_3, m_decisions_bd_3, m_decisions_to_3, m_decisions_pi_3,
     m_releases_bo_3, m_releases_bd_3, m_releases_to_3, m_releases_pi_3,
     m_purchases_bd_3, m_purchases_to_3, m_purchases_pi_3,
     m_tenders_bd_3, m_tenders_to_3, m_tenders_pi_3,
     m_decisions_bo_10, m_decisions_bd_10, m_decisions_to_10, m_decisions_pi_10,
     m_releases_bo_10, m_releases_bd_10, m_releases_to_10, m_releases_pi_10,
     m_purchases_bd_10, m_purchases_to_10, m_purchases_pi_10,
     m_tenders_bd_10, m_tenders_to_10, m_tenders_pi_10,
     file = "Intermediates/events.Rdata"
)
