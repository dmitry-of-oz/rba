# Model AOFM Syndications
# Dmitry Titkov / MPI / DM
# January 2025

# 0. Preliminaries ####

## 0.1. Packages ####

library(fixest)
library(hms)
library(tidyverse)

## 0.2. Functions ####

source("define_functions.R")

# 1. Load data ####

load("Inputs/settings_and_metadata.Rdata")
load("Inputs/aofm_events.Rdata")
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

md_announcements_3 <- md_3 %>%
  left_join(aofm_syndications %>% group_by(date_announcement) %>% summarise(amount_sum = sum(amount), tenorthree_wm = weighted.mean(tenorthree, amount), .groups = "drop") %>% ungroup() %>% rename(date = date_announcement, amount = amount_sum, tenorthree = tenorthree_wm), by = "date") %>%
  mutate(treatment = case_when(type == "announcement" ~ 1, type == "control" ~ 0, TRUE ~ as.numeric(NA)), amount = replace_na(amount, 0), tenorthree = replace_na(tenorthree, 0), tenorthree = 1 - tenorthree, amount_adj = amount * tenorthree) %>%
  filter(!is.na(treatment))
md_announcements_10 <- md_10 %>%
  left_join(aofm_syndications %>% group_by(date_announcement) %>% summarise(amount_sum = sum(amount), tenorthree_wm = weighted.mean(tenorthree, amount), .groups = "drop") %>% ungroup() %>% rename(date = date_announcement, amount = amount_sum, tenorthree = tenorthree_wm), by = "date") %>%
  mutate(treatment = case_when(type == "announcement" ~ 1, type == "control" ~ 0, TRUE ~ as.numeric(NA)), amount = replace_na(amount, 0), tenorthree = replace_na(tenorthree, 0), amount_adj = amount * tenorthree) %>%
  filter(!is.na(treatment))

md_pricings_3 <- md_3 %>%
  left_join(aofm_syndications %>% mutate(time_pricing = floor_hms(time, 300)) %>% select(-time), by = "date") %>%
  mutate(treatment = case_when(type == "pricing" ~ 1, type == "control" ~ 0, TRUE ~ as.numeric(NA)), amount = replace_na(amount, 0), tenorthree = replace_na(tenorthree, 0), tenorthree = 1 - tenorthree, amount_adj = amount * tenorthree, time_relative = as_hms(time - time_pricing), time_relative = replace_na(time_relative, 0)) %>%
  filter(!is.na(treatment) & time_relative >= time_relative_min & time_relative <= time_relative_max)
md_pricings_10 <- md_10 %>%
  left_join(aofm_syndications %>% mutate(time_pricing = floor_hms(time, 300)) %>% select(-time), by = "date") %>%
  mutate(treatment = case_when(type == "pricing" ~ 1, type == "control" ~ 0, TRUE ~ as.numeric(NA)), amount = replace_na(amount, 0), tenorthree = replace_na(tenorthree, 0), amount_adj = amount * tenorthree, time_relative = as_hms(time - time_pricing), time_relative = replace_na(time_relative, 0)) %>%
  filter(!is.na(treatment) & time_relative >= time_relative_min & time_relative <= time_relative_max)

md_syndications_3 <- daily_3 %>%
  left_join(dates_syndications, by = "date") %>%
  left_join(dates_events %>% select(-day) %>% mutate_if(is.logical, ~ case_when(. == TRUE ~ 1, TRUE ~ 0)), by = "date") %>%
  mutate(tenorthree = 1 - tenorthree, amount_adj = amount * tenorthree) %>%
  filter(!is.na(date_syndication) & amount_adj != 0 & narrow == 0)
md_syndications_10 <- daily_10 %>%
  left_join(dates_syndications, by = "date") %>%
  left_join(dates_events %>% select(-day) %>% mutate_if(is.logical, ~ case_when(. == TRUE ~ 1, TRUE ~ 0)), by = "date") %>%
  mutate(amount_adj = amount * tenorthree) %>%
  filter(!is.na(date_syndication) & amount_adj != 0 & narrow == 0)

## 2.2. Models ####

m_time_announcement <- as_hms(time_announcement + time_offset)
m_time_pricing <- -time_offset

### 2.2.1. Announcements ####

m_announcements_bd_3 <- feols(bd ~ i(time, treatment, m_time_announcement) | factor(date) + factor(time), cluster = c("date", "time"), data = md_announcements_3 %>% mutate(bd = log(tDepth1 / 2 / 1e3)))
m_announcements_bd_10 <- feols(bd ~ i(time, treatment, m_time_announcement) | factor(date) + factor(time), cluster = c("date", "time"), data = md_announcements_10 %>% mutate(bd = log(tDepth1 / 2 / 1e3)))

m_announcements_to_3 <- feols(to ~ i(time, treatment, m_time_announcement) | factor(date) + factor(time), cluster = c("date", "time"), data = md_announcements_3 %>% mutate(to = tTurnover1 / 1e3))
m_announcements_to_10 <- feols(to ~ i(time, treatment, m_time_announcement) | factor(date) + factor(time), cluster = c("date", "time"), data = md_announcements_10 %>% mutate(to = tTurnover1 / 1e3))

m_announcements_pi_3 <- feols(pi ~ i(time, treatment, m_time_announcement) | factor(date) + factor(time), cluster = c("date", "time"), data = md_announcements_3 %>% mutate(pi = beta1 * 1e6))
m_announcements_pi_10 <- feols(pi ~ i(time, treatment, m_time_announcement) | factor(date) + factor(time), cluster = c("date", "time"), data = md_announcements_10 %>% mutate(pi = beta1 * 1e6))

### 2.2.2. Pricings ####

m_pricings_bd_3_treatment <- feols(bd ~ i(time_relative, treatment, m_time_pricing) | factor(date) + factor(time), cluster = c("date", "time"), data = md_pricings_3 %>% mutate(bd = log(bDepth1 / 2 / 1e3)))
m_pricings_bd_10_treatment <- feols(bd ~ i(time_relative, treatment, m_time_pricing) | factor(date) + factor(time), cluster = c("date", "time"), data = md_pricings_10 %>% mutate(bd = log(bDepth1 / 2 / 1e3)))

m_pricings_bd_3 <- feols(bd ~ i(time_relative, amount_adj, m_time_pricing) | factor(date) + factor(time), cluster = c("date", "time"), data = md_pricings_3 %>% mutate(bd = log(bDepth1 / 2 * 10), amount_adj = amount_adj / 1e3))
m_pricings_bd_10 <- feols(bd ~ i(time_relative, amount_adj, m_time_pricing) | factor(date) + factor(time), cluster = c("date", "time"), data = md_pricings_10 %>% mutate(bd = log(bDepth1 / 2 * 10), amount_adj = amount_adj / 1e3))

m_pricings_to_3_treatment <- feols(to ~ i(time_relative, treatment, m_time_pricing) | factor(date) + factor(time), cluster = c("date", "time"), data = md_pricings_3 %>% mutate(to = tTurnover1 / 1e3))
m_pricings_to_10_treatment <- feols(to ~ i(time_relative, treatment, m_time_pricing) | factor(date) + factor(time), cluster = c("date", "time"), data = md_pricings_10 %>% mutate(to = tTurnover1 / 1e3))

m_pricings_to_3 <- feols(to ~ i(time_relative, amount_adj, m_time_pricing) | factor(date) + factor(time), cluster = c("date", "time"), data = md_pricings_3 %>% mutate(to = tTurnover1 * 10))
m_pricings_to_10 <- feols(to ~ i(time_relative, amount_adj, m_time_pricing) | factor(date) + factor(time), cluster = c("date", "time"), data = md_pricings_10 %>% mutate(to = tTurnover1 * 10))

m_pricings_pi_3_treatment <- feols(pi ~ i(time_relative, treatment, m_time_pricing) | factor(date) + factor(time), cluster = c("date", "time"), data = md_pricings_3 %>% mutate(pi = beta1 * 1e6))
m_pricings_pi_10_treatment <- feols(pi ~ i(time_relative, treatment, m_time_pricing) | factor(date) + factor(time), cluster = c("date", "time"), data = md_pricings_10 %>% mutate(pi = beta1 * 1e6))

m_pricings_pi_3 <- feols(pi ~ i(time_relative, amount_adj, m_time_pricing) | factor(date) + factor(time), cluster = c("date", "time"), data = md_pricings_3 %>% mutate(pi = beta1 * 1e6, amount_adj = amount_adj / 1e3))
m_pricings_pi_10 <- feols(pi ~ i(time_relative, amount_adj, m_time_pricing) | factor(date) + factor(time), cluster = c("date", "time"), data = md_pricings_10 %>% mutate(pi = beta1 * 1e6, amount_adj = amount_adj / 1e3))

### 2.2.3. Periods ####

m_syndications_bd_3 <- feols(bd ~ i(factor(day), amount_adj, -7) | factor(date_syndication) + factor(abs) + factor(tender) + factor(decision) + factor(minutes) + factor(purchase) + factor(rbnz), data = md_syndications_3 %>% mutate(bd = log(tDepth1 / 2 * 10), amount_adj = amount_adj / 1e3))
m_syndications_to_3 <- feols(to ~ i(factor(day), amount_adj, -7) | factor(date_syndication) + factor(abs) + factor(tender) + factor(decision) + factor(minutes) + factor(purchase) + factor(rbnz), data = md_syndications_3 %>% mutate(to = tTurnover1 * 10))
m_syndications_pi_3 <- feols(pi ~ i(factor(day), amount_adj, -7) | factor(date_syndication) + factor(abs) + factor(tender) + factor(decision) + factor(minutes) + factor(purchase) + factor(rbnz), data = md_syndications_3 %>% mutate(pi = beta1 * 1e6, amount_adj = amount_adj / 1e3))

m_syndications_bd_10 <- feols(bd ~ i(factor(day), amount_adj, -7) | factor(date_syndication) + factor(abs) + factor(tender) + factor(decision) + factor(minutes) + factor(purchase) + factor(rbnz), data = md_syndications_10 %>% mutate(bd = log(tDepth1 / 2 * 10), amount_adj = amount_adj / 1e3))
m_syndications_to_10 <- feols(to ~ i(factor(day), amount_adj, -7) | factor(date_syndication) + factor(abs) + factor(tender) + factor(decision) + factor(minutes) + factor(purchase) + factor(rbnz), data = md_syndications_10 %>% mutate(to = tTurnover1 * 10))
m_syndications_pi_10 <- feols(pi ~ i(factor(day), amount_adj, -7) | factor(date_syndication) + factor(abs) + factor(tender) + factor(decision) + factor(minutes) + factor(purchase) + factor(rbnz), data = md_syndications_10 %>% mutate(pi = beta1 * 1e6, amount_adj = amount_adj / 1e3))

# 3. Produce outputs ####

save(m_announcements_bd_3, m_announcements_bd_10,
     m_announcements_to_3, m_announcements_to_10,
     m_announcements_pi_3, m_announcements_pi_10,
     m_pricings_bd_3_treatment, m_pricings_bd_10_treatment,
     m_pricings_to_3_treatment, m_pricings_to_10_treatment,
     m_pricings_pi_3_treatment, m_pricings_pi_10_treatment,
     m_pricings_bd_3, m_pricings_bd_10,
     m_pricings_to_3, m_pricings_to_10,
     m_pricings_pi_3, m_pricings_pi_10,
     m_syndications_bd_3, m_syndications_bd_10,
     m_syndications_to_3, m_syndications_to_10,
     m_syndications_pi_3, m_syndications_pi_10,
     file = "Intermediates/syndications.Rdata"
)
