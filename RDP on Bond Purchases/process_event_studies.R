# Process Data for Event Studies (to Use in Graphs and Analysis)
# Richard Finlay, Dmitry Titkov and Michelle Xiang / MPI / DM
# November 2021

# 0. Preliminaries ####

## 0.1. Packages ####

library(lubridate)
library(tidyverse)
library(zoo)

## 0.2. Functions ####

source("define_functions.R")

interpolate_bbg <- function (date_set, maturity_set, issuer_set, type_set) {
  ttm_set <- as.numeric(maturity_set - date_set) / days_year
  bbg_bonds_renamed <- dplyr::rename(bbg_bonds,
                                     value = !!rlang::sym(type_set))
  bbg_bonds_filtered <- dplyr::filter(bbg_bonds_renamed,
                                      date == date_set & issuer == issuer_set)
  bbg_bonds_mutated <- dplyr::mutate(bbg_bonds_filtered,
                                     ttm = as.numeric(maturity - date) / days_year)
  if (nrow(bbg_bonds_mutated) < 2) {
    return(NA)
  } else {
    return(approx(x = c(bbg_bonds_mutated$ttm, 0),
                  y = c(bbg_bonds_mutated$value, 0),
                  xout = ttm_set,
                  rule = 2)$y)
  }
}

# 1. Load data ####

load("Inputs/settings.Rdata")
load("Loaded/bloomberg.Rdata")
load("Loaded/mops.Rdata")

# 2. Process data ####

## 2.1. For event study graphs and analysis ####

### 2.1.1. Identify required dates ####

dates_es <- yields_ags %>%
  distinct(date) %>%
  arrange(date) %>%
  left_join(bind_rows(dates_es_mf,
                      dates_es_qe %>% select(-event)),
            by = "date") %>%
  mutate(required = case_when(!is.na(window) | (is.na(window) & lead(window) == "close_to_close") ~ TRUE,
                              TRUE ~ FALSE)) %>%
  filter(required == TRUE) %>%
  select(-required)

### 2.1.2. Interpolate AGS and semis yields ####

#### 2.1.2.1. Join up Bloomberg data and metadata ####

bbg_bonds <- left_join(bbg_metadata_bonds %>%
                         as_tibble() %>%
                         rename_all(~ str_to_lower(.)) %>%
                         mutate(issuer = case_when(str_detect(security_short_des, "ACGB") ~ "Cth",
                                                   str_detect(security_short_des, "NSWTC") ~ "NSW",
                                                   str_detect(security_short_des, "TCV") ~ "Vic",
                                                   str_detect(security_short_des, "QTC") ~ "Qld",
                                                   str_detect(security_short_des, "WATC") ~ "WA",
                                                   str_detect(security_short_des, "SAFA") ~ "SA",
                                                   str_detect(security_short_des, "TASCOR") ~ "Tas",
                                                   str_detect(security_short_des, "AUSCAP") ~ "ACT",
                                                   str_detect(security_short_des, "NTTC") ~ "NT",
                                                   TRUE ~ as.character(NA))) %>%
                         bind_cols(enframe(rownames(bbg_metadata_bonds), name = NULL, value = "bbg_code")),
                       metadata,
                       by = c("issuer", "maturity", "coupon")) %>%
  filter(subtype == "Nominals" & ticker != "TC2XXX" & coupon != 0 & nchar(cusip) > 1 & gg == FALSE
         & issuer != "NT" # PX_LAST/PX_OPEN seem to show prices rather than yields
         & !(ticker %in% c("ACT015", "TS3101", "TV3130", "TV3135", "TV3178"))) %>% # ditto
  right_join(bbg_data_bonds,
             by = "bbg_code") %>%
  filter(!is.na(ticker)) %>%
  rename(close = PX_LAST,
         open = PX_OPEN)

#### 2.1.2.2. Estimate closing and opening yields by issuer ####

bbg_yields <- crossing(dates_es %>% select(date),
                       tenors %>% enframe(name = NULL, value = "tenor"),
                       bbg_bonds %>% distinct(issuer),
                       c("close", "open") %>% enframe(name = NULL, value = "type")) %>%
  mutate(maturity = make_date(year(date) + tenor, month(date), day(date)),
         yield = suppressWarnings(mapply(interpolate_bbg, date, maturity, issuer, type))) %>%
  select(-maturity) %>%
  spread(type, yield) %>%
  mutate(open_to_close = (close - open) * 100) %>%
  arrange(tenor, issuer, date) %>%
  group_by(tenor, issuer) %>%
  mutate(close_to_close = (close - lag(close)) * 100) %>%
  ungroup() %>%
  rename(type = issuer) %>%
  gather("window", "change", -date, -tenor, -type)

#### 2.1.2.3. Combined data, including OIS rates ####

bbg_ois <- bbg_data_ois %>%
  filter(date %in% dates_es$date) %>%
  rename(close = PX_LAST,
         open = PX_OPEN) %>%
  mutate(tenor = as.numeric(str_remove(str_remove(bbg_code, "ADSO"), " Curncy")),
         open_to_close = (close - open) * 100) %>%
  select(-bbg_code) %>%
  arrange(tenor, date) %>%
  group_by(tenor) %>%
  mutate(close_to_close = (close - lag(close)) * 100) %>%
  ungroup() %>%
  mutate(type = "OIS") %>%
  gather("window", "change", -date, -tenor, -type)

es_data <- left_join(bind_rows(dates_es_mf %>% mutate(study = "mf"),
                               dates_es_qe %>% mutate(study = "qe")),
                     bind_rows(bbg_yields,
                               bbg_ois),
                     by = c("date", "window"))

### 2.1.3. Calculate changes across event studies ####

#### 2.1.3.1. First ('market functioning') policy package ####

##### 2.1.3.1.1. AGS yields ####

gd_es_mf_ags <- es_data %>%
  filter(study == "mf" & !(tenor %in% c(12, 15, 25)) & type == "Cth") %>%
  group_by(tenor) %>%
  summarise(change_sum = sum(change),
            .groups = "drop") %>%
  ungroup() %>%
  mutate(row = row_number(),
         tenor = as.character(tenor)) %>%
  rename(change = change_sum)

##### 2.1.3.1.2. AGS spreads to OIS ####

gd_es_mf_ois <- es_data %>%
  filter(study == "mf" & !(tenor %in% c(12, 15, 25)) & type %in% c("Cth", "OIS")) %>%
  group_by(tenor, type) %>%
  summarise(change_sum = sum(change),
            .groups = "drop") %>%
  ungroup() %>%
  spread(type, change_sum) %>%
  mutate(change = Cth - OIS,
         row = row_number(),
         tenor = as.character(tenor))

##### 2.1.3.1.3. Semis spreads to AGS ####

gd_es_mf_semis <- es_data %>%
  filter(study == "mf" & !(tenor %in% c(20, 25, 30)) & type != "OIS") %>%
  group_by(tenor, type) %>%
  summarise(change_sum = sum(change),
            .groups = "drop") %>%
  ungroup() %>%
  spread(type, change_sum) %>%
  relocate(Cth, .after = last_col()) %>%
  mutate(tenor = as.character(tenor)) %>%
  mutate_if(is.numeric, ~ . - Cth) %>%
  select(-Cth) %>%
  mutate(row = row_number()) %>%
  gather("issuer", "change", -tenor, -row) %>%
  filter(!(issuer %in% issuers_drop_es)) %>%
  group_by(tenor, row) %>%
  summarise(change_mean = mean(change, na.rm = TRUE),
            change_median = median(change, na.rm = TRUE),
            .groups = "drop") %>%
  ungroup() %>%
  arrange(row)

#### 2.1.3.2. Second ('QE') policy package ####

##### 2.1.3.2.1. AGS yields ####

###### 2.1.3.2.1.1. Total effect across the yield curve ####

gd_es_qe_ags <- es_data %>%
  filter(study == "qe" & !(tenor %in% c(12, 15, 25)) & type == "Cth") %>%
  group_by(tenor) %>%
  summarise(change_sum = sum(change),
            .groups = "drop") %>%
  ungroup() %>%
  mutate(row = row_number(),
         tenor = as.character(tenor)) %>%
  rename(change = change_sum)

###### 2.1.3.2.1.2. Effect by event on the 10-year yield ####

td_10y_es_qe <- es_data %>%
  filter(study == "qe" & tenor == 10 & type == "Cth") %>%
  select(date, window, event, change) %>%
  mutate(change = round(change * 2, 0) / 2)

##### 2.1.3.2.2. AGS spreads to OIS ####

gd_es_qe_ois <- es_data %>%
  filter(study == "qe" & !(tenor %in% c(12, 15, 25)) & type %in% c("Cth", "OIS")) %>%
  group_by(tenor, type) %>%
  summarise(change_sum = sum(change),
            .groups = "drop") %>%
  ungroup() %>%
  spread(type, change_sum) %>%
  mutate(change = Cth - OIS,
         row = row_number(),
         tenor = as.character(tenor))

##### 2.1.3.2.3. Semis spreads to AGS ####

gd_es_qe_semis <- es_data %>%
  filter(study == "qe" & !(tenor %in% c(20, 25, 30)) & type != "OIS") %>%
  group_by(tenor, type) %>%
  summarise(change_sum = sum(change),
            .groups = "drop") %>%
  ungroup() %>%
  spread(type, change_sum) %>%
  relocate(Cth, .after = last_col()) %>%
  mutate(tenor = as.character(tenor)) %>%
  mutate_if(is.numeric, ~ . - Cth) %>%
  select(-Cth) %>%
  mutate(row = row_number()) %>%
  gather("issuer", "change", -tenor, -row) %>%
  filter(!(issuer %in% issuers_drop_es)) %>%
  group_by(tenor, row) %>%
  summarise(change_mean = mean(change, na.rm = TRUE),
            change_median = median(change, na.rm = TRUE),
            .groups = "drop") %>%
  ungroup() %>%
  arrange(row)

## 2.2. For a graph of the long-short difference across the edge of QE-eligibility ####

ls <- bind_rows(crossing(bbg_bonds %>% distinct(date),
                         c(10, 12) %>% enframe(name = NULL, value = "tenor"),
                         bbg_bonds %>% distinct(issuer),
                         c("close") %>% enframe(name = NULL, value = "type")) %>%
                  mutate(maturity = make_date(year(date) + tenor, month(date), day(date)),
                         yield = suppressWarnings(mapply(interpolate_bbg, date, maturity, issuer, type))) %>%
                  select(-maturity),
                bbg_data_ois %>%
                  mutate(tenor = as.numeric(str_remove(str_remove(bbg_code, "ADSO"), " Curncy")),
                         issuer = "OIS rate",
                         type = "close") %>%
                  filter(tenor %in% c(10, 12)) %>%
                  rename(yield = PX_LAST) %>%
                  select(date, tenor, issuer, type, yield)) %>%
  filter(!(issuer %in% issuers_drop_ls)) %>%
  mutate(type = case_when(issuer == "Cth" ~ "AGS yield",
                          issuer == "OIS rate" ~ issuer,
                          TRUE ~ "Semis yield")) %>%
  group_by(date, tenor, type) %>%
  summarise(yield_mean = mean(yield),
            .groups = "drop") %>%
  ungroup() %>%
  spread(tenor, yield_mean) %>%
  mutate(value = (`12` - `10`) * 100) %>%
  select(date, type, value) %>%
  spread(type, value) %>%
  arrange(date) %>%
  mutate_if(is.numeric, ~ rollmeanr(., 5, na.rm = TRUE, fill = NA)) %>%
  gather("type", "value", -date)

gd_ls <- ls %>%
  left_join(ls %>%
              filter(date == date_qe_announcement) %>%
              rename(value_announcement = value) %>%
              select(type, value_announcement),
            by = "type") %>%
  mutate(spread = value - value_announcement)

# 3. Produce outputs ####

save(es_data,
     gd_es_mf_ags, gd_es_mf_ois, gd_es_mf_semis,
     gd_es_qe_ags, gd_es_qe_ois, gd_es_qe_semis,
     td_10y_es_qe,
     gd_ls,
     file = "Processed/event_studies.Rdata")