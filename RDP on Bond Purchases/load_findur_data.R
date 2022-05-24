# Load Findur Data (on the RBA's Purchases and Holdings of AGS and Semis, and Generic AGS Yields)
# Dmitry Titkov / MPI / DM
# July 2021

# 0. Preliminaries ####

## 0.1. Packages ####

library(lubridate)
library(odbc)
library(tidyverse)

## 0.2. Functions ####

source("define_functions.R")

# 1. Load data ####

conn_findur <- dbConnect(odbc(), "Findur_R")

## 1.1. Outright trades ####

outrights <- dbGetQuery(conn_findur,
                        "select
                        t.trade_date as date,
                        ti.value as purpose,
                        i.issuer as issuer,
                        i.issue_type as type,
                        i.maturity_date as maturity,
                        i.rate as coupon,
                        t.position as value
                        from
                        ab_tran as t
                        inner join ab_tran_info as ti on ti.tran_num = t.tran_num
                        inner join vw_USER_Issues_all as i on i.ins_num = t.ins_num
                        where
                        ti.value in ('QE', 'Bond Purchase Programme', 'Liquidity', 'Long-dated Liquidity Ops', 'Small Parcel', 'Other Outrights', 'Switches')
                        and t.asset_type = 2
                        and t.internal_portfolio not in (20045, 20046)
                        and t.tran_status in (3, 4)") %>%
  as_tibble() %>%
  mutate(date = as_date(date),
         maturity = as_date(maturity),
         purpose = case_when(purpose == "QE" ~ "qe",
                             purpose == "Bond Purchase Programme" ~ "ytmf",
                             TRUE ~ "other"),
         issuer = case_when(issuer == "ACTT ISSUER" ~ "ACT",
                            issuer == "AUSC ISSUER" ~ "Cth",
                            issuer == "NSWT ISSUER" ~ "NSW",
                            issuer == "NTTY ISSUER" ~ "NT",
                            str_detect(issuer, "QLD") ~ "Qld",
                            issuer == "SAFA ISSUER" ~ "SA",
                            issuer == "TASC ISSUER" ~ "Tas",
                            issuer == "TCVM ISSUER" ~ "Vic",
                            str_detect(issuer, "WAT") ~ "WA",
                            TRUE ~ as.character(NA)),
         coupon = case_when(type %in% c("TN", "PN") ~ as.numeric(NA),
                            TRUE ~ coupon)) %>%
  filter(!is.na(issuer)) %>%
  select(-type) %>%
  group_by(date, purpose, issuer, maturity, coupon) %>%
  summarise(value_sum = sum(value),
            .groups = "drop") %>%
  ungroup() %>%
  rename(value = value_sum)

## 1.2. Generic AGS yields ####

yields_ags_generic <- dbGetQuery(conn_findur,
                                 "select *
                                 from USER_DM_YLD_IntradayGeneric
                                 where convert(time, Time) in ('16:30', '08:40')") %>%
  as_tibble() %>%
  mutate(date = as_date(Date),
         time = case_when(hour(Time) == 16 ~ "close",
                          hour(Time) == 8 ~ "open",
                          TRUE ~ as.character(NA))) %>%
  rename(type = Type) %>%
  select(date, type, time, Yield) %>%
  spread(time, Yield) %>%
  mutate(change = (close - open) * 100)

dbDisconnect(conn_findur)

# 2. Process data ####

## 2.1. Purchases for market functioning, the yield target and QE ####

purchases <- outrights %>%
  filter(purpose %in% c("qe", "ytmf")) %>%
  left_join(mops::load_ags_semis_metadata(),
            by = c("issuer", "maturity", "coupon")) %>%
  mutate(purpose = case_when(purpose == "qe" ~ "QE",
                             purpose == "ytmf" &
                               date > date_mf_end &
                               date <= date_yt_end &
                               ticker %in% bonds_yt_plus ~ "YT",
                             TRUE ~ "MF"))

## 2.2. Holdings ####

holdings <- outrights %>%
  select(-purpose) %>%
  group_by(date, issuer, maturity, coupon) %>%
  summarise(value_sum = sum(value),
            .groups = "drop") %>%
  ungroup() %>%
  unite("issuer_maturity_coupon", issuer, maturity, coupon) %>%
  spread(issuer_maturity_coupon, value_sum) %>%
  right_join(days(min(outrights$date), max(outrights$date)),
             by = "date") %>%
  arrange(date) %>%
  mutate_if(is.numeric, ~ cumsum(replace_na(., 0))) %>%
  gather("issuer_maturity_coupon", "value", -date) %>%
  separate(issuer_maturity_coupon, c("issuer", "maturity", "coupon"), sep = "_") %>%
  mutate(maturity = as_date(maturity),
         coupon = suppressWarnings(as.numeric(coupon)),
         value = case_when(date > maturity ~ 0,
                           date <= maturity ~ value,
                           TRUE ~ as.numeric(NA))) %>%
  left_join(mops::load_ags_semis_metadata() %>%
              filter(nchar(cusip) > 1),
            by = c("issuer", "maturity", "coupon")) %>%
  mutate(value = if_else(value < 0.05, 0, value))

# 3. Produce outputs ####

save(outrights, purchases, holdings,
     yields_ags_generic,
     file = "Loaded/findur.Rdata")