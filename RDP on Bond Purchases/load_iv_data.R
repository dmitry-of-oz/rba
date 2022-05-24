# Load Instrumental Variables Data (for a Graph and the Cross-sectional Flows Model)
# Michelle Xiang and Dmitry Titkov / MPI / DM
# July 2021

# 0. Preliminaries ####

## 0.1. Packages ####

library(lubridate)
library(readxl)
library(tidyverse)

## 0.2. Function to calculate the root mean square errors of a yield curve fitted for an issuer ####

rmse_issuer <- function (issuer_set, maturity_min, maturity_max) {
  hist.file <- dplyr::filter(hist.files,
                             stringr::str_detect(value, issuer_set))
  load(paste0("Inputs/", hist.file))
  data_tibbled <- dplyr::as_tibble(hist.Ye)
  data_selected <- dplyr::select_if(data_tibbled,
                                    is.numeric)
  data_binded <- dplyr::bind_cols(data_selected,
                                  dplyr::select(data_tibbled, date))
  data_gathered <- tidyr::gather(data_binded,
                                 "id", "error", -date)
  data_joined <- dplyr::left_join(data_gathered,
                                  bloomberg_data,
                                  by = "id")
  data_filtered <- dplyr::filter(data_joined,
                                 maturity >= maturity_min & maturity <= maturity_max)
  data_grouped <- dplyr::group_by(data_filtered,
                                  date)
  data_summarised <- dplyr::summarise(data_grouped,
                                      rmse = sqrt(sum(error^2, na.rm = TRUE) / n()) * 100,
                                      .groups = "drop")
  data_ungrouped <- dplyr::ungroup(data_summarised)
  data_refiltered <- dplyr::filter(data_ungrouped,
                                   rmse != 0)
  data_mutated <- dplyr::mutate(data_refiltered,
                                issuer = issuer_set)
  return(data_mutated)
}

# 1. Load data ####

## 1.1. From ASX ####

baskets_raw <- read_excel("Inputs/baskets.xlsx",
                          sheet = "baskets") %>%
  mutate(basket_month_year = as_date(basket_month_year),
         maturity_month_year = as_date(maturity_month_year))

## 1.2. From Bloomberg ####

### 1.2.1. Metadata ####

load("Inputs/bloomberg_data.Rdata")

### 1.2.2. Data ####

ycfe_raw <- tibble()

hist.files <- list.files("Inputs/") %>%
  as_tibble() %>%
  filter(str_detect(value, "history"))

for (hist in hist.files$value) {
  load(paste0("Inputs/", hist))
  hist.ycfe <- hist.Ye %>%
    gather("id", "ycfe", -date) %>%
    as_tibble()
  ycfe_raw <- bind_rows(ycfe_raw, hist.ycfe)
}

## 1.3. From auctions ####

load("Loaded/auctions.Rdata")

## 1.4. From master settings and 'mops' package ####

load("Inputs/settings.Rdata")
load("Loaded/mops.Rdata")

# 2. Process data ####

## 2.1. Futures baskets ####

baskets <- baskets_raw %>%
  mutate(contract = format(basket_month_year, "%b-%y"),
         purpose = case_when(contract %in% c("Mar-20", "Jun-20") ~ "MF",
                             contract %in% c("Dec-20", "Mar-21") ~ "QE",
                             TRUE ~ as.character(NA)),
         month = month(maturity_month_year),
         year = year(maturity_month_year)) %>%
  left_join(metadata %>%
              filter(type == "AGS") %>%
              mutate(month = month(maturity),
                     year = year(maturity)),
            by = c("month", "year")) %>%
  select(-basket_month_year, -maturity_month_year, -month, -year)

## 2.2. Yield curve fitting errors ####

### 2.2.1. By issuer for a graph ####

rmse <- tibble()

for (issuer in auctions %>% distinct(issuer) %>% pull(issuer)) {
  issuer_maturity <- auctions %>%
    filter(issuer == issuer & purpose_eligible == "QE" & !(ticker %in% bonds_yt)) %>%
    pull(maturity)
  issuer_rmse <- rmse_issuer(issuer_set = issuer,
                             maturity_min = issuer_maturity %>% min(),
                             maturity_max = issuer_maturity %>% max())
  rmse <- bind_rows(rmse, issuer_rmse)
}

### 2.2.2. By bond line for IV models ####

ycfe <- ycfe_raw %>%
  filter(date %in% c(date_mf_before, date_qe_before)) %>%
  mutate(purpose = case_when(date == date_mf_before ~ "MF",
                             date == date_qe_before ~ "QE",
                             TRUE ~ as.character(NA))) %>%
  left_join(bloomberg_data %>%
              rename(coupon = cpn),
            by = "id") %>%
  left_join(metadata,
            by = c("issuer", "maturity", "coupon")) %>%
  filter(ticker != "TV3130") # also drops around a dozen EMTNs, the 8.5% NTTC Sep-25 and the 5.5% NSWTC Nov-28

# 3. Produce outputs ####

save(baskets, rmse, ycfe,
     file = "Loaded/iv.Rdata")