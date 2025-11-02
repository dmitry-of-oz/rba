# Load AOFM Events
# Dmitry Titkov / MPI / DM
# November 2024

# 0. Preliminaries ####

## 0.1. Packages ####

library(hms)
library(lubridate)
library(readxl)
library(tidyverse)

## 0.2. Definitions ####

aofm_url <- "https://www.aofm.gov.au/sites/default/files/2025-06-20/treasury%20bonds%20-%20issuance.xlsx"
aofm_file <- tempfile(fileext = ".xlsx")

# 1. Load data ####

load("Inputs/settings_and_metadata.Rdata")

download.file(aofm_url,
              aofm_file,
              quiet = TRUE,
              mode = "wb"
)

aofm_issuance_raw <- read_excel(aofm_file,
                                sheet = "Transactions",
                                skip = 1
)

aofm_syndications_raw <- read_excel("Inputs/metadata_and_events.xlsx",
                                    sheet = "aofm_syndications"
) %>%
  mutate(
    date_announcement = as_date(date_announcement),
    date_pricing = as_date(date_pricing),
    time_pricing = as_hms(time_pricing),
    maturity = as_date(maturity)
  )

# 2. Process data ####

## 2.1. Issuance ####

aofm_issuance <- aofm_issuance_raw[-1, ] %>%
  mutate_all(~ as.character(.)) %>%
  pivot_longer(
    cols = -c(`Date Held`, `Date Settled`, `Tender Number`, Maturity, Coupon, ISIN),
    names_to = "value_type",
    values_to = "value"
  ) %>%
  mutate(
    date_issue = as_date(as.numeric(`Date Held`), origin = "1899-12-30"),
    date_settle = as_date(as.numeric(`Date Settled`), origin = "1899-12-30"),
    tender = `Tender Number`,
    maturity = as_date(as.numeric(`Maturity`), origin = "1899-12-30"),
    coupon = as.numeric(Coupon),
    isin = ISIN,
    value = as.numeric(value),
    value_type = dplyr::recode(value_type,
                               "Amount Offered" = "amount_offer",
                               "Amount Allotted" = "amount_issue",
                               "Amount of Bids" = "amount_bid",
                               "Coverage Ratio" = "coverage_ratio",
                               "Weighted Average Issue Yield" = "yield_wm_issue",
                               "Lowest Accepted Yield" = "yield_min_issue",
                               "Highest Accepted Yield" = "yield_max_issue",
                               "Highest Bid Yield" = "yield_max_bid",
                               "Weighted Average Bid" = "yield_wm_bid",
                               "Secondary Market Mid Rate" = "yield_mid_market",
                               "Number of Bids" = "bids_total",
                               "Number of Successful Bids" = "bids_successful",
                               "Number of Bids Accepted in Full" = "bids_full",
                               "Settlement Proceeds" = "amount_proceeds"
    )
  ) %>%
  select(date_issue, date_settle, tender, maturity, coupon, isin, value, value_type)

## 2.2. Syndications ####

aofm_syndications <- aofm_issuance %>%
  filter(date_issue >= date_min & date_issue <= date_max & str_detect(tender, "SYN") & value_type == "amount_issue") %>%
  rename(date = date_issue) %>%
  mutate(
    amount = value / 1e6,
    tenor = as.numeric(maturity - date) / days_in_year,
    tenorthree = case_when(
      tenor >= 10 ~ 1,
      tenor < 10 & tenor > 3 ~ (tenor - 3) / 7,
      tenor <= 3 ~ 0,
      TRUE ~ as.numeric(NA)
    ),
    subtype = if_else(tenorthree < 0.5, "short", "long")
  ) %>%
  left_join(
    aofm_syndications_raw %>%
      rename(
        date = date_pricing,
        time = time_pricing
      ),
    by = c("date", "maturity")
  ) %>%
  distinct(date_announcement, date, time, subtype, amount, tenorthree)

## 2.3. Tenders ####

aofm_tenders <- aofm_issuance %>%
  filter(date_issue >= date_min & date_issue <= date_max & !str_detect(tender, "SYN") & value_type == "amount_issue") %>%
  rename(date = date_issue) %>%
  mutate(
    amount = value / 1e6,
    tenor = as.numeric(maturity - date) / days_in_year,
    tenorthree = case_when(
      tenor >= 10 ~ 1,
      tenor < 10 & tenor > 3 ~ (tenor - 3) / 7,
      tenor <= 3 ~ 0,
      TRUE ~ as.numeric(NA)
    ),
    subtype = if_else(tenorthree < 0.5, "short", "long")
  ) %>%
  distinct(date, subtype, amount, tenorthree)

# 3. Produce outputs ####

save(aofm_issuance, aofm_syndications, aofm_tenders, file = "Inputs/aofm_events.Rdata")
