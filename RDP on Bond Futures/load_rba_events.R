# Load RBA Events
# Dmitry Titkov / MPI / DM
# November 2024

# 0. Preliminaries ####

library(lubridate)
library(rbatools)
library(readrba)
library(tidyverse)

# 1. Load data ####

load("Inputs/settings_and_metadata.Rdata")

rba_purchases_raw <- read_rba("a3")
rba_decisions_raw <- board_decisions()

# 2. Process data ####

## 2.1. Board decisions ####

rba_decisions <- rba_decisions_raw %>%
  mutate(date = effective_date - 1) %>%
  filter(date >= date_min & date <= date_max) %>%
  distinct(date, change, target)

## 2.2. Bond purchases ####

rba_purchases <- full_join(
  rba_purchases_raw %>%
    filter(table_title %in% c("A3 Monetary Policy Operations - Bond Purchase Program", "A3 Monetary Policy Operations - Long-Dated Open Market Operations") &
             series == "Maturity" &
             date >= date_min &
             date <= date_max) %>%
    mutate(
      subtype = if_else(str_detect(table_title, "Bond Purchase Program"), "qe", "yt_mf"),
      subtype = case_when(
        subtype == "qe" & wday(date, label = TRUE, abbr = FALSE) == "Monday" ~ "qe_ags_short",
        subtype == "qe" & wday(date, label = TRUE, abbr = FALSE) == "Wednesday" ~ "qe_semis",
        subtype == "qe" & wday(date, label = TRUE, abbr = FALSE) == "Thursday" ~ "qe_ags_long",
        TRUE ~ subtype
      ),
      row = row_number()
    ) %>%
    select(row, date, subtype, maturity = value),
  rba_purchases_raw %>%
    filter(table_title %in% c("A3 Monetary Policy Operations - Bond Purchase Program", "A3 Monetary Policy Operations - Long-Dated Open Market Operations") &
             series == "Face Value" &
             date >= date_min &
             date <= date_max) %>%
    mutate(
      subtype = if_else(str_detect(table_title, "Bond Purchase Program"), "qe", "yt_mf"),
      subtype = case_when(
        subtype == "qe" & wday(date, label = TRUE, abbr = FALSE) == "Monday" ~ "qe_ags_short",
        subtype == "qe" & wday(date, label = TRUE, abbr = FALSE) == "Wednesday" ~ "qe_semis",
        subtype == "qe" & wday(date, label = TRUE, abbr = FALSE) == "Thursday" ~ "qe_ags_long",
        TRUE ~ subtype
      ),
      row = row_number()
    ) %>%
    select(row, date, subtype, amount = value),
  by = c("row", "date", "subtype")
) %>%
  select(-row) %>%
  mutate(
    maturity = as_date(maturity, origin = as_date("1899-12-30")),
    tenor = as.numeric(maturity - date) / days_in_year,
    tenorthree = case_when(
      tenor >= 10 ~ 1,
      tenor < 10 & tenor > 3 ~ (tenor - 3) / 7,
      tenor <= 3 ~ 0,
      TRUE ~ as.numeric(NA)
    )
  ) %>%
  group_by(date, subtype) %>%
  summarise(
    amount_sum = sum(amount),
    tenorthree_wm = weighted.mean(tenorthree, amount),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  rename(
    amount = amount_sum,
    tenorthree = tenorthree_wm
  ) %>%
  distinct(date, subtype, amount, tenorthree)

# 3. Produce outputs ####

save(rba_decisions, rba_purchases, file = "Inputs/rba_events.Rdata")
