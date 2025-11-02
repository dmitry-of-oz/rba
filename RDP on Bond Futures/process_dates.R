# Process Dates
# Dmitry Titkov / MPI / DM
# January 2025

# 0. Preliminaries ####

library(lubridate)
library(tidyverse)

# 1. Load data ####

load("Inputs/settings_and_metadata.Rdata")
load("Inputs/aofm_events.Rdata")
load("Inputs/calendar_events.Rdata")
load("Inputs/rba_events.Rdata")
load("Inputs/rbnz_events.Rdata")

# 2. Process data ####

## 2.1. For events analysis ####

### 2.1.1. Contracts and dates ####

load("Inputs/ags_futures_3.Rdata")
ags_futures_contracts_3 <- ags_futures_contracts %>% rename(contract_3 = contract)
ags_futures_dates_3 <- ags_futures_dates %>% mutate(contract_3 = TRUE)

load("Inputs/ags_futures_10.Rdata")
ags_futures_contracts_10 <- ags_futures_contracts %>% rename(contract_10 = contract)
ags_futures_dates_10 <- ags_futures_dates %>% mutate(contract_10 = TRUE)

ags_futures_contracts <- full_join(ags_futures_contracts_3,
                                   ags_futures_contracts_10,
                                   by = c("date_contract", "date_narrowing")
)

ags_futures_dates <- full_join(ags_futures_dates_3,
                               ags_futures_dates_10,
                               by = "date"
) %>%
  select(date)

### 2.1.2. Narrow increments ####

dates_narrow <- tibble()
for (i in 1:nrow(ags_futures_contracts)) {
  dates_narrow_contract <- ags_futures_dates %>%
    filter(date >= ags_futures_contracts[i, ]$date_narrowing & date <= ags_futures_contracts[i, ]$date_contract)
  dates_narrow <- bind_rows(dates_narrow, dates_narrow_contract)
}

### 2.1.3. Events ####

dates_events <- ags_futures_dates %>%
  mutate(day = wday(date, label = TRUE, abbr = FALSE)) %>%
  left_join(abs_releases %>% distinct(date) %>% mutate(abs = TRUE), by = "date") %>%
  left_join(aofm_syndications %>% distinct(date) %>% mutate(syndication_pricing = TRUE), by = "date") %>%
  left_join(aofm_syndications %>% distinct(date_announcement) %>% rename(date = date_announcement) %>% mutate(syndication_announcement = TRUE), by = "date") %>%
  left_join(aofm_tenders %>% distinct(date) %>% mutate(tender = TRUE), by = "date") %>%
  left_join(rba_decisions %>% distinct(date) %>% mutate(decision = TRUE), by = "date") %>%
  left_join(rba_minutes %>% distinct(date) %>% mutate(minutes = TRUE), by = "date") %>%
  left_join(rba_purchases %>% distinct(date) %>% mutate(purchase = TRUE), by = "date") %>%
  left_join(rbnz_decisions %>% distinct(date) %>% mutate(rbnz = TRUE), by = "date") %>%
  left_join(dates_metadata %>% distinct(date) %>% mutate(metadata = TRUE), by = "date") %>%
  left_join(dates_narrow %>% distinct(date) %>% mutate(narrow = TRUE), by = "date")

### 2.1.4. Samples ####

dates_samples <- dates_events %>%
  mutate(type = case_when(
    day == "Tuesday" & is.na(abs) & is.na(syndication_pricing) & is.na(syndication_announcement) & is.na(tender) & is.na(decision) & is.na(minutes) & is.na(purchase) & is.na(rbnz) & is.na(metadata) & is.na(narrow) ~ "control",
    abs == TRUE & is.na(syndication_pricing) & is.na(syndication_announcement) & is.na(tender) & is.na(decision) & is.na(minutes) & is.na(purchase) & is.na(rbnz) & is.na(metadata) & is.na(narrow) ~ "abs",
    syndication_pricing == TRUE ~ "pricing",
    syndication_announcement == TRUE ~ "announcement",
    tender == TRUE & is.na(abs) & is.na(syndication_pricing) & is.na(syndication_announcement) & is.na(decision) & is.na(minutes) & is.na(purchase) & is.na(rbnz) & is.na(metadata) & is.na(narrow) ~ "tender",
    decision == TRUE & is.na(abs) & is.na(syndication_pricing) & is.na(syndication_announcement) & is.na(tender) & is.na(minutes) & is.na(purchase) & is.na(rbnz) & is.na(metadata) & is.na(narrow) ~ "decision",
    minutes == TRUE & is.na(abs) & is.na(syndication_pricing) & is.na(syndication_announcement) & is.na(tender) & is.na(decision) & is.na(purchase) & is.na(rbnz) & is.na(metadata) & is.na(narrow) ~ "minutes",
    purchase == TRUE & is.na(abs) & is.na(syndication_pricing) & is.na(syndication_announcement) & is.na(tender) & is.na(decision) & is.na(minutes) & is.na(rbnz) & is.na(metadata) & is.na(narrow) ~ "purchase",
    TRUE ~ as.character(NA)
  )) %>%
  filter(!is.na(type)) %>%
  select(date, type)

## 2.2. For rolls analysis ####

dates_rolls <- tibble()
for (i in 1:nrow(ags_futures_contracts)) {
  date_roll <- ags_futures_contracts[i, ]$date_contract
  dates_roll <- bind_rows(
    ags_futures_dates %>%
      filter(date <= date_roll & !(date %in% dates_metadata$date)) %>%
      tail(days_roll_ante) %>%
      mutate(day = row_number() - days_roll_ante),
    ags_futures_dates %>%
      filter(date > date_roll & !(date %in% dates_metadata$date)) %>%
      head(days_roll_post) %>%
      mutate(day = row_number())
  ) %>%
    select(date, day) %>%
    mutate(date_roll = date_roll)
  dates_rolls <- bind_rows(dates_rolls, dates_roll)
}

## 2.3. For syndications analysis ####

dates_syndications <- tibble()
for (i in 1:nrow(aofm_syndications)) {
  date_syndication <- aofm_syndications[i, ]$date
  dates_syndication <- bind_rows(
    ags_futures_dates %>%
      filter(date <= date_syndication & !(date %in% dates_metadata$date)) %>%
      tail(days_syndication_ante) %>%
      mutate(day = row_number() - days_syndication_ante),
    ags_futures_dates %>%
      filter(date > date_syndication & !(date %in% dates_metadata$date)) %>%
      head(days_syndication_post) %>%
      mutate(day = row_number())
  ) %>%
    select(date, day) %>%
    mutate(
      date_syndication = date_syndication,
      amount = aofm_syndications[i, ]$amount,
      tenorthree = aofm_syndications[i, ]$tenorthree
    )
  dates_syndications <- bind_rows(dates_syndications, dates_syndication)
}

# 3. Produce outputs ####

save(dates_narrow, dates_events, dates_samples, dates_rolls, dates_syndications, file = "Intermediates/dates.Rdata")
