# Load calendar Events
# Dmitry Titkov / MPI / DM
# January 2025

# 0. Preliminaries ####

## 0.1. Packages ####

library(hms)
library(lubridate)
library(readxl)
library(tidyverse)

## 0.2. Definitions ####

rba_minutes_missing <- tribble(
  ~date, ~time,
  as_date("2022-02-15"), as_hms("11:30:00"),
  as_date("2022-03-15"), as_hms("11:30:00")
)

# 1. Load data ####

calendar_events_old <- read_excel("Inputs/metadata_and_events.xlsx",
                                   sheet = "calendar_events_old"
) %>%
  suppressMessages() %>%
  mutate(
    month = str_sub(`Date Time`, 1, 2) %>% as.numeric(),
    day = str_sub(`Date Time`, 4, 5) %>% as.numeric(),
    year = str_sub(`Date Time`, 7, 10) %>% as.numeric(),
    date = make_date(year = year, month = month, day = day),
    Relevance = as.numeric(Relevance)
  ) %>%
  suppressWarnings() %>%
  filter(!is.na(date) & nchar(`Date Time`) == 16) %>%
  mutate(time = paste0(str_sub(`Date Time`, 12, 16), ":00") %>% as_hms()) %>%
  rename_all(~ str_to_lower(.))

calendar_events_new <- read_excel("Inputs/metadata_and_events.xlsx",
                                   sheet = "calendar_events_new"
) %>%
  suppressMessages() %>%
  mutate(
    datetime = round_date(as_datetime(as_date(as.numeric(`Date Time`), origin = as_date("1899-12-30"))), "minute"),
    Relevance = as.numeric(Relevance)
  ) %>%
  suppressWarnings() %>%
  filter(!is.na(datetime)) %>%
  mutate(
    date = as_date(datetime),
    time = as_hms(datetime)
  ) %>%
  rename_all(~ str_to_lower(.))

calendar_events <- bind_rows(
  calendar_events_old,
  calendar_events_new
)

# 2. Process data ####

## 2.1. ABS releases ####

abs_releases <- calendar_events %>%
  mutate(release = case_when(
    event == "CPI QoQ" ~ "cpi",
    event == "Unemployment Rate" ~ "lfs",
    event == "GDP SA QoQ" ~ "natties",
    event == "Retail Sales MoM" ~ "retail",
    event == "Wage Price Index QoQ" ~ "wpi",
    TRUE ~ as.character(NA)
  )) %>%
  filter(!is.na(release) & !str_detect(...6, " F")) %>%
  select(date, time, release) %>%
  arrange(date, time)

## 2.2. RBA minutes ####

rba_minutes <- calendar_events %>%
  filter(str_detect(event, "RBA") & str_detect(event, "Minutes")) %>%
  bind_rows(rba_minutes_missing) %>%
  select(date, time) %>%
  arrange(date, time)

# 3. Produce outputs ####

save(calendar_events, abs_releases, rba_minutes, file = "Inputs/calendar_events.Rdata")
