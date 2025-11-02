# Process Ticks and Trades
# Richard Finlay and Dmitry Titkov / MPI / DM
# January 2025

# 0. Preliminaries ####

library(lubridate)
library(hms)
library(tidyverse)
library(zoo)

# 1. Load data ####

load("Inputs/settings_and_metadata.Rdata")

# 2. Process data ####

## 2.1. Ticks ####

ticks <- ags_futures %>%
  filter(time >= time_lower & time <= time_upper) %>%
  left_join(ags_futures_contracts, by = "contract") %>%
  mutate(date_contract = if_else(is.na(date_contract), date_next, date_contract)) %>%
  group_by(date) %>%
  mutate(contract_type = if_else(date_contract == min(date_contract, na.rm = TRUE), "first", "second")) %>%
  ungroup() %>%
  group_by(date, contract_type) %>%
  mutate(
    bid_value = if_else(type == "bid", value, NA) %>% na.locf(na.rm = FALSE),
    bid_size = if_else(type == "bid", size, NA) %>% na.locf(na.rm = FALSE),
    ask_value = if_else(type == "ask", value, NA) %>% na.locf(na.rm = FALSE),
    ask_size = if_else(type == "ask", size, NA) %>% na.locf(na.rm = FALSE),
    price_last = if_else(type == "trade", value, NA) %>% na.locf(na.rm = FALSE)
  ) %>%
  ungroup() %>%
  mutate(
    price_mid = (ask_value + bid_value) / 2,
    bid_offer = ask_value - bid_value,
    best_depth = (ask_size + bid_size) / 2,
    trade_bid = if_else(type == "trade" & value == bid_value, size, NA),
    trade_ask = if_else(type == "trade" & value == ask_value, size, NA),
    trade_other = if_else(type == "trade" & value != bid_value & value != ask_value, size, NA)
  )

## 2.2. Trades ####

trades <- ags_futures %>%
  filter(type == "trade" & (code != "R" | is.na(code)))

# 3. Produce outputs ####

save(ticks, file = paste0("Intermediates/ticks_", ags_futures_tenor, ".Rdata"))
save(trades, file = paste0("Intermediates/trades_", ags_futures_tenor, ".Rdata"))
