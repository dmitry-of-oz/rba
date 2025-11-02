# Process Indicators
# Dmitry Titkov / MPI / DM
# February 2025

# 0. Preliminaries ####

library(hms)
library(tidyverse)

# 1. Load intraday data ####

load("Inputs/settings_and_metadata.Rdata")
load("Intermediates/dates.Rdata")

## 1.1. 3-year ####

dropped_3 <- read_csv("Intermediates/Dropped/3y_date.csv", show_col_types = FALSE) %>%
  rename(date = x) %>%
  left_join(read_csv("Intermediates/Dropped/beta_3y_exc_1130.csv", show_col_types = FALSE), by = "...1") %>%
  rename(
    beta_1130 = beta,
    df_1130 = df,
    stdError_1130 = stdError
  ) %>%
  left_join(read_csv("Intermediates/Dropped/beta_3y_exc_1430.csv", show_col_types = FALSE), by = c("...1", "time")) %>%
  rename(
    beta_1430 = beta,
    df_1430 = df,
    stdError_1430 = stdError
  ) %>%
  suppressMessages() %>%
  select(-1) %>%
  mutate(time = as_hms(time - 300))

intraday_3_raw <- read_csv("Intermediates/Full/data3y5m_1.csv", show_col_types = FALSE) %>%
  left_join(read_csv("Intermediates/Full/data3y5m_2.csv", show_col_types = FALSE), by = "...1") %>%
  left_join(read_csv("Intermediates/Full/data3y5m_3.csv", show_col_types = FALSE), by = "...1") %>%
  left_join(read_csv("Intermediates/Full/data3y5m_4.csv", show_col_types = FALSE), by = "...1") %>%
  left_join(read_csv("Intermediates/Combined/comb3_1.csv", show_col_types = FALSE) %>% rename(beta_comb = beta), by = c("...1", "date", "time")) %>%
  left_join(read_csv("Intermediates/Combined/comb3_2.csv", show_col_types = FALSE), by = c("...1")) %>%
  left_join(read_csv("Intermediates/Combined/ind_roll3.csv", show_col_types = FALSE) %>% rename(roll = x), by = c("...1")) %>%
  left_join(read_csv("Intermediates/Combined/ind_1bp3.csv", show_col_types = FALSE) %>% rename(widening = x), by = c("...1")) %>%
  suppressMessages() %>%
  select(-1) %>%
  mutate(time = as_hms(time - 300))

intraday_3 <- intraday_3_raw %>%
  left_join(dropped_3, by = c("date", "time")) %>%
  mutate(
    beta1 = case_when(
      time >= as_hms("11:20:00") & time <= as_hms("13:30:00") ~ beta_1130,
      time >= as_hms("14:20:00") & time <= as_hms("16:30:00") ~ beta_1430,
      TRUE ~ beta1
    ),
    beta_comb = case_when(
      time >= as_hms("11:20:00") & time <= as_hms("13:30:00") ~ beta_1130,
      time >= as_hms("14:20:00") & time <= as_hms("16:30:00") ~ beta_1430,
      TRUE ~ beta_comb
    )
  )

## 1.2. 10-year ####

dropped_10 <- read_csv("Intermediates/Dropped/10y_date.csv", show_col_types = FALSE) %>%
  rename(date = x) %>%
  left_join(read_csv("Intermediates/Dropped/beta_10y_exc_1130.csv", show_col_types = FALSE), by = "...1") %>%
  rename(
    beta_1130 = beta,
    df_1130 = df,
    stdError_1130 = stdError
  ) %>%
  left_join(read_csv("Intermediates/Dropped/beta_10y_exc_1430.csv", show_col_types = FALSE), by = c("...1", "time")) %>%
  rename(
    beta_1430 = beta,
    df_1430 = df,
    stdError_1430 = stdError
  ) %>%
  suppressMessages() %>%
  select(-1) %>%
  mutate(time = as_hms(time - 300))

intraday_10_raw <- read_csv("Intermediates/Full/data10y5m_1.csv", show_col_types = FALSE) %>%
  left_join(read_csv("Intermediates/Full/data10y5m_2.csv", show_col_types = FALSE), by = "...1") %>%
  left_join(read_csv("Intermediates/Full/data10y5m_3.csv", show_col_types = FALSE), by = "...1") %>%
  left_join(read_csv("Intermediates/Full/data10y5m_4.csv", show_col_types = FALSE), by = "...1") %>%
  left_join(read_csv("Intermediates/Combined/comb10_1.csv", show_col_types = FALSE) %>% rename(beta_comb = beta), by = c("...1", "date", "time")) %>%
  left_join(read_csv("Intermediates/Combined/comb10_2.csv", show_col_types = FALSE), by = c("...1")) %>%
  left_join(read_csv("Intermediates/Combined/ind_roll10.csv", show_col_types = FALSE) %>% rename(roll = x), by = c("...1")) %>%
  suppressMessages() %>%
  select(-1) %>%
  mutate(time = as_hms(time - 300))

intraday_10 <- intraday_10_raw %>%
  left_join(dropped_10, by = c("date", "time")) %>%
  mutate(
    beta1 = case_when(
      time >= as_hms("11:20:00") & time <= as_hms("13:30:00") ~ beta_1130,
      time >= as_hms("14:20:00") & time <= as_hms("16:30:00") ~ beta_1430,
      TRUE ~ beta1
    ),
    beta_comb = case_when(
      time >= as_hms("11:20:00") & time <= as_hms("13:30:00") ~ beta_1130,
      time >= as_hms("14:20:00") & time <= as_hms("16:30:00") ~ beta_1430,
      TRUE ~ beta_comb
    )
  )

# 2. Process daily data ####

## 2.1. 3-year ####

daily_3_raw <- intraday_3_raw %>%
  group_by(date) %>%
  summarise(
    beta1_daily = mean(beta1, na.rm = TRUE),
    beta_comb_daily = mean(beta_comb, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  rename_all(~ str_remove(., "_daily")) %>%
  filter(date >= date_min & date <= date_max & !(date %in% dates_metadata$date))

daily_3 <- intraday_3 %>%
  group_by(date) %>%
  summarise(
    netBuy1_daily = sum(netBuy1, na.rm = TRUE),
    netBuy2_daily = sum(netBuy2, na.rm = TRUE),
    lastPrice1_daily = mean(lastPrice1, na.rm = TRUE),
    lastPrice2_daily = mean(lastPrice2, na.rm = TRUE),
    dPrice1_daily = sum(dPrice1, na.rm = TRUE),
    dPrice2_daily = sum(dPrice2, na.rm = TRUE),
    beta_daily = mean(beta, na.rm = TRUE),
    stdError_daily = mean(stdError, na.rm = TRUE),
    tValue_daily = mean(tValue, na.rm = TRUE),
    pValue_daily = mean(pValue, na.rm = TRUE),
    df_daily = mean(df, na.rm = TRUE),
    BidAsk1_daily = mean(BidAsk1, na.rm = TRUE),
    BidAsk2_daily = mean(BidAsk2, na.rm = TRUE),
    bDepth1_daily = mean(bDepth1, na.rm = TRUE),
    bDepth2_daily = mean(bDepth2, na.rm = TRUE),
    aDepth1_daily = mean(aDepth1, na.rm = TRUE),
    aDepth2_daily = mean(aDepth2, na.rm = TRUE),
    tDepth1_daily = mean(tDepth1, na.rm = TRUE),
    tDepth2_daily = mean(tDepth2, na.rm = TRUE),
    bTurnover1_daily = sum(bTurnover1, na.rm = TRUE),
    bTurnover2_daily = sum(bTurnover2, na.rm = TRUE),
    aTurnover1_daily = sum(aTurnover1, na.rm = TRUE),
    aTurnover2_daily = sum(aTurnover2, na.rm = TRUE),
    tTurnover1_daily = sum(tTurnover1, na.rm = TRUE),
    tTurnover2_daily = sum(tTurnover2, na.rm = TRUE),
    beta1_daily = mean(beta1, na.rm = TRUE),
    beta2_daily = mean(beta2, na.rm = TRUE),
    stdError1_daily = mean(stdError1, na.rm = TRUE),
    stdError2_daily = mean(stdError2, na.rm = TRUE),
    df1_daily = mean(df1, na.rm = TRUE),
    df2_daily = mean(df2, na.rm = TRUE),
    dPrice_daily = sum(dPrice, na.rm = TRUE),
    beta_comb_daily = mean(beta_comb, na.rm = TRUE),
    BidAsk_daily = mean(BidAsk, na.rm = TRUE),
    bDepth_daily = mean(bDepth, na.rm = TRUE),
    aDepth_daily = mean(aDepth, na.rm = TRUE),
    tDepth_daily = mean(tDepth, na.rm = TRUE),
    bTurnover_daily = sum(bTurnover, na.rm = TRUE),
    aTurnover_daily = sum(aTurnover, na.rm = TRUE),
    tTurnover_daily = sum(tTurnover, na.rm = TRUE),
    roll_daily = mean(roll, na.rm = TRUE),
    widening_daily = mean(widening, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  rename_all(~ str_remove(., "_daily")) %>%
  filter(date >= date_min & date <= date_max & !(date %in% dates_metadata$date))

## 2.2. 10-year ####

daily_10_raw <- intraday_10_raw %>%
  group_by(date) %>%
  summarise(
    beta1_daily = mean(beta1, na.rm = TRUE),
    beta_comb_daily = mean(beta_comb, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  rename_all(~ str_remove(., "_daily")) %>%
  filter(date >= date_min & date <= date_max & !(date %in% dates_metadata$date))

daily_10 <- intraday_10 %>%
  group_by(date) %>%
  summarise(
    netBuy1_daily = sum(netBuy1, na.rm = TRUE),
    netBuy2_daily = sum(netBuy2, na.rm = TRUE),
    lastPrice1_daily = mean(lastPrice1, na.rm = TRUE),
    lastPrice2_daily = mean(lastPrice2, na.rm = TRUE),
    dPrice1_daily = sum(dPrice1, na.rm = TRUE),
    dPrice2_daily = sum(dPrice2, na.rm = TRUE),
    beta_daily = mean(beta, na.rm = TRUE),
    stdError_daily = mean(stdError, na.rm = TRUE),
    tValue_daily = mean(tValue, na.rm = TRUE),
    pValue_daily = mean(pValue, na.rm = TRUE),
    df_daily = mean(df, na.rm = TRUE),
    BidAsk1_daily = mean(BidAsk1, na.rm = TRUE),
    BidAsk2_daily = mean(BidAsk2, na.rm = TRUE),
    bDepth1_daily = mean(bDepth1, na.rm = TRUE),
    bDepth2_daily = mean(bDepth2, na.rm = TRUE),
    aDepth1_daily = mean(aDepth1, na.rm = TRUE),
    aDepth2_daily = mean(aDepth2, na.rm = TRUE),
    tDepth1_daily = mean(tDepth1, na.rm = TRUE),
    tDepth2_daily = mean(tDepth2, na.rm = TRUE),
    bTurnover1_daily = sum(bTurnover1, na.rm = TRUE),
    bTurnover2_daily = sum(bTurnover2, na.rm = TRUE),
    aTurnover1_daily = sum(aTurnover1, na.rm = TRUE),
    aTurnover2_daily = sum(aTurnover2, na.rm = TRUE),
    tTurnover1_daily = sum(tTurnover1, na.rm = TRUE),
    tTurnover2_daily = sum(tTurnover2, na.rm = TRUE),
    beta1_daily = mean(beta1, na.rm = TRUE),
    beta2_daily = mean(beta2, na.rm = TRUE),
    stdError1_daily = mean(stdError1, na.rm = TRUE),
    stdError2_daily = mean(stdError2, na.rm = TRUE),
    df1_daily = mean(df1, na.rm = TRUE),
    df2_daily = mean(df2, na.rm = TRUE),
    dPrice_daily = sum(dPrice, na.rm = TRUE),
    beta_comb_daily = mean(beta_comb, na.rm = TRUE),
    BidAsk_daily = mean(BidAsk, na.rm = TRUE),
    bDepth_daily = mean(bDepth, na.rm = TRUE),
    aDepth_daily = mean(aDepth, na.rm = TRUE),
    tDepth_daily = mean(tDepth, na.rm = TRUE),
    bTurnover_daily = sum(bTurnover, na.rm = TRUE),
    aTurnover_daily = sum(aTurnover, na.rm = TRUE),
    tTurnover_daily = sum(tTurnover, na.rm = TRUE),
    roll_daily = mean(roll, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  rename_all(~ str_remove(., "_daily")) %>%
  filter(date >= date_min & date <= date_max & !(date %in% dates_metadata$date))

# 3. Produce outputs ####

save(intraday_3, intraday_10,
     intraday_3_raw, intraday_10_raw,
     daily_3, daily_10,
     daily_3_raw, daily_10_raw,
     file = "Intermediates/indicators.Rdata"
)
