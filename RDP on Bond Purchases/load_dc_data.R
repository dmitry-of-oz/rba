# Load Data Capture Data (on 10-year US Treasury Yields at the Australian Close and Open)
# Dmitry Titkov / MPI / DM
# July 2021

# 0. Preliminaries ####

library(lubridate)
library(odbc)
library(tidyverse)

# 1. Load data ####

conn_dc <- dbConnect(odbc(), "FM_DataCapture")

yields_ust_intraday <- dbGetQuery(conn_dc,
                                  "select
                                  date,
                                  time,
                                  value
                                  from
                                  USER_DC_Feeds
                                  where
                                  instrumentUniqueId in (select
                                                         uniqueId
                                                         from
                                                         USER_DC_FeedsInstruments
                                                         where
                                                         ticker = 'BOND_US10YB'
                                                         and status in (0, 3))") %>%
  as_tibble() %>%
  rename(datetime = time) %>%
  mutate(date = as_date(date),
         value = as.numeric(value))

dbDisconnect(conn_dc)

# 2. Process data ####

yields_ust <- yields_ust_intraday %>%
  mutate(time = case_when(hour(datetime) == 16 & minute(datetime) == 30 ~ "close",
                          hour(datetime) == 8 & minute(datetime) == 40 ~ "open",
                          TRUE ~ as.character(NA))) %>%
  filter(!is.na(time)) %>%
  distinct() %>%
  select(-datetime) %>%
  spread(time, value)

# 3. Produce outputs ####

save(yields_ust_intraday, yields_ust, file = "Loaded/dc.Rdata")