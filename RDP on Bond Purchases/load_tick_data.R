# Load Tick Data (on Intraday Semis Yields for the Days of the RBA's Market Functioning Auctions)
# Richard Finlay and Dmitry Titkov / MPI / DM
# July 2021

# 0. Preliminaries ####

library(lubridate)
library(readxl)
library(tidyverse)

# 1. Load data ####

## 1.1. From Bloomberg ####

tick <- read_excel("Inputs/Raw/semis_intraday_MF_values.xlsx") %>%
  mutate(date = as_date(date),
         datetime = as_datetime(datetime),
         month = month(maturity),
         year = year(maturity))

## 1.2. From 'mops' package ####

load("Loaded/mops.Rdata")

# 2. Process data ####

yields_semis_intraday_purchases_mf <- left_join(tick %>%
                                                  select(-ticker, -maturity, -issued, -cusip),
                                                metadata %>%
                                                  mutate(month = month(maturity),
                                                         year = year(maturity)),
                                                by = c(metacols[-c(4:6, 8)], "month", "year")) %>%
  select(-month, -year) %>%
  filter(!is.na(ticker)) # drops the 6% QTC Jun-21 and the 6% NSWTC May-23

yields_semis_intraday_purchases <- bind_rows(yields_semis_intraday_purchases_mf,
                                             yields_semis_intraday_purchases_qe)

# 3. Produce outputs ####

save(tick,
     yields_semis_intraday_purchases_mf,
     yields_semis_intraday_purchases,
     file = "Loaded/tick.Rdata")