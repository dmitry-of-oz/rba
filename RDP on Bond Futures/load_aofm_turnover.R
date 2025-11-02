# Load AOFM Turnover
# Dmitry Titkov / MPI / DM
# March 2025

# 0. Preliminaries ####

## 0.1. Packages ####

library(lubridate)
library(readxl)
library(tidyverse)

## 0.2. Definitions ####

turnover_url <- "https://www.aofm.gov.au/sites/default/files/2025-05-02/turnover_-_treasury_bonds.xlsx"
turnover_file <- tempfile(fileext = ".xlsx")

# 1. Load data ####

download.file(turnover_url,
              turnover_file,
              quiet = TRUE,
              mode = "wb"
)

aofm_turnover_raw <- read_excel(turnover_file,
                                sheet = "By Tenor",
                                skip = 1
) %>%
  suppressMessages()

# 2. Process data ####

aofm_turnover <- aofm_turnover_raw %>%
  mutate(
    date = as_date(Month),
    to_3 = `2-5 years` / 1e3,
    to_10 = `9-12 years` / 1e3,
    to_other = (`0-2 years` + `5-9 years` + `12+ years`) / 1e3
  ) %>%
  select(date, to_3, to_10, to_other)

# 3. Produce outputs ####

save(aofm_turnover, file = "Inputs/aofm_turnover.Rdata")
