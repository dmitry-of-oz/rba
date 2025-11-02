# Define Settings and Metadata
# Dmitry Titkov / MPI / DM
# November 2024

# 0. Preliminaries ####

library(hms)
library(lubridate)
library(readxl)
library(tidyverse)

# 1. Define settings ####

set.seed(20191009)

## 1.1. Dates ####

date_min <- as_date("2019-10-09")
date_max <- as_date("2025-06-23")
date_next <- as_date("2025-09-15")

date_narrow <- as_date("2020-09-15")
date_widening <- as_date("2022-10-18")
date_delinked <- as_date("2025-03-17")

date_widening_min <- as_date("2022-09-16")
date_widening_max <- as_date("2022-12-08")

days_narrowing <- 5 # https://www.asx.com.au/blog/managing-futures-exposure-the-quarterly-futures-roll-explained
days_in_year <- 365.24

days_syndication_ante <- 8
days_syndication_post <- 4

days_roll_ante <- 10
days_roll_post <- 2

## 1.2. Times ####

time_lower <- as_hms("08:30:00")
time_min <- as_hms("10:00:00")
time_max <- as_hms("16:25:00")
time_upper <- as_hms("16:30:00")
time_offset <- as_hms("03:30:00")

time_decision <- as_hms("14:30:00")
time_release <- as_hms("11:30:00")
time_purchase <- as_hms("15:30:00")
time_tender <- as_hms("11:00:00")
time_announcement <- as_hms("12:00:00")

time_relative_min <- as_hms(-time_offset - as_hms("00:15:00"))
time_relative_max <- as_hms(-time_offset + as_hms("04:45:00"))

# 2. Define metadata ####

dates_metadata <- read_excel("Inputs/metadata_and_events.xlsx",
                             sheet = "dates_metadata"
) %>%
  mutate(date = as_date(date))

# 3. Produce outputs ####

save(date_min, date_max, date_next,
     date_narrow, date_widening, date_delinked,
     date_widening_min, date_widening_max,
     days_narrowing, days_in_year,
     days_syndication_ante, days_syndication_post,
     days_roll_ante, days_roll_post,
     time_lower, time_min, time_max, time_upper, time_offset,
     time_decision, time_release, time_purchase, time_tender, time_announcement,
     time_relative_min, time_relative_max,
     dates_metadata,
     file = "Inputs/settings_and_metadata.Rdata"
)
