# Load RBNZ Events
# Dmitry Titkov / MPI / DM
# December 2024

# 0. Preliminaries ####

library(lubridate)
library(readxl)
library(tidyverse)

# 1. Load data ####

rbnz_decisions_raw <- read_excel("Inputs/metadata_and_events.xlsx",
                                 sheet = "rbnz_decisions"
)

# 2. Process data ####

rbnz_decisions <- rbnz_decisions_raw %>%
  mutate(date = as_date(date))

# 3. Produce outputs ####

save(rbnz_decisions, file = "Inputs/rbnz_events.Rdata")
