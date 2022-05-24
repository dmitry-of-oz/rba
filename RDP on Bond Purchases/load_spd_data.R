# Load Survey of Primary Dealers Data (on Expectations for US Fed Holdings of US Treasuries)
# Dmitry Titkov / MPI / DM
# August 2021

# 0. Preliminaries ####

library(lubridate)
library(readxl)
library(tidyverse)

# 1. Load data ####

spd_raw <- read_excel("Inputs/spd.xlsx",
                      sheet = "exp")

# 2. Process data ####

spd <- spd_raw %>%
  mutate(date = as_date(date))

# 3. Produce outputs ####

save(spd,
     file = "Loaded/spd.Rdata")