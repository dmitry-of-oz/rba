# Back to the Futures
# Richard Finlay, Ben Jackman and Dmitry Titkov / MPI / DM
# August 2025

# 0. Preliminaries ####

source("define_functions.R")
source("define_settings_and_metadata.R")

# 1. Load data ####

source("load_aofm_events.R")
source("load_aofm_turnover.R")
source("load_calendar_events.R")
source("load_rba_events.R")
source("load_rbnz_events.R")

# 2. Process data ####

## 2.1. Separately for 3-year and 10-year tenors ####

for (ags_futures_tenor in c("3", "10")) {
  source("process_ags_futures.R")
  source("process_ticks_and_trades.R")
  source("process_data_full.R")
  source("process_data_dropped.R")
}

source("process_data_combined.R")

## 2.2. Together for both tenors ####

source("process_dates.R")
source("process_indicators.R")
source("process_introduction.R")
source("process_review.R")

# 3. Produce outputs ####

## 3.1. Models ####

source("model_events.R")
source("model_rolls.R")
source("model_syndications.R")
source("model_widening.R")

## 3.2. Graphs and tables ####

source("produce_graphs.R")
source("produce_tables.R")
