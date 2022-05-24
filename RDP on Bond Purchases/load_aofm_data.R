# Load AOFM Data (on AGS Outstanding)
# Dmitry Titkov / MPI / DM
# July 2021

# 0. Preliminaries ####

## 0.1. Packages ####

library(lubridate)
library(readaofm)
library(tidyverse)

## 0.2. Functions ####

source("define_functions.R")

### 0.2.1. For AOFM transactions data ####

aofm_transactions <- function (aofm_data) {
  colnames(aofm_data)[1] <- "date"
  aofm_data_filtered <- dplyr::filter(aofm_data,
                                      value_type %in% c("amount_issue", "amount_repurchase"))
  aofm_data_mutated <- dplyr::mutate(aofm_data_filtered,
                                     value = dplyr::case_when(value_type == "amount_issue" ~ value / 1e6,
                                                              value_type == "amount_repurchase" ~ value / -1e6,
                                                              TRUE ~ as.numeric(NA)))
  aofm_data_selected <- dplyr::select(aofm_data_mutated,
                                      date, maturity, coupon, value)
  return(aofm_data_selected)
}

### 0.2.2. For AOFM positions data to be calculated from transactions data ####

aofm_positions <- function (aofm_data) {
  aofm_data_united <- tidyr::unite(aofm_data,
                                   "maturity_coupon", maturity, coupon)
  aofm_data_grouped <- dplyr::group_by(aofm_data_united,
                                       date, maturity_coupon)
  aofm_data_summarised <- dplyr::summarise(aofm_data_grouped,
                                           value_sum = sum(value),
                                           .groups = "drop")
  aofm_data_ungrouped <- dplyr::ungroup(aofm_data_summarised)
  aofm_data_spreaded <- tidyr::spread(aofm_data_ungrouped,
                                      maturity_coupon, value_sum)
  aofm_data_joined <- dplyr::right_join(aofm_data_spreaded,
                                        days(min(aofm_data$date), date_max),
                                        by = "date")
  aofm_data_arranged <- dplyr::arrange(aofm_data_joined,
                                       date)
  aofm_data_mutated <- dplyr::mutate_if(aofm_data_arranged,
                                        is.numeric, ~ cumsum(tidyr::replace_na(., 0)))
  aofm_data_gathered <- tidyr::gather(aofm_data_mutated,
                                      "maturity_coupon", "value", -date)
  aofm_data_separated <- tidyr::separate(aofm_data_gathered,
                                         maturity_coupon, c("maturity", "coupon"), sep = "_")
  aofm_data_remutated <- dplyr::mutate(aofm_data_separated,
                                       maturity = lubridate::as_date(maturity),
                                       value = dplyr::case_when(date > maturity ~ 0,
                                                                date <= maturity ~ value,
                                                                TRUE ~ as.numeric(NA)))
  return(aofm_data_remutated)
}

# 1. Load data ####

## 1.1. From AOFM ####

ags_positions <- aofm_data(type = "positions", instrument = "tb", positions_basis = "dealt")
ags_issuance <- aofm_data(type = "issuance", instrument = "tb")
ags_buybacks <- aofm_data(type = "buybacks", instrument = "tb")

message(crayon::green(paste0("The latest AGS issuance data loaded using the 'readaofm' package were for ", str_trim(format(max(ags_issuance$date_issue), "%e %B %Y")))))
message(crayon::yellow("AGS excluded from QE auctions after the above date may be mistakenly analysed as eligible"))

## 1.2. From master settings and 'mops' package ####

load("Inputs/settings.Rdata")
load("Loaded/mops.Rdata")

# 2. Process data ####

ags <- bind_rows(ags_positions %>%
                   filter(date == date_min & value_type == "fv" & !is.na(value)) %>%
                   mutate(value = value / -1e6) %>%
                   select(-value_type),
                 ags_issuance %>%
                   aofm_transactions() %>%
                   filter(date > date_min),
                 ags_buybacks %>%
                   aofm_transactions() %>%
                   filter(date > date_min)) %>%
  aofm_positions() %>%
  mutate(issuer = "Cth",
         coupon = as.numeric(coupon)) %>%
  left_join(metadata,
            by = c("issuer", "maturity", "coupon"))

# 3. Produce outputs ####

save(ags_positions, ags_issuance, ags_buybacks, ags, file = "Loaded/aofm.Rdata")