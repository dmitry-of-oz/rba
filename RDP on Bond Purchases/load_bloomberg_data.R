# Load Bloomberg Data (on Historical Australian and US Data, and Recent OIS Rates and AGS/Semis Yields)
# Dmitry Titkov / MPI / DM
# July 2021

# 0. Preliminaries ####

## 0.1. Packages ####

library(tidyverse)

## 0.2. Functions ####

### 0.2.1. To process Bloomberg data ####

bbg_process <- function (bbg_data) {
  bbg_data_binded <- dplyr::tibble()
  for (bbg_code_number in c(1:length(bbg_data))) {
    bbg_code_name <- names(bbg_data)[bbg_code_number]
    bbg_data_code <- dplyr::as_tibble(bbg_data[[bbg_code_number]])
    bbg_data_code_mutated <- dplyr::mutate(bbg_data_code,
                                           bbg_code = bbg_code_name)
    bbg_data_binded <- dplyr::bind_rows(bbg_data_binded, bbg_data_code_mutated)
  }
  return(bbg_data_binded)
}

### 0.2.2. To load Bloomberg data and save processed data ####

bbg_load <- function (bbg_codes, bbg_codes_ois, date_historical = date_old, date_recent = date_new) {
  
  # Connect to Bloomberg ####
  Rblpapi::blpConnect()
  
  # Load main data ####
  bbg_data_raw <- Rblpapi::bdh(securities = bbg_codes,
                               fields = "PX_LAST",
                               start.date = date_historical)
  bbg_data <- bbg_process(bbg_data_raw)
  
  # Load recent closing and opening OIS rates ####
  bbg_data_ois_raw <- Rblpapi::bdh(securities = bbg_codes_ois,
                                   fields = c("PX_LAST", "PX_OPEN"),
                                   start.date = date_recent)
  bbg_data_ois <- bbg_process(bbg_data_ois_raw)
  
  # Load recent closing and opening AGS/semis yields ####
  ids <- dplyr::mutate(dplyr::as_tibble(Rblpapi::bsrch("FI:RDP")), # requires a Bloomberg search called 'RDP', with settings as shown in 'RDP Bloomberg search.gif'
                       id = as.character(id))
  bbg_metadata_bonds <- Rblpapi::bdp(securities = ids$id,
                                     fields = c("SECURITY_SHORT_DES", "MATURITY", "COUPON"))
  bbg_data_bonds_raw <- Rblpapi::bdh(securities = ids$id,
                                     fields = c("PX_LAST", "PX_OPEN"),
                                     start.date = date_recent)
  bbg_data_bonds <- bbg_process(bbg_data_bonds_raw)
  
  # Save processed data ####
  save(bbg_data, bbg_metadata,
       bbg_data_ois, bbg_metadata_ois,
       bbg_data_bonds, bbg_metadata_bonds,
       file = "Loaded/bloomberg.Rdata")
  
}

## 0.3. Settings ####

load("Inputs/settings.Rdata")

bbg_metadata <- tribble(~bbg_code, ~bbg_series,
                        "I00110Y Index", "aud_zcy_10y",
                        "I02510Y Index", "usd_zcy_10y",
                        "I00103Y Index", "aud_zcy_3y",
                        "ADSO10 Curncy", "aud_ois_10y",
                        "USSO10 Curncy", "usd_ois_10y",
                        "ADSO2 Curncy", "aud_ois_2y",
                        "ADSO3 Curncy", "aud_ois_3y",
                        "ADSO4 Curncy", "aud_ois_4y",
                        "ADSOC Curncy", "aud_ois_3m",
                        "USSOC Curncy", "usd_ois_3m",
                        "BBSW3M Index", "aud_bbsw_3m",
                        "US0003M Index", "usd_libor_3m",
                        "AUGDPC Index", "aud_gdp",
                        "GDP CUR$ Index", "usd_gdp",
                        "CEFBTNBL Index", "fed") # excludes T-bills

bbg_metadata_ois <- paste0("ADSO", tenors, " Curncy") %>% enframe(name = NULL, value = "bbg_code")

# 1. Load data ####

tryCatch(bbg_load(bbg_metadata$bbg_code, bbg_metadata_ois$bbg_code),
         error = function (e) {
           message(crayon::yellow("No connection to Bloomberg was detected, so fresh data were not loaded"))
         },
         finally = {
           load(file = "Loaded/bloomberg.Rdata")
           message(crayon::green(paste0("The latest yields and rates data loaded from Bloomberg were for ", str_trim(format(max(bbg_data$date), "%e %B %Y")), "")))
         })

# 2. Process data ####

bbg <- left_join(bbg_data,
                 bbg_metadata,
                 by = "bbg_code") %>%
  select(date, bbg_series, PX_LAST) %>%
  spread(bbg_series, PX_LAST)

# 3. Produce outputs ####

save(bbg, bbg_data, bbg_metadata,
     bbg_data_ois, bbg_metadata_ois,
     bbg_data_bonds, bbg_metadata_bonds,
     file = "Loaded/bloomberg.Rdata")