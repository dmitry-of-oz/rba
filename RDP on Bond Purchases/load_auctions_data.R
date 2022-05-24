# Load Auctions Data (on AGS and Semis Eligible to be Purchased in the RBA's Auctions)
# Michelle Xiang and Dmitry Titkov / MPI / DM
# July 2021

# 0. Preliminaries ####

## 0.1. Packages ####

library(lubridate)
library(readxl)
library(tidyverse)

## 0.2. Functions ####

source("define_functions.R")

# 1. Load data ####

## 1.1. From manual inputs ####

auctions_qe_semis_raw <- read_excel("Inputs/auctions.xlsx",
                                    sheet = "qe_semis") %>%
  mutate(date = as_date(date),
         maturity_min = as_date(maturity_min),
         maturity_max = as_date(maturity_max),
         excluded_month_year = as_date(excluded_month_year))

auctions_mf_ags_raw <- read_excel("Inputs/auctions.xlsx",
                                  sheet = "mf_ags") %>%
  mutate(date = as_date(date),
         maturity_month_year = as_date(maturity_month_year))

auctions_mf_semis_raw <- read_excel("Inputs/auctions.xlsx",
                                    sheet = "mf_semis") %>%
  mutate(date = as_date(date))

## 1.2. From Findur ####

load("Loaded/findur.Rdata")

## 1.3. From AOFM ####

load("Loaded/aofm.Rdata")

auctions_ags_issuance_raw <- ags_issuance %>%
  distinct(date_issue, tender, maturity, coupon, isin)

## 1.4. From master settings and 'mops' package ####

load("Inputs/settings.Rdata")
load("Loaded/mops.Rdata")

auctions_metadata <- metadata %>%
  filter(subtype == "Nominals" & ticker != "TC2XXX" & coupon != 0 & nchar(cusip) > 1 & gg == FALSE)

# 2. Process data ####

## 2.1. Market functioning auctions ####

### 2.1.1. AGS ####

auctions_mf_ags <- auctions_mf_ags_raw %>%
  mutate(month = month(maturity_month_year),
         year = year(maturity_month_year)) %>%
  left_join(auctions_metadata %>%
              filter(type == "AGS") %>%
              mutate(month = month(maturity),
                     year = year(maturity)),
            by = c("month", "year")) %>%
  filter(issued < date) %>%
  select(-maturity_month_year, -month, -year)

### 2.1.2. Semis ####

auctions_mf_semis <- auctions_mf_semis_raw %>%
  left_join(auctions_metadata %>%
              filter(type == "Semis") %>%
              mutate(maturity_year = year(maturity)),
            by = "maturity_year") %>%
  filter(issued < date) %>%
  select(-maturity_year)

## 2.2. QE auctions ####

### 2.2.1. AGS ####

#### 2.2.1.1. Apply minimum and maximum maturities for QE-eligible AGS ####

auctions_qe_ags_raw <- purchases %>%
  filter(purpose == "QE" & type == "AGS") %>%
  distinct(date) %>%
  mutate(maturity_min = case_when(date < as_date("2021-11-08") & wday(date, label = TRUE) == "Mon" ~ as_date("2024-11-01"),
                                  date >= as_date("2021-11-08") & wday(date, label = TRUE) == "Mon" ~ as_date("2024-04-01"),
                                  date < as_date("2021-09-13") & wday(date, label = TRUE) == "Thu" ~ as_date("2028-11-01"),
                                  date >= as_date("2021-09-13") & wday(date, label = TRUE) == "Thu" ~ as_date("2029-04-01"),
                                  TRUE ~ as_date(NA)),
         maturity_max = case_when(date < as_date("2021-09-13") & wday(date, label = TRUE) == "Mon" ~ as_date("2028-05-31"),
                                  date >= as_date("2021-09-13") & wday(date, label = TRUE) == "Mon" ~ as_date("2028-11-30"),
                                  date < as_date("2021-09-13") & wday(date, label = TRUE) == "Thu" ~ as_date("2032-05-31"),
                                  date >= as_date("2021-09-13") &
                                    date < as_date("2021-12-16") & wday(date, label = TRUE) == "Thu" ~ as_date("2032-11-30"),
                                  date >= as_date("2021-12-16") & wday(date, label = TRUE) == "Thu" ~ as_date("2033-04-30"),
                                  TRUE ~ as_date(NA)))

#### 2.2.1.2. Determine which AGS are in the maturity range for each QE auction ####

auctions_qe_ags_unexcluded <- crossing(auctions_qe_ags_raw,
                                       auctions_metadata %>%
                                         filter(type == "AGS")) %>%
  filter(issued < date & maturity >= maturity_min & maturity <= maturity_max)

#### 2.2.1.3. Identify new issuance of AGS and taps of existing AGS ####

auctions_ags_issuance_new <- auctions_ags_issuance_raw %>%
  group_by(maturity, coupon, isin) %>%
  summarise(date_issue_min = min(date_issue),
            .groups = "drop") %>%
  ungroup() %>%
  mutate(type_issue = "new")

auctions_ags_issuance <- left_join(auctions_ags_issuance_raw,
                                   auctions_ags_issuance_new %>% rename(date_issue = date_issue_min),
                                   by = c("date_issue", "maturity", "coupon", "isin")) %>%
  mutate(type_issue = replace_na(type_issue, "tap"))

#### 2.2.1.4. Exclude AGS that are tapped (for the week of the tap) or newly issued (for a month from issuance) ####

auctions_qe_ags_excluded <- auctions_qe_ags_unexcluded %>%
  left_join(auctions_ags_issuance %>%
              rename(cusip = isin),
            by = c("maturity", "coupon", "cusip")) %>%
  mutate(week = ceiling_date(date, "week"),
         week_issue = ceiling_date(date_issue, "week"),
         exclude = case_when(type_issue == "tap" & week == week_issue ~ TRUE,
                             type_issue == "new" & date < make_date(year = year(date_issue),
                                                                    month = month(date_issue) + 1,
                                                                    day = day(date_issue)) ~ TRUE,
                             TRUE ~ FALSE)) %>%
  filter(exclude == TRUE)

auctions_qe_ags <- left_join(auctions_qe_ags_unexcluded,
                             auctions_qe_ags_excluded %>%
                               select(date, ticker, exclude),
                             by = c("date", "ticker")) %>%
  filter(is.na(exclude)) %>%
  select(-exclude, -maturity_min, -maturity_max)

### 2.2.2. Semis ####

#### 2.2.2.1. Determine which semis are in the maturity range for each QE auction, and apply ad hoc exclusions ####

auctions_qe_semis_unexcluded <- crossing(auctions_qe_semis_raw %>%
                                           distinct(date, maturity_min, maturity_max),
                                         auctions_metadata %>%
                                           filter(type == "Semis")) %>%
  filter(issued < date & maturity >= maturity_min & maturity <= maturity_max &
           !(ticker == "NT2779" & date <= as_date("2021-06-30")) & # NTTC May-32 (excluded by mistake)
           !(ticker == "TV3178" & date >= as_date("2021-06-09"))) # 4.75% TCV Nov-30 (excluded due to outlying yield)

#### 2.2.2.2. Exclude semis that are tapped or newly issued (according to manual inputs), and manually adjust for any unintended exclusions ####

auctions_qe_semis_excluded <- auctions_qe_semis_raw %>%
  mutate(month = month(excluded_month_year),
         year = year(excluded_month_year)) %>%
  rename(issuer = excluded_issuer) %>%
  left_join(auctions_metadata %>%
              filter(type == "Semis") %>%
              mutate(month = month(maturity),
                     year = year(maturity)),
            by = c("issuer", "month", "year")) %>%
  filter(!(ticker == "TV3178" & date == as_date("2021-07-07")) # TCV has two Nov-30 bonds, one of which is excluded ad hoc
         & !(ticker == "TV3178" & date == as_date("2021-11-17"))
         & !(ticker == "TV3178" & date == as_date("2021-11-24")))

auctions_qe_semis <- left_join(auctions_qe_semis_unexcluded,
                               auctions_qe_semis_excluded %>%
                                 filter(!is.na(type)) %>%
                                 mutate(exclude = TRUE) %>%
                                 select(date, ticker, exclude),
                               by = c("date", "ticker")) %>%
  filter(is.na(exclude)) %>%
  select(-exclude, -maturity_min, -maturity_max)

### 2.2.3. Combined data ####

auctions_qe <- crossing(days(date_qe_begin, date_qe_end),
                        bind_rows(auctions_qe_ags_unexcluded,
                                  auctions_qe_semis_unexcluded) %>%
                          distinct(ticker)) %>%
  left_join(bind_rows(auctions_qe_ags_unexcluded,
                      auctions_qe_semis_unexcluded) %>%
              group_by(ticker) %>%
              summarise(date_minimum = min(date),
                        .groups = "drop") %>%
              ungroup() %>%
              mutate(date_minimum = case_when(date_minimum %in% c(as_date("2020-11-05"), as_date("2020-11-09"), as_date("2020-11-11"), as_date("2020-11-18")) ~ date_qe_begin,
                                              TRUE ~ date_minimum),
                     date_maximum = case_when(ticker == "TV3178" ~ as_date("2021-06-02"),
                                              TRUE ~ date_qe_end)),
            by = "ticker") %>%
  filter(date >= date_minimum & date <= date_maximum) %>%
  select(-date_minimum, -date_maximum)

## 2.3. Market functioning and QE auctions, applying ad hoc inclusions ####

auctions <- bind_rows(auctions_mf_ags %>% mutate(purpose_eligible = "MF"),
                      auctions_mf_semis %>% mutate(purpose_eligible = "MF"),
                      auctions_qe_ags %>% mutate(purpose_eligible = "QE"),
                      auctions_qe_semis %>% mutate(purpose_eligible = "QE"),
                      auctions_metadata %>%
                        filter(ticker == "WT6072") %>%
                        mutate(date = as_date("2021-01-13"),
                               purpose_eligible = "QE")) # included in order to have at least one eligible WATC bond

## 2.4. Data quality tests ####

### 2.4.1. Have the manual inputs for semis QE auctions been updated? ####

auctions_test1 <- purchases %>%
  filter(purpose == "QE" & type == "Semis") %>%
  distinct(date) %>%
  full_join(auctions_qe_semis_raw %>%
              distinct(date) %>%
              mutate(test = TRUE),
            by = "date")

if (sum(is.na(auctions_test1$date), is.na(auctions_test1$test)) != 0) {
  message(crayon::yellow("The auction dates in the qe_semis tab in auctions.xlsx do not line up with dates in Findur\nIs the spreadsheet up to date?"))
} else {
  message(crayon::green("The manual inputs for semis excluded from QE auctions appear up to date"))
}

### 2.4.2. Are there any potentially unintended exclusions for semis QE auctions? ####

auctions_test2a <- auctions_qe_semis_raw %>%
  filter(!is.na(excluded_issuer))

auctions_test2b <- auctions_qe_semis_excluded %>%
  filter(!is.na(ticker))

if (nrow(auctions_test2a) - nrow(auctions_test2b) != 0) {
  message(crayon::yellow("The excluded bonds in the qe_semis tab in auctions.xlsx do not match one-for-one with bonds in Findur\nIs the spreadsheet correct, and if so then is a manual adjustment necessary?"))
} else {
  message(crayon::green("The manual inputs for semis excluded from QE auctions appear to match the metadata"))
}

### 2.4.3. Have all purchases been of eligible bonds, for both the market functioning and QE auctions? ####

auctions_test3 <- full_join(auctions,
                            purchases %>% filter(purpose %in% c("MF", "QE")),
                            by = c("date", metacols))

if (sum(is.na(auctions_test3$purpose_eligible)) != 0) {
  message(crayon::yellow("The purchases in Findur are not all identified as being eligible\nAre there any ad hoc inclusions that need to be applied?"))
} else {
  message(crayon::green("All AGS and semis purchases appear to have been of eligible bonds"))
}

# 3. Produce outputs ####

save(auctions, auctions_metadata,
     auctions_qe_ags_unexcluded,
     auctions_qe_semis_unexcluded,
     auctions_mf_ags, auctions_mf_semis,
     auctions_qe_ags, auctions_qe_semis, auctions_qe,
     file = "Loaded/auctions.Rdata")