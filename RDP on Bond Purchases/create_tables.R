# Create Tables (as Word Files with RDP-like Formatting)
# Dmitry Titkov / MPI / DM
# August 2021

# 0. Preliminaries ####

library(modelsummary)
library(sandwich)
library(tidyverse)

# 1. Load data ####

load("Processed/counterfactual_approaches.Rdata")
load("Outputs/Model Data/10y_ags_yield.Rdata")
load("Outputs/Model Data/bid_offer_spreads_and_turnover.Rdata")
load("Outputs/Model Data/cross_sectional_flows.Rdata")
load("Outputs/Model Data/time_series_flows.Rdata")
load("Outputs/Model Data/yield_target_purchases.Rdata")

# 2. Process data ####

t_stars <- c("*" = 0.1, "**" = 0.05, "***" = 0.01)
t_gof <- tribble(~raw, ~clean, ~fmt,
                 "nobs", "Number of observations", 0,
                 "adj.r.squared", "Adjusted R squared", 2)

# 3. Produce outputs ####

## 3.1. Models of flow effects ####

### 3.1.1. Time-series model ####

#### 3.1.1.1. OLS/IV ####

t_ts <- modelsummary(models = list("MF AGS OLS" = m_mf_ags_ts_ols,
                                   "MF AGS IV" = m_mf_ags_ts_iv,
                                   "MF semis OLS" = m_mf_semis_spreads_ts_ols,
                                   "MF semis IV" = m_mf_semis_spreads_ts_iv,
                                   "QE AGS OLS" = m_qe_ags_ts_ols,
                                   "QE AGS IV" = m_qe_ags_ts_iv,
                                   "QE semis OLS" = m_qe_semis_spreads_ts_ols,
                                   "QE semis IV" = m_qe_semis_spreads_ts_iv),
                     title = "One-day Impact of the Flow of Bond Purchases",
                     stars = t_stars,
                     gof_map = t_gof,
                     coef_map = c("share_freefloat" = "Purchases of the eligible bond (ppts)",
                                  "share_freefloat_adjacent" = "Purchases of its adjacent bonds (ppts)",
                                  "fit_share_freefloat" = "Purchases of the eligible bond (ppts)",
                                  "fit_share_freefloat_adjacent" = "Purchases of its adjacent bonds (ppts)"),
                     fmt = 2,
                     escape = FALSE,
                     align = c("lcccccccc"),
                     vcov = vcovHAC,
                     output = "Outputs/One-day Impact of the Flow of Bond Purchases.docx")

#### 3.1.1.2. ANOVA separately ####

t_ts_separately <- modelsummary(models = list("AGS short" = m_qe_ags_ts_anova_short,
                                              "AGS long" = m_qe_ags_ts_anova_long,
                                              "Semis short" = m_qe_semis_spreads_ts_anova_short,
                                              "Semis long" = m_qe_semis_spreads_ts_anova_long,
                                              "Semis combined" = m_qe_semis_spreads_ts_anova_combined),
                                title = "One-day Impact of Eligibility within Auctions in the Bond Purchase Program",
                                stars = t_stars,
                                gof_map = t_gof,
                                coef_map = c("eligibleTRUE" = "Bond eligible for purchase"),
                                fmt = 2,
                                escape = FALSE,
                                align = c("lccccc"),
                                # vcov = "hetero", # applied in models
                                output = "Outputs/One-day Impact of Eligibility within Auctions in the Bond Purchase Program.docx")

#### 3.1.1.3. ANOVA together ####

t_ts_together <- modelsummary(models = list("MF AGS" = m_mf_ags_ts_anova_1d,
                                            "MF semis" = m_mf_semis_spreads_ts_anova_1d,
                                            "QE AGS" = m_qe_ags_ts_anova_1d,
                                            "QE semis" = m_qe_semis_spreads_ts_anova_1d),
                              title = "One-day Impact of Eligibility across each Purchase Program",
                              stars = t_stars,
                              gof_map = t_gof,
                              coef_map = c("eligibleTRUE" = "Bond eligible for purchase"),
                              fmt = 2,
                              escape = FALSE,
                              align = c("lcccc"),
                              # vcov = vcovHAC, # applied in models to use in impact graphs
                              output = "Outputs/One-day Impact of Eligibility across each Purchase Program.docx")

### 3.1.2. Cross-sectional model ####

t_cs <- modelsummary(models = list("MF AGS OLS" = m_mf_ags_cs_ols,
                                   "MG AGS IV" = m_mf_ags_cs_iv,
                                   "MF semis OLS" = m_mf_semis_spreads_cs_ols,
                                   "MF semis IV" = m_mf_semis_spreads_cs_iv,
                                   "QE AGS OLS" = m_qe_ags_cs_ols,
                                   "QE AGS IV" = m_qe_ags_cs_iv,
                                   "QE semis OLS" = m_qe_semis_spreads_cs_ols,
                                   "QE semis IV" = m_qe_semis_spreads_cs_iv),
                     title = "Cross-sectional Impact of the Flow of Bond Purchases",
                     stars = t_stars,
                     gof_map = t_gof,
                     coef_map = c("share_freefloat" = "Purchases of the eligible bond (ppts)",
                                  "share_freefloat_adjacent" = "Purchases of its adjacent bonds (ppts)",
                                  "fit_share_freefloat" = "Purchases of the eligible bond (ppts)",
                                  "fit_share_freefloat_adjacent" = "Purchases of its adjacent bonds (ppts)",
                                  "ttm" = "Residual maturity (years)",
                                  "ttm_squared" = "Residual maturity squared (years)",
                                  "coupon" = "Coupon rate (per cent)"),
                     fmt = 2,
                     escape = FALSE,
                     align = c("lcccccccc"),
                     # vcov = "hetero", # applied in models
                     output = "Outputs/Cross-sectional Impact of the Flow of Bond Purchases.docx")

## 3.2. Model of yield target purchases ####

t_yt <- modelsummary(list("AGS $b" = m_ags_1d,
                          "OIS $b" = m_ois_1d,
                          "AGS share" = m_ags_share_1d,
                          "OIS share" = m_ois_share_1d),
                     title = "On-the-day Impact of Yield Target Purchases",
                     stars = t_stars,
                     gof_map = t_gof,
                     coef_map = c("target" = "Purchases of target bond",
                                  "adjacent" = "Purchases of adjacent bond",
                                  "ois_3m_1d" = "Change in 3-month OIS rate (bps)",
                                  "ags_10y_1d" = "Change in 10-year AGS yield (bps)"),
                     fmt = 2,
                     escape = FALSE,
                     align = c("lcccc"),
                     # vcov = vcovHAC, # applied in models to use in impact graphs
                     output = "Outputs/On-the-day Impact of Yield Target Purchases.docx")

## 3.3. Model of the 10-year AGS yield ####

t_10y <- modelsummary(list("Model 1" = m_10y_1,
                           "Model 2" = m_10y_2,
                           "Model 3" = m_10y_3,
                           "Preferred model" = m_10y),
                      title = "Linear Regressions of Monthly Changes in the 10-year AGS Yield",
                      stars = t_stars,
                      gof_map = t_gof,
                      coef_map = c("aud_ois_10y" = "10-year AUD OIS rate",
                                   "usd_zcy_10y" = "10-year US Treasury yield",
                                   "ust_ois_10y" = "10-year US Treasury yield/OIS spread",
                                   "rba_to_gdp" = "RBA bond holdings to GDP",
                                   "fedex_to_gdp" = "US Fed bond holdings to GDP",
                                   "bbsw_ois" = "3-month BBSW/OIS spread",
                                   "libor_ois" = "3-month USD Libor/OIS spread"),
                      add_rows = td_10y_model,
                      fmt = 2,
                      escape = FALSE,
                      align = c("lcccc"),
                      vcov = vcovHAC,
                      output = "Outputs/Linear Regressions of Monthly Changes in the 10-year AGS Yield.docx")

## 3.4. Models of market functioning impacts ####

### 3.4.1. Bid-offer spreads ####

t_bo <- modelsummary(models = list("MF AGS" = m_mf_ags_bo,
                                   "MF semis" = m_mf_semis_bo,
                                   "QE AGS" = m_qe_ags_bo,
                                   "QE semis" = m_qe_semis_bo,
                                   "Yield target" = m_yt_bo),
                     title = "Impact on Bid-offer Spreads of Bond Purchases/Holdings",
                     stars = t_stars,
                     gof_map = t_gof,
                     coef_map = c("share_purchases_10pc" = "Purchases of free-float in the week",
                                  "share_holdings_10pc" = "Holdings of total stock at the start of the week",
                                  "elevated_sl_rateTRUE" = "Dummy for elevated stock lending fee"),
                     fmt = 2,
                     escape = FALSE,
                     align = c("lccccc"),
                     vcov = vcovHAC,
                     output = "Outputs/Impact on Bid-offer Spreads of Bond Purchases-Holdings.docx")

### 3.4.2. Turnover ####

t_to <- modelsummary(models = list("MF AGS" = m_mf_ags_to,
                                   "MF semis" = m_mf_semis_to,
                                   "QE AGS" = m_qe_ags_to,
                                   "QE semis" = m_qe_semis_to,
                                   "Yield target" = m_yt_to),
                     title = "Impact on Turnover of Bond Purchases/Holdings",
                     stars = t_stars,
                     gof_map = t_gof,
                     coef_map = c("share_purchases_10pc" = "Purchases of free-float in the week",
                                  "share_holdings_10pc" = "Holdings of total stock at the start of the week",
                                  "elevated_sl_rateTRUE" = "Dummy for elevated stock lending fee"),
                     fmt = 2,
                     escape = FALSE,
                     align = c("lccccc"),
                     vcov = vcovHAC,
                     output = "Outputs/Impact on Turnover of Bond Purchases-Holdings.docx")