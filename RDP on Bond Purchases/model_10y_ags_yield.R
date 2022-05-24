# Model of the 10-year AGS Yield (to Estimate the Yield Impact of the RBA's Bond Purchase Program)
# Dmitry Titkov / MPI / DM
# August 2021

# 0. Preliminaries ####

library(fixest)
library(tidyverse)

# 1. Load data ####

load("Inputs/settings.Rdata")
load("Processed/counterfactual_approaches.Rdata")

# 2. Process data ####

## 2.1. Select model data ####

md_10y <- cf_monthly %>%
  filter(date >= date_10y_begin & date <= date_10y_end) %>%
  arrange(date) %>%
  mutate_if(is.numeric, ~ . - lag(.)) %>%
  tail(-1)

md_10y_actual <- cf_monthly %>%
  filter(date >= date_10y_begin & date <= date_max) %>%
  arrange(date) %>%
  mutate_if(is.numeric, ~ . - lag(.)) %>%
  tail(-1)

md_10y_begin <- cf_monthly %>%
  filter(date == date_10y_begin)

## 2.2. Define model equations ####

me_10y_1 <- aud_zcy_10y ~
  aud_ois_10y +
  usd_zcy_10y +
  ust_ois_10y +
  rba_to_gdp +
  fedex_to_gdp +
  bbsw_ois +
  libor_ois # removed for model 2

me_10y_2 <- aud_zcy_10y ~
  aud_ois_10y +
  usd_zcy_10y +
  ust_ois_10y +
  rba_to_gdp + # removed for model 3
  fedex_to_gdp +
  bbsw_ois

me_10y_3 <- aud_zcy_10y ~
  aud_ois_10y +
  usd_zcy_10y +
  ust_ois_10y + # removed for preferred model
  fedex_to_gdp +
  bbsw_ois

me_10y <- aud_zcy_10y ~
  aud_ois_10y +
  usd_zcy_10y +
  fedex_to_gdp +
  bbsw_ois

## 2.3. Estimate models ####

m_10y_1 <- feols(me_10y_1, md_10y)
m_10y_2 <- feols(me_10y_2, md_10y)
m_10y_3 <- feols(me_10y_3, md_10y)
m_10y <- feols(me_10y, md_10y)

# 3. Produce outputs, including model data ####

## 3.1. For a graph of the actual vs counterfactual 10-year AGS yield ####

gd_10y_model <- bind_cols(md_10y_actual %>%
                            select(date, aud_zcy_10y) %>%
                            rename(actual = aud_zcy_10y),
                          enframe(predict(m_10y, md_10y_actual), name = NULL, value = "counterfactual")) %>%
  mutate(Actual = md_10y_begin$aud_zcy_10y + cumsum(actual),
         Counterfactual = md_10y_begin$aud_zcy_10y + cumsum(counterfactual),
         Residual = (Actual - Counterfactual) * 100)

## 3.2. For a table of regression results ####

td_10y_model <- tribble(~term, ~`Model 1`, ~`Model 2`, ~`Model 3`, ~`Preferred model`,
                        "Durbin–Watson statistic",
                        car::dwt(m_10y_1$residuals),
                        car::dwt(m_10y_2$residuals),
                        car::dwt(m_10y_3$residuals),
                        car::dwt(m_10y$residuals))

## 3.3. Save outputs ####

save(md_10y,
     me_10y_1, me_10y_2, me_10y_3, me_10y,
     m_10y_1, m_10y_2, m_10y_3, m_10y,
     gd_10y_model, td_10y_model,
     file = "Outputs/Model Data/10y_ags_yield.Rdata")