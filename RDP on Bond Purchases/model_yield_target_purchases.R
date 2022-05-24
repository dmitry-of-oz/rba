# Model of Yield Target Purchases (to Estimate the Contribution of Bond Purchases to the Implementation of the RBA's Yield Target)
# Dmitry Titkov / MPI / DM
# August 2021

# 0. Preliminaries ####

library(fixest)
library(sandwich)
library(tidyverse)

# 1. Load data ####

load("Inputs/settings.Rdata")
load("Processed/yield_target_analysis.Rdata")

# 2. Process data ####

## 2.1. Select model data ####

### 2.1.1. Purchases in $b ####

md_ags <- yt_ags %>% filter(date >= date_yt_begin & date <= date_yt_end) %>% rename(target = purchases_that, adjacent = purchases_other)
md_ois <- yt_ois %>% filter(date >= date_yt_begin & date <= date_yt_end) %>% rename(target = purchases_that, adjacent = purchases_other)

### 2.1.2. Purchases as a share of the free-float of the target bond ####

md_ags_share <- yt_ags %>% filter(date >= date_yt_begin & date <= date_yt_end) %>% rename(target = share_freefloat_that, adjacent = share_freefloat_other)
md_ois_share <- yt_ois %>% filter(date >= date_yt_begin & date <= date_yt_end) %>% rename(target = share_freefloat_that, adjacent = share_freefloat_other)

## 2.2. Define model equations ####

### 2.2.1. 3-year AGS yield ####

me_ags_1d <- ags_3y_1d ~
  target +
  adjacent +
  ois_3m_1d +
  ags_10y_1d

me_ags_2d <- ags_3y_2d ~
  target +
  adjacent +
  purchased_yt_p1 +
  ois_3m_2d +
  ags_10y_2d

me_ags_3d <- ags_3y_3d ~
  target +
  adjacent +
  purchased_yt_p2 +
  ois_3m_3d +
  ags_10y_3d

me_ags_4d <- ags_3y_4d ~
  target +
  adjacent +
  purchased_yt_p3 +
  ois_3m_4d +
  ags_10y_4d

### 2.2.2. 3-year OIS rate ####

me_ois_1d <- ois_3y_1d ~
  target +
  adjacent +
  ois_3m_1d +
  ags_10y_1d

me_ois_2d <- ois_3y_2d ~
  target +
  adjacent +
  purchased_yt_p1 +
  ois_3m_2d +
  ags_10y_2d

me_ois_3d <- ois_3y_3d ~
  target +
  adjacent +
  purchased_yt_p2 +
  ois_3m_3d +
  ags_10y_3d

me_ois_4d <- ois_3y_4d ~
  target +
  adjacent +
  purchased_yt_p3 +
  ois_3m_4d +
  ags_10y_4d

## 2.3. Estimate models ####

m_ags_1d <- feols(me_ags_1d, md_ags, vcov = vcovHAC)
m_ags_2d <- feols(me_ags_2d, md_ags, vcov = vcovHAC)
m_ags_3d <- feols(me_ags_3d, md_ags, vcov = vcovHAC)
m_ags_4d <- feols(me_ags_4d, md_ags, vcov = vcovHAC)

m_ois_1d <- feols(me_ois_1d, md_ois, vcov = vcovHAC)
m_ois_2d <- feols(me_ois_2d, md_ois, vcov = vcovHAC)
m_ois_3d <- feols(me_ois_3d, md_ois, vcov = vcovHAC)
m_ois_4d <- feols(me_ois_4d, md_ois, vcov = vcovHAC)

m_ags_share_1d <- feols(me_ags_1d, md_ags_share, vcov = vcovHAC)
m_ags_share_2d <- feols(me_ags_2d, md_ags_share, vcov = vcovHAC)
m_ags_share_3d <- feols(me_ags_3d, md_ags_share, vcov = vcovHAC)
m_ags_share_4d <- feols(me_ags_4d, md_ags_share, vcov = vcovHAC)

m_ois_share_1d <- feols(me_ois_1d, md_ois_share, vcov = vcovHAC)
m_ois_share_2d <- feols(me_ois_2d, md_ois_share, vcov = vcovHAC)
m_ois_share_3d <- feols(me_ois_3d, md_ois_share, vcov = vcovHAC)
m_ois_share_4d <- feols(me_ois_4d, md_ois_share, vcov = vcovHAC)

# 3. Produce outputs ####

save(md_ags, md_ois,
     md_ags_share, md_ois_share,
     me_ags_1d, me_ags_2d, me_ags_3d, me_ags_4d,
     me_ois_1d, me_ois_2d, me_ois_3d, me_ois_4d,
     m_ags_1d, m_ags_2d, m_ags_3d, m_ags_4d,
     m_ois_1d, m_ois_2d, m_ois_3d, m_ois_4d,
     m_ags_share_1d, m_ags_share_2d, m_ags_share_3d, m_ags_share_4d,
     m_ois_share_1d, m_ois_share_2d, m_ois_share_3d, m_ois_share_4d,
     file = "Outputs/Model Data/yield_target_purchases.Rdata")