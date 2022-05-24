# Create Graphs (as EMF+ Files with RDP Formatting)
# Dmitry Titkov / MPI / DM
# July 2021

# 0. Preliminaries ####

## 0.1. Packages ####

library(arphit)
library(lubridate)
library(tidyverse)

## 0.2. Function to graph error bars ####

model_name_term <- function (model_set, name_set, term_set) {
  model_tidied <- broom::tidy(model_set)
  model_filtered <- dplyr::filter(model_tidied,
                                  term == term_set)
  model_mutated <- dplyr::mutate(model_filtered,
                                 name = name_set,
                                 lower = estimate - 1.96 * std.error,
                                 upper = estimate + 1.96 * std.error)
  model_selected <- dplyr::select(model_mutated,
                                  name, term, lower, estimate, upper)
  return(model_selected)
}

## 0.3. RBA theme for 'ggplot2' package ####

source("define_colours.R") # with thanks to Calvin He and Angus Moore (including for the RBA's 'arphit' package more generally)
source("define_theme.R")

# 1. Load data ####

load("Inputs/settings.Rdata")
load("Loaded/findur.Rdata")
load("Processed/counterfactual_approaches.Rdata")
load("Processed/event_studies.Rdata")
load("Processed/market_functioning_impacts.Rdata")
load("Processed/yield_target_analysis.Rdata")
load("Outputs/Model Data/10y_ags_yield.Rdata")
load("Outputs/Model Data/time_series_flows.Rdata")
load("Outputs/Model Data/yield_target_purchases.Rdata")

# 2. Process data ####

## 2.1. Event studies ####

### 2.1.1. First policy package ####

g_es_mf_ags <- arphitgg(data = gd_es_mf_ags) +
  agg_col(agg_aes(x = tenor, y = change, order = row), colour = RBA["Blue10"]) +
  agg_units("bps") +
  agg_ylim(-20, 30, 6) +
  # agg_title("Change in AGS Yields") +
  # agg_subtitle("Over 16 and 19 March 2020") +
  # agg_source(c("Bloomberg", "RBA")) +
  agg_xaxislabel("Term to maturity (years)")

g_es_mf_ois <- arphitgg(data = gd_es_mf_ois) +
  agg_col(agg_aes(x = tenor, y = change, order = row), colour = RBA["Pink2"]) +
  agg_units("bps") +
  agg_ylim(-40, 60, 6) +
  # agg_title("Change in AGS Spreads to OIS") +
  # agg_subtitle("Over 16 and 19 March 2020") +
  # agg_source(c("Bloomberg", "RBA")) +
  agg_xaxislabel("Term to maturity (years)")

g_es_mf_semis <- arphitgg(data = gd_es_mf_semis) +
  agg_col(agg_aes(x = tenor, y = change_mean, order = row), colour = RBA["Olive1"]) +
  agg_units("bps") +
  agg_ylim(-10, 10, 5) +
  # agg_title("Change in Semis Spreads to AGS") +
  # agg_subtitle("Over 16 and 19 March 2020") +
  # agg_footnote("Title* = Simple average for the states; excludes the territories") +
  # agg_source(c("Bloomberg", "RBA")) +
  agg_xaxislabel("Term to maturity (years)")

### 2.1.2. Second policy package ####

g_es_qe_ags <- arphitgg(data = gd_es_qe_ags) +
  agg_col(agg_aes(x = tenor, y = change, order = row), colour = RBA["Blue10"]) +
  agg_units("bps") +
  agg_ylim(-40, 10, 6) +
  # agg_title("Change in AGS Yields") +
  # agg_subtitle("Over key event study days") +
  # agg_source(c("Bloomberg", "RBA")) +
  agg_xaxislabel("Term to maturity (years)")

g_es_qe_ois <- arphitgg(data = gd_es_qe_ois) +
  agg_col(agg_aes(x = tenor, y = change, order = row), colour = RBA["Pink2"]) +
  agg_units("bps") +
  agg_ylim(-40, 10, 6) +
  # agg_title("Change in AGS Spreads to OIS") +
  # agg_subtitle("Over key event study days") +
  # agg_source(c("Bloomberg", "RBA")) +
  agg_xaxislabel("Term to maturity (years)")

g_es_qe_semis <- arphitgg(data = gd_es_qe_semis) +
  agg_col(agg_aes(x = tenor, y = change_mean, order = row), colour = RBA["Olive1"]) +
  agg_units("bps") +
  agg_ylim(-10, 10, 5) +
  # agg_title("Change in Semis Spreads to AGS") +
  # agg_subtitle("Over key event study days") +
  # agg_footnote("Title* = Simple average for the states; excludes the territories") +
  # agg_source(c("Bloomberg", "RBA")) +
  agg_xaxislabel("Term to maturity (years)")

## 2.2. Introduction of the yield target ####

g_yt_begin <- arphitgg(showallxlabels = FALSE) +
  agg_line(data = gd_yt_begin,
           agg_aes(x = time, y = `AGS yield`),
           colour = RBA["Blue10"]) +
  agg_line(data = gd_yt_begin,
           agg_aes(x = time, y = `3-year OIS rate`),
           colour = RBA["Pink2"]) +
  agg_ylim(0, 60, 7) +
  agg_units("bps") +
  agg_vline(x = 74, panel = "1", lty = 2) +
  agg_label("Yield target\nintroduced", x = 62, y = 45, panel = "1") +
  agg_xaxislabel("19 March 2020") +
  # agg_title("April 2023 AGS Yield and the Introduction of the Yield Target") +
  # agg_source(c("RBA", "Tullett Prebon", "Yieldbroker")) +
  agg_autolabel(ignore_existing_labels = TRUE)

## 2.3. Yield target and bond purchases ####

g_yt_purchases <- arphitgg(layout = "2h") +
  agg_line(data = gd_yt_purchases,
           agg_aes(x = date, y = value, group = type),
           panel = "1",
           colour = c(RBA["Blue10"], RBA["Pink2"])) +
  agg_col(data = purchases %>%
            filter(ticker %in% bonds_yt_plus & purpose != "QE") %>%
            group_by(date) %>%
            summarise(value_sum = sum(value) / 1e3,
                      .groups = "drop") %>%
            ungroup(),
          agg_aes(x = date, y = value_sum),
          panel = "3",
          colour = RBA["Violet2"],
          barcol = RBA["Violet2"]) +
  agg_xlim(2020, 2022.25) +
  agg_ylim(0, 1.25, 6, panel = "1") +
  agg_units("$b", panel = "3") +
  agg_vline(x = 2020 + 78 / 365, panel = "1", lty = 2, colour = RBA["Grey3"]) +
  agg_vline(x = 2020 + 78 / 365, panel = "3", lty = 2, colour = RBA["Grey3"]) +
  agg_label("Target\nintroduced", x = 2020.45, y = 1, panel = "1", colour = RBA["Grey3"]) +
  agg_vline(x = 2020 + 307 / 365, panel = "1", lty = 2, colour = RBA["Grey3"]) +
  agg_vline(x = 2020 + 307 / 365, panel = "3", lty = 2, colour = RBA["Grey3"]) +
  agg_label("Target\nlowered", x = 2020.66, y = 3, panel = "3", colour = RBA["Grey3"]) +
  agg_vline(x = 2021 + 306 / 365, panel = "1", lty = 2, colour = RBA["Grey3"]) +
  agg_vline(x = 2021 + 306 / 365, panel = "3", lty = 2, colour = RBA["Grey3"]) +
  agg_label("Target\nremoved", x = 2021.64, y = 2, panel = "3", colour = RBA["Grey3"]) +
  agg_title("3-year rates", panel = "1") +
  agg_title("Purchases", panel = "3") +
  # agg_title("Yield Target and Bond Purchases") +
  # agg_footnote("3-year rates* = For the April 2023 AGS maturity until 20 October 2020, and the April 2024 AGS maturity thereafter") +
  # agg_footnote("Purchases** = Of November 2022, April 2023 and April 2024 AGS") +
  # agg_source(c("Bloomberg", "RBA", "Yieldbroker")) +
  agg_autolabel(ignore_existing_labels = TRUE)

## 2.4. On-the-day changes in target bond yields ####

g_yt_otd <- ggplot() +
  geom_errorbar(data = gd_yt_otd,
                aes(x = reorder(type, order), ymin = change_lower, ymax = change_upper),
                position = position_dodge(),
                width = 0.1) +
  geom_point(data = gd_yt_otd,
             aes(x = reorder(type, order), y = change_mean, colour = type, size = 12)) +
  ylab("bps") + 
  xlab("") +
  rba_theme() +
  rba_syc() +
  rba_ylim(-4, 4) +
  rba_ylab_position(left_margin = -5, right_margin = -5) +
  scale_colour_manual(values = c(rba[["grey3"]], rba[["violet2"]])) +
  # labs(title = "On-the-day Changes in Target Bond Yields",
  #      subtitle = paste("From 5 August 2020 to 2 November 2021"),
  #      caption = "Notes: Dots show the average change; whiskers show a range within one standard deviation of the average
  #      \nSources: RBA; Yieldbroker") +
  rba_panel_text_size(title = 1.8, subtitle = 1.2)

## 2.5. Purchase announcements ####

g_announcements <- arphitgg(layout = "2v",
                            showallxlabels = FALSE) +
  agg_title("Yield target\npurchases", panel = "1") +
  agg_line(data = gd_announcements %>% filter(category == "YT"),
           agg_aes(x = time, y = value_diff_mean, group = subcategory),
           colour = RBA["Violet2"],
           panel = "1") +
  agg_title("Bond\npurchase\nprogram", panel = "2") +
  agg_line(data = gd_announcements %>% filter(category == "QE"),
           agg_aes(x = time, y = value_diff_mean, group = subcategory),
           colour = c(RBA["Blue10"], RBA["Olive1"]),
           panel = "2") +
  agg_ylim(-1.5, 1, 6) +
  agg_units("bps") +
  agg_vline(x = 20, panel = "1", lty = 2) +
  agg_vline(x = 20, panel = "2", lty = 2) +
  agg_bgshading(x1 = 59, x2 = 60, panel = "1") +
  agg_bgshading(x1 = 59, x2 = 72, panel = "2") +
  # agg_title("Yield Impact of Purchase Announcements") +
  # agg_subtitle("Dashed line indicates publication of announcement; shading indicates actual purchases") +
  # agg_footnote("Title* = Impact calculated by taking a simple average of changes in yields on bonds eligible for purchase around 11.15 am = 0 bps") +
  # agg_footnote("AGS** = Excludes purchases brought forward under the bond purchase program on 1 March 2021") +
  # agg_source(c("RBA", "Yieldbroker")) +
  agg_autolabel()

## 2.6. Flow effects using different impact measurement windows ####

### 2.6.1. Impact of yield target purchases ####

#### 2.6.1.1. Graph data ####

gd_windows_yt <- bind_rows(model_name_term(m_ags_1d, "ags_amount_1d", "target"),
                           model_name_term(m_ags_2d, "ags_amount_2d", "target"),
                           model_name_term(m_ags_3d, "ags_amount_3d", "target"),
                           model_name_term(m_ags_4d, "ags_amount_4d", "target"),
                           model_name_term(m_ois_1d, "ois_amount_1d", "target"),
                           model_name_term(m_ois_2d, "ois_amount_2d", "target"),
                           model_name_term(m_ois_3d, "ois_amount_3d", "target"),
                           model_name_term(m_ois_4d, "ois_amount_4d", "target"),
                           model_name_term(m_ags_share_1d, "ags_share_1d", "target"),
                           model_name_term(m_ags_share_2d, "ags_share_2d", "target"),
                           model_name_term(m_ags_share_3d, "ags_share_3d", "target"),
                           model_name_term(m_ags_share_4d, "ags_share_4d", "target"),
                           model_name_term(m_ois_share_1d, "ois_share_1d", "target"),
                           model_name_term(m_ois_share_2d, "ois_share_2d", "target"),
                           model_name_term(m_ois_share_3d, "ois_share_3d", "target"),
                           model_name_term(m_ois_share_4d, "ois_share_4d", "target")) %>%
  separate(name, c("rate", "type", "window")) %>%
  mutate(rate = recode(rate,
                       "ags" = "Target bond yields",
                       "ois" = "3-year OIS rate"),
         rate = fct_relevel(rate,
                            "Target bond yields",
                            "3-year OIS rate"),
         window = recode(window,
                         "1d" = "1",
                         "2d" = "2",
                         "3d" = "3",
                         "4d" = "4"))

#### 2.6.1.2. Graphs ####

##### 2.6.1.2.1. Purchases in $b ####

g_windows_yt_amount <- ggplot() +
  geom_errorbar(data = gd_windows_yt %>% filter(type == "amount"),
                aes(x = window, ymin = lower, ymax = upper),
                position = position_dodge(), 
                width = 0.2) +
  geom_point(data = gd_windows_yt %>% filter(type == "amount"),
             aes(x = window, y = estimate, colour = rate, size = 6)) +
  facet_wrap(. ~ rate) +
  ylab("bps") + 
  xlab("Days in impact measurement window") +
  rba_theme(facet = TRUE, x_title = TRUE) +
  rba_syc() +
  rba_ylim(-6, 6) +
  rba_ylab_position(left_margin = -5, right_margin = -5) +
  scale_colour_manual(values = c(rba[["blue10"]], rba[["pink2"]])) +
  # labs(title = "Impact of Yield Target Purchases (Amount in $b)",
  #      caption = "Notes: Dots show the estimated effect; whiskers show a 95 per cent confidence interval
  #      \nSources: FENICS; RBA; Tullett Prebon; Yieldbroker") +
  rba_panel_text_size(title = 1.8, subtitle = 1.2)

##### 2.6.1.2.2. Purchases as a share of the free-float of the target bond ####

g_windows_yt_share <- ggplot() +
  geom_errorbar(data = gd_windows_yt %>% filter(type == "share"),
                aes(x = window, ymin = lower, ymax = upper),
                position = position_dodge(), 
                width = 0.2) +
  geom_point(data = gd_windows_yt %>% filter(type == "share"),
             aes(x = window, y = estimate, colour = rate, size = 6)) +
  facet_wrap(. ~ rate) +
  ylab("bps") + 
  xlab("Days in impact measurement window") +
  rba_theme(facet = TRUE, x_title = TRUE) +
  rba_syc() +
  rba_ylim(-1.5, 1.5) +
  rba_ylab_position(left_margin = -10, right_margin = -10) +
  scale_colour_manual(values = c(rba[["blue10"]], rba[["pink2"]])) +
  # labs(title = "Impact of Yield Target Purchases (Share of Free-float)",
  #      caption = "Notes: Dots show the estimated effect; whiskers show a 95 per cent confidence interval
  #      \nSources: AOFM; FENICS; RBA; Tullett Prebon; Yieldbroker") +
  rba_panel_text_size(title = 1.8, subtitle = 1.2)

### 2.6.2. Impact of purchase eligibility ####

#### 2.6.2.1. Graph data ####

gd_windows_ts <- bind_rows(model_name_term(m_mf_ags_ts_anova_1d, "mf_ags_1d", "eligibleTRUE"),
                           model_name_term(m_mf_ags_ts_anova_2d, "mf_ags_2d", "eligibleTRUE"),
                           model_name_term(m_mf_ags_ts_anova_3d, "mf_ags_3d", "eligibleTRUE"),
                           model_name_term(m_mf_ags_ts_anova_4d, "mf_ags_4d", "eligibleTRUE"),
                           model_name_term(m_qe_ags_ts_anova_1d, "qe_ags_1d", "eligibleTRUE"),
                           model_name_term(m_qe_ags_ts_anova_2d, "qe_ags_2d", "eligibleTRUE"),
                           model_name_term(m_qe_ags_ts_anova_3d, "qe_ags_3d", "eligibleTRUE"),
                           model_name_term(m_qe_ags_ts_anova_4d, "qe_ags_4d", "eligibleTRUE"),
                           model_name_term(m_mf_semis_spreads_ts_anova_1d, "mf_semis_1d", "eligibleTRUE"),
                           model_name_term(m_mf_semis_spreads_ts_anova_2d, "mf_semis_2d", "eligibleTRUE"),
                           model_name_term(m_mf_semis_spreads_ts_anova_3d, "mf_semis_3d", "eligibleTRUE"),
                           model_name_term(m_mf_semis_spreads_ts_anova_4d, "mf_semis_4d", "eligibleTRUE"),
                           model_name_term(m_qe_semis_spreads_ts_anova_1d, "qe_semis_1d", "eligibleTRUE"),
                           model_name_term(m_qe_semis_spreads_ts_anova_2d, "qe_semis_2d", "eligibleTRUE"),
                           model_name_term(m_qe_semis_spreads_ts_anova_3d, "qe_semis_3d", "eligibleTRUE"),
                           model_name_term(m_qe_semis_spreads_ts_anova_4d, "qe_semis_4d", "eligibleTRUE")) %>%
  separate(name, c("purpose", "type", "window")) %>%
  mutate(type = recode(type,
                       "ags" = "AGS yields",
                       "semis" = "Semis spreads"),
         window = recode(window,
                         "1d" = "1",
                         "2d" = "2",
                         "3d" = "3",
                         "4d" = "4"))

#### 2.6.2.2. Graphs ####

##### 2.6.2.2.1. Market functioning purchases ####

g_windows_ts_mf <- ggplot() +
  geom_errorbar(data = gd_windows_ts %>% filter(purpose == "mf"),
                aes(x = window, ymin = lower, ymax = upper),
                position = position_dodge(), 
                width = 0.2) +
  geom_point(data = gd_windows_ts %>% filter(purpose == "mf"),
             aes(x = window, y = estimate, colour = type, size = 6)) +
  facet_wrap(. ~ type) +
  ylab("bps") + 
  xlab("Days in impact measurement window") +
  rba_theme(facet = TRUE, x_title = TRUE) +
  rba_syc() +
  rba_ylim(-4, 4) +
  rba_ylab_position(left_margin = -5, right_margin = -5) +
  scale_colour_manual(values = c(rba[["blue10"]], rba[["olive1"]])) +
  # labs(title = "Impact of Eligibility for Market Functioning Purchases",
  #      caption = "Notes: Dots show the estimated effect; whiskers show a 95 per cent confidence interval
  #      \nSources: RBA; Yieldbroker") +
  rba_panel_text_size(title = 1.8, subtitle = 1.2)

##### 2.6.2.2.2. Bond purchase program ####

g_windows_ts_qe <- ggplot() +
  geom_errorbar(data = gd_windows_ts %>% filter(purpose == "qe"),
                aes(x = window, ymin = lower, ymax = upper),
                position = position_dodge(), 
                width = 0.2) +
  geom_point(data = gd_windows_ts %>% filter(purpose == "qe"),
             aes(x = window, y = estimate, colour = type, size = 6)) +
  facet_wrap(. ~ type) +
  ylab("bps") + 
  xlab("Days in impact measurement window") +
  rba_theme(facet = TRUE, x_title = TRUE) +
  rba_syc() +
  rba_ylim(-1, 1) +
  rba_ylab_position(left_margin = -10, right_margin = -10) +
  scale_colour_manual(values = c(rba[["blue10"]], rba[["olive1"]])) +
  # labs(title = "Impact of Eligibility in the Bond Purchase Program",
  #      caption = "Notes: Dots show the estimated effect; whiskers show a 95 per cent confidence interval
  #      \nSources: RBA; Yieldbroker") +
  rba_panel_text_size(title = 1.8, subtitle = 1.2)

## 2.7. Change in difference between 10-year and 12-year rates ####

g_ls <- arphitgg() +
  agg_line(data = gd_ls,
           agg_aes(x = date, y = spread, group = type),
           colour = c(RBA["Blue10"], RBA["Pink2"], RBA["Olive1"])) +
  agg_units("bps") +
  agg_vline(x = 2020 + 307 / 365, panel = "1", lty = 2) +
  agg_label("Bond purchase\nprogram introduced", x = 2021.25, y = -10, panel = "1") +
  # agg_title("Change in Difference between 10-year and 12-year Rates") +
  # agg_subtitle("1-week moving average; 3 November 2020 = 0 bps") +
  # agg_footnote("Semis yield* = Simple average for the states; excludes the territories") +
  # agg_source(c("Bloomberg", "RBA")) +
  agg_autolabel(ignore_existing_labels = TRUE)

## 2.8. Stock lending rate changes ####

g_sl <- arphitgg(layout = "3v",
                 showallxlabels = FALSE,
                 srt = 45) +
  agg_line(data = gd_sl %>% filter(str_detect(time, "Mar")),
           agg_aes(x = time, y = `AGS yield`, order = row),
           panel = "1",
           colour = RBA["Blue10"]) +
  agg_vline(x = 52, panel = "1", lty = 2, lwd = 2, colour = RBA["Olive1"]) +
  agg_abline(x1 = 110, x2 = 110, y1 = 0, y2 = 17, panel = "1", lty = 2, colour = RBA["Grey3"]) +
  agg_line(data = gd_sl %>% filter(str_detect(time, "May")),
           agg_aes(x = time, y = `AGS yield`, order = row),
           panel = "2",
           colour = RBA["Blue10"]) +
  agg_vline(x = 52, panel = "2", lty = 2, lwd = 2, colour = RBA["Green6"]) +
  agg_line(data = gd_sl %>% filter(str_detect(time, "Oct")),
           agg_aes(x = time, y = `AGS yield`, order = row),
           panel = "3",
           colour = RBA["Blue10"]) +
  agg_vline(x = 100, panel = "3", lty = 2, lwd = 2, colour = RBA["Olive1"]) +
  agg_ylim(0, 20, 5) +
  agg_units("bps") +
  agg_label("Increase\nin fee", x = 110, y = 18.5, panel = "1", colour = RBA["Olive1"]) +
  agg_label("Decrease\nin fee", x = 115, y = 13.5, panel = "2", colour = RBA["Green6"]) +
  agg_label("Lowe\nspeech", x = 155, y = 7.5, panel = "1", colour = RBA["Grey3"]) +
  # agg_title("April 2024 AGS Yield and Stock Lending Fee Changes") +
  # agg_source(c("RBA", "Yieldbroker")) +
  agg_xaxislabel("Dates are labelled at the close", panel = "2")

## 2.9. Removal of the yield target ####

g_yt_end <- arphitgg(showallxlabels = FALSE, srt = 45) +
  agg_line(data = gd_yt_end,
           agg_aes(x = time, y = `AGS yield`, order = row),
           colour = RBA["Blue10"],
           panel = "1") +
  agg_line(data = gd_yt_end,
           agg_aes(x = time, y = `OIS rate`, order = row),
           colour = RBA["Pink2"],
           panel = "2") +
  agg_ylim(0, 100, 5, panel = "1") +
  agg_ylim(50, 150, 5, panel = "2") +
  agg_units("bps") +
  agg_vline(x = 676, panel = "1", lty = 2) +
  agg_vline(x = 240, panel = "1", lty = 2, colour = RBA["Grey3"]) +
  agg_label("Yield target\nremoved", x = 595, y = 25, panel = "1") +
  agg_label("CPI released for\nSeptember quarter", x = 121, y = 62.5, panel = "1", colour = RBA["Grey3"]) +
  agg_xaxislabel("Dates are labelled at the close") +
  # agg_title("April 2024 AGS Yield and the Removal of the Yield Target") +
  # agg_source(c("FENICS", "RBA", "Yieldbroker")) +
  agg_autolabel(ignore_existing_labels = TRUE)

## 2.10. 10-year yield differential ####

g_10y_differential <- arphitgg(layout = "2h") +
  agg_line(data = gd_10y_differential %>%
             select(-Differential) %>%
             gather("type", "value", -date),
           agg_aes(x = date, y = value, group = type),
           colour = c(RBA["Blue10"], RBA["Red6"]),
           panel = "1") +
  agg_line(data = gd_10y_differential,
           agg_aes(x = date, y = Differential),
           colour = RBA["Orange2"],
           panel = "3") +
  agg_ylim(0, 4, 5, panel = "1") +
  agg_ylim(-90, 60, 6, panel = "3") +
  agg_xlim(2018, 2022.25) +
  agg_units("bps", panel = "3") +
  agg_title("Spread", panel = "3") +
  # agg_title("10-year Government Bond Yields") +
  # agg_source(c("Bloomberg", "RBA", "Yieldbroker")) +
  agg_autolabel()

## 2.11. 10-year actual vs counterfactual ####

g_10y_model <- arphitgg(layout = "2h") +
  agg_line(data = gd_10y_model %>%
             select(date, Actual, Counterfactual) %>%
             gather("type", "value", -date),
           agg_aes(x = date, y = value, group = type),
           colour = c(RBA["Blue10"], RBA["Pink2"]),
           panel = "1") +
  agg_col(data = gd_10y_model,
          agg_aes(x = date, y = Residual),
          colour = RBA["Orange2"],
          panel = "3") +
  agg_ylim(0, 4, 5, panel = "1") +
  agg_ylim(-40, 40, 5, panel = "3") +
  agg_bgshading(x1 = year(date_10y_end) + month(date_10y_end) / 12, x2 = 2022.25, panel = "1") +
  agg_bgshading(x1 = year(date_10y_end) + month(date_10y_end) / 12, x2 = 2022.25, panel = "3") +
  agg_xlim(2018, 2022.25) +
  agg_units("bps", panel = "3") +
  agg_title("Difference", panel = "3") +
  # agg_title("10-year AGS Yield") +
  # agg_subtitle("End of month; shading indicates the out-of-sample period for the counterfactual") +
  # agg_footnote("Counterfactual* = Calculated by cumulating modelled changes in the 10-year AGS yield") +
  # agg_source(c("Bloomberg", "FRBNY", "RBA", "Yieldbroker")) +
  agg_autolabel(ignore_existing_labels = TRUE)

## 2.12. Bid-offer spreads ####

g_bo <- arphitgg(layout = "2h") +
  agg_line(data = gd_bo %>%
             filter(issuer == "Cth" & tenor %in% c("Yield target bonds", "5-year", "10-year")) %>%
             mutate(tenor = fct_relevel(tenor, "5-year", "Yield target bonds", "10-year")),
           agg_aes(x = date, y = bo, group = tenor),
           colour = c(RBA["Grey3"], RBA["Violet2"], RBA["Blue10"]),
           panel = "1") +
  agg_line(data = gd_bo %>%
             filter(issuer %in% c("NSW", "Vic", "Qld", "WA") & tenor == "5-year") %>%
             mutate(issuer = fct_relevel(issuer, "WA", "Qld", "Vic", "NSW")),
           agg_aes(x = date , y = bo, group = issuer),
           colour = c(RBA["Orange3"], RBA["Olive7"], RBA["Olive1"], RBA["Green6"]),
           panel = "3") +
  agg_title("AGS", panel = "1") +
  agg_title("Semis", panel = "3") +
  agg_subtitle("5-year", panel = "3") +
  agg_ylim(0, 40, 5, panel = "1") +
  agg_ylim(0, 60, 5, panel = "3") +
  agg_xlim(2020, 2022.25) +
  agg_units("bps") +
  # agg_title("Bid-offer Spreads") +
  # agg_source(c("RBA", "Yieldbroker")) +
  agg_autolabel()

## 2.13. Yield curve fitting errors ####

g_ycfe <- arphitgg() +
  agg_line(data = gd_ycfe,
           agg_aes(x = date, y = ycfe, group = type),
           colour = c(RBA["Blue10"], RBA["Olive1"])) +
  agg_xlim(2020, 2022.25) +
  agg_units("bps") +
  # agg_title("Yield Curve Fitting Errors") +
  # agg_subtitle("1-week moving average; only bonds eligible for the bond purchase program") +
  # agg_source(c("Bloomberg", "RBA")) +
  # agg_footnote("Semis* = Simple average for the states and territories") +
  agg_autolabel()

# 3. Produce outputs ####

## 3.1. Graphs ####

agg_draw(g_es_mf_ags, "Outputs/Change in AGS Yields 1.emf+")
agg_draw(g_es_mf_ois, "Outputs/Change in AGS Spreads to OIS 1.emf+")
agg_draw(g_es_mf_semis, "Outputs/Change in Semis Spreads to AGS 1.emf+")
agg_draw(g_es_qe_ags, "Outputs/Change in AGS Yields 2.emf+")
agg_draw(g_es_qe_ois, "Outputs/Change in AGS Spreads to OIS 2.emf+")
agg_draw(g_es_qe_semis, "Outputs/Change in Semis Spreads to AGS 2.emf+")
agg_draw(g_yt_begin, "Outputs/April 2023 AGS Yield and the Introduction of the Yield Target.emf+")
agg_draw(g_yt_purchases, "Outputs/Yield Target and Bond Purchases.emf+")
ggsave_rba(plot = g_yt_otd, "Outputs/On-the-day Changes in Target Bond Yields.png")
agg_draw(g_announcements, "Outputs/Yield Impact of Purchase Announcements.emf+")
ggsave_rba(plot = g_windows_yt_amount, "Outputs/Impact of Yield Target Purchases ($b).png")
ggsave_rba(plot = g_windows_yt_share, "Outputs/Impact of Yield Target Purchases (ppts).png")
ggsave_rba(plot = g_windows_ts_mf, "Outputs/Impact of Eligibility for Market Functioning Purchases.png")
ggsave_rba(plot = g_windows_ts_qe, "Outputs/Impact of Eligibility in the Bond Purchase Program.png")
agg_draw(g_ls, "Outputs/Change in Difference between 10-year and 12-year Rates.emf+")
agg_draw(g_sl, "Outputs/April 2024 AGS Yield and Stock Lending Fee Changes.emf+")
agg_draw(g_yt_end, "Outputs/April 2024 AGS Yield and the Removal of the Yield Target.emf+")
agg_draw(g_10y_differential, "Outputs/10-year Government Bond Yields.emf+")
agg_draw(g_10y_model, "Outputs/10-year AGS Yield.emf+")
agg_draw(g_bo, "Outputs/Bid-offer Spreads.emf+")
agg_draw(g_ycfe, "Outputs/Yield Curve Fitting Errors.emf+")

## 3.2. Graph data ####

agg_draw(g_es_mf_ags, "Outputs/Graph Data/Change in AGS Yields 1.xlsx")
agg_draw(g_es_mf_ois, "Outputs/Graph Data/Change in AGS Spreads to OIS 1.xlsx")
agg_draw(g_es_mf_semis, "Outputs/Graph Data/Change in Semis Spreads to AGS 1.xlsx")
agg_draw(g_es_qe_ags, "Outputs/Graph Data/Change in AGS Yields 2.xlsx")
agg_draw(g_es_qe_ois, "Outputs/Graph Data/Change in AGS Spreads to OIS 2.xlsx")
agg_draw(g_es_qe_semis, "Outputs/Graph Data/Change in Semis Spreads to AGS 2.xlsx")
agg_draw(g_yt_begin, "Outputs/Graph Data/April 2023 AGS Yield and the Introduction of the Yield Target.xlsx")
agg_draw(g_yt_purchases, "Outputs/Graph Data/Yield Target and Bond Purchases.xlsx")
write_csv(gd_yt_otd, "Outputs/Graph Data/On-the-day Changes in Target Bond Yields.csv")
agg_draw(g_announcements, "Outputs/Graph Data/Yield Impact of Purchase Announcements.xlsx")
write_csv(gd_windows_yt, "Outputs/Graph Data/Impact of Yield Target Purchases.csv")
write_csv(gd_windows_ts, "Outputs/Graph Data/Impact of Purchase Eligibility.csv")
agg_draw(g_ls, "Outputs/Graph Data/Change in Difference between 10-year and 12-year Rates.xlsx")
agg_draw(g_sl, "Outputs/Graph Data/April 2024 AGS Yield and Stock Lending Fee Changes.xlsx")
agg_draw(g_yt_end, "Outputs/Graph Data/April 2024 AGS Yield and the Removal of the Yield Target.xlsx")
agg_draw(g_10y_differential, "Outputs/Graph Data/10-year Government Bond Yields.xlsx")
agg_draw(g_10y_model, "Outputs/Graph Data/10-year AGS Yield.xlsx")
agg_draw(g_bo, "Outputs/Graph Data/Bid-offer Spreads.xlsx")
agg_draw(g_ycfe, "Outputs/Graph Data/Yield Curve Fitting Errors.xlsx")