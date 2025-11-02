# Produce Graphs
# Ben Jackman and Dmitry Titkov / MPI / DM
# November 2024

# 0. Preliminaries ####

## 0.1. Packages ####

library(ggfixest)
library(ggrba)
library(lubridate)
library(patchwork)
library(tidyverse)

## 0.2. Functions ####

source("define_functions.R")

## 0.3. Definitions ####

colours <- c(
  "Pre-narrow" = RBA[["Olive1"]],
  "Post-narrow" = RBA[["Olive4"]],
  "Post-widen" = RBA[["Olive7"]],
  "Post-delink" = RBA[["Olive10"]],
  "3-year" = RBA[["Purple2"]],
  "10-year" = RBA[["Orange2"]],
  "Other" = RBA[["Grey3"]],
  "3-year (LHS)" = RBA[["Purple2"]],
  "10-year (RHS)" = RBA[["Orange2"]],
  "Bid-asks" = "#21918c", #  RBA[["DarkGreen3"]],
  "Bid-asks (pre-widening)" = "#5ec962", # RBA[["Green3"]],
  "Bid-asks (post-widening)" = "#21918c", # RBA[["DarkGreen3"]], 
  "Best depth" = "#3b528b", # RBA[["Blue10"]],
  "Turnover" = "#440154", #  RBA[["Aqua2"]],
  "Price impact" = "#fde725" # RBA[["Olive1"]]
)

# 1. Load data ####

load("Inputs/settings_and_metadata.Rdata")
load("Intermediates/indicators.Rdata")
load("Intermediates/introduction.Rdata")
load("Intermediates/review.Rdata")
source("model_events.R")
source("model_rolls.R")
source("model_syndications.R")
source("model_widening.R")

# 2. Process data ####

## 2.1. Introduction ####

### 2.1.1. Average daily turnover ####

# hack job to fix the x-axis ticks (endorse by AH so it's good actually)
gd_turnover <- gd_turnover |> 
  dplyr::mutate(date_fix = lubridate::floor_date(date, unit = "months") + lubridate::days(16))

g_turnover_asx <- ggrba(gd_turnover %>% filter(type == "asx")) +
  geom_col(aes(x = date_fix, y = value, fill = tenor)) +
  scale_fill_manual(values = colours) +
  scale_y_continuous_rba(limits = c(0, 100), units = "$b") +
  labs(title = "AGS futures")

g_turnover_aofm <- ggrba(gd_turnover %>% filter(type == "aofm") %>% mutate(tenor = factor(tenor, level = c("3-year", "10-year", "Other")))) +
  geom_col(aes(x = date_fix, y = value, fill = tenor), position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = colours) +
  scale_y_continuous_rba(limits = c(0, 10), units = "$b") +
  labs(title = "AGS") # +
# autolabel_var_rba(tenor)

g_turnover <- g_turnover_asx + autolabel_rba(g_turnover_aofm) +
  plot_layout(nrow = 2, ncol = 1) +
  multipanel_title_rba(
    title = "Average Daily Turnover in AGS Markets",
    subtitle = "Face value, monthly"
  ) +
  legend_rba(position = "bottom", nrow  = 1)

### 2.1.2. Average intraday turnover ####

br_sessions_3 <- sessions_3 %>%
  filter(hour_new == 1) %>%
  pull(character) %>%
  unique()

br_sessions_10 <- sessions_10 %>%
  filter(hour_new == 1) %>%
  pull(character) %>%
  unique()

g_sessions_3 <- ggrba(sessions_3) +
  geom_col(aes(x = reorder(character, time), y = to), fill = RBA[["Purple2"]]) +
  scale_x_discrete_rba(breaks = br_sessions_3, guide = guide_axis_rba(centre_labels = "labels")) +
  scale_y_continuous_rba(limits = c(0, 2), units = "’000") +
  labs(title = "3-year")

g_sessions_10 <- ggrba(sessions_10) +
  geom_col(aes(x = reorder(character, time), y = to), fill = RBA[["Orange2"]]) +
  scale_x_discrete_rba(breaks = br_sessions_3, guide = guide_axis_rba(centre_labels = "labels")) +
  scale_y_continuous_rba(limits = c(0, 2), units = "’000") +
  axis_titles_rba(ytitle = FALSE) +
  xlab("Hour of day in Sydney") +
  labs(title = "10-year")

g_sessions <- g_sessions_3 + g_sessions_10 +
  plot_layout(nrow = 2, ncol = 1) +
  plot_size_rba(plot.width = grid::unit(250, "mm")) +
  multipanel_title_rba(
    title = "Average Intraday Turnover in 3- and 10-year Futures",
    subtitle = "5-minute intervals"
  )

### 2.1.3. Daily liquidity measures ####

#### 2.1.3.1. 3-year ####

g_daily_bo_3 <- ggrba(gd_daily %>% filter(tenor == "3-year")) +
  geom_line(aes(x = date, y = bo, colour = tenor)) +
  scale_colour_manual(values = colours) +
  scale_x_date_rba(limits = c(as_date("2019-01-01"), as_date("2025-12-31"))) +
  scale_y_continuous_rba(limits = c(0, 2), units = "bps") +
  labs(title = "Bid-ask spreads")

g_daily_bd_3 <- ggrba(gd_daily %>% filter(tenor == "3-year")) +
  geom_line(aes(x = date, y = bd, colour = tenor)) +
  scale_colour_manual(values = colours) +
  scale_x_date_rba(limits = c(as_date("2019-01-01"), as_date("2025-12-31"))) +
  scale_y_continuous_rba(limits = c(0.01, 100), 
                         units = "’000", 
                         transform = scales::log_trans(), 
                         breaks = c(0.01 ,0.1, 1, 10, 100),
                         expand = expansion(mult = c(0.001, 0))) + # ,
  # keep_first_label = "keep",
  # check.overlap = FALSE) +
  labs(
    title = "Best depth",
    subtitle = "Log scale"
  )

g_daily_to_3 <- ggrba(gd_daily %>% filter(tenor == "3-year")) +
  geom_line(aes(x = date, y = to, colour = tenor)) +
  scale_colour_manual(values = colours) +
  scale_x_date_rba(limits = c(as_date("2019-01-01"), as_date("2025-12-31"))) +
  scale_y_continuous_rba(limits = c(0, 400), units = "’000") +
  labs(title = "Turnover")

g_daily_pi_3 <- ggrba(gd_daily %>% filter(tenor == "3-year")) +
  geom_line(aes(x = date, y = pi, colour = tenor)) +
  scale_colour_manual(values = colours) +
  scale_x_date_rba(limits = c(as_date("2019-01-01"), as_date("2025-12-31"))) +
  scale_y_continuous_rba(limits = c(0, 50), units = "bps") +
  labs(title = "Price impact")

g_daily_3 <- g_daily_bo_3 + g_daily_to_3 + g_daily_pi_3 + g_daily_bd_3 +
  plot_layout(nrow = 4, ncol = 1) +
  plot_size_rba(plot.height = grid::unit(280, "mm")) +
  multipanel_title_rba(
    title = "3-year Futures Measures of Liquidity",
    subtitle = "Daily, total for turnover and averages for other measures"
  )

#### 2.1.3.2. 10-year ####

g_daily_bo_10 <- ggrba(gd_daily %>% filter(tenor == "10-year")) +
  geom_line(aes(x = date, y = bo, colour = tenor)) +
  scale_colour_manual(values = colours) +
  scale_x_date_rba(limits = c(as_date("2019-01-01"), as_date("2025-12-31"))) +
  scale_y_continuous_rba(limits = c(0, 1.25), breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25), units = "bps") +
  labs(title = "Bid-ask spreads")

g_daily_bd_10 <- ggrba(gd_daily %>% filter(tenor == "10-year")) +
  geom_line(aes(x = date, y = bd, colour = tenor)) +
  scale_colour_manual(values = colours) +
  scale_x_date_rba(limits = c(as_date("2019-01-01"), as_date("2025-12-31"))) +
  scale_y_continuous_rba(limits = c(0.02, 62.5), 
                         units = "’000", 
                         transform = scales::log_trans(), 
                         breaks = c(0.02, 0.1, 0.5, 2.5, 12.5, 62.5),
                         keep_first_label = "keep",
                         check.overlap = FALSE,
                         expand = expansion(mult = c(0.001, 0))) +
  labs(
    title = "Best depth",
    subtitle = "Log scale"
  )

g_daily_to_10 <- ggrba(gd_daily %>% filter(tenor == "10-year")) +
  geom_line(aes(x = date, y = to, colour = tenor)) +
  scale_colour_manual(values = colours) +
  scale_x_date_rba(limits = c(as_date("2019-01-01"), as_date("2025-12-31"))) +
  scale_y_continuous_rba(limits = c(0, 1000), units = "’000") +
  labs(title = "Turnover")

g_daily_pi_10 <- ggrba(gd_daily %>% filter(tenor == "10-year")) +
  geom_line(aes(x = date, y = pi, colour = tenor)) +
  scale_colour_manual(values = colours) +
  scale_x_date_rba(limits = c(as_date("2019-01-01"), as_date("2025-12-31"))) +
  scale_y_continuous_rba(limits = c(0, 50), units = "bps") +
  labs(title = "Price impact")

g_daily_10 <- g_daily_bo_10 + g_daily_to_10 + g_daily_pi_10 + g_daily_bd_10 +
  plot_layout(nrow = 4, ncol = 1) +
  plot_size_rba(plot.height = grid::unit(280, "mm")) +
  multipanel_title_rba(
    title = "10-year Futures Measures of Liquidity",
    subtitle = "Daily, total for turnover and averages for other measures"
  )

### 2.1.4. Autocorrelations ####
#Manually labelling at RDP Graph Editor's request 

g_autocorrelation_3 <- ggrba(gd_autocorrelation_3) +
  geom_line(aes(x = day, y = value, colour = type)) +
  scale_colour_manual(values = colours) +
  scale_x_continuous_rba(breaks = c(0, 5, 10, 15, 20, 25, 30), units = "") +
  scale_y_continuous_rba(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1.0), units = "") +
  labs(title = "3-year") +
  # autolabel_var_rba(type) +   # autolabel_rba(g_autocorrelation_3, print_code = TRUE)
  annotate(
    GeomText,
    x = 22.9914417744917,
    y = 0.397236180904523,
    label = "Bid-asks (post-widen)",
    colour = "#21918c", # "#264e37ff"
    size = 7.029,
    hjust = 0.5, vjust = 0.5) +
  annotate(
    GeomText,
    x = 6.7486136783734,
    y = 0.56208542713568,
    label = "Bid-asks (pre-widen)",
    colour = "#5ec962", # "#5ec152ff"
    size = 7.029,
    hjust = 0.5, vjust = 0.5) 


g_autocorrelation_10 <- ggrba(gd_autocorrelation_10) +
  geom_line(aes(x = day, y = value, colour = type)) +
  scale_colour_manual(values = colours) +
  scale_x_continuous_rba(breaks = c(0, 5, 10, 15, 20, 25, 30), units = "") +
  scale_y_continuous_rba(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1.0), units = "") +
  axis_titles_rba(ytitle = FALSE) +
  xlab("Lag in days") +
  labs(title = "10-year") +
  # Plot: 0
  annotate(
    GeomText,
    x = 22.48521256931608,
    y = 0.395276381909548,
    label = "Price impact",
    colour = "#fde725", # "#ffb611ff"
    size = 7.029,
    hjust = 0.5, vjust = 0.5) +
  # Plot: 0
  annotate(
    GeomText,
    x = 4.0221811460259,
    y = 0.1254773869346734,
    label = "Turnover",
    colour = "#440154", # "#41dac5ff"
    size = 7.029,
    hjust = 0.5, vjust = 0.5) +
  annotate(
    GeomText,
    x = 5.3493530499076,
    y = 0.858542713567839,
    label = "Best depth",
    colour = "#3b528b", # "#001ca7ff"
    size = 7.029,
    hjust = 0.5, vjust = 0.5) +
  annotate(
    GeomText,
    x = 9.4214417744917,
    y = 0.387236180904523,
    label = "Bid-asks",
    colour = "#21918c", # "#264e37ff"
    size = 7.029,
    hjust = 0.5, vjust = 0.5)

# Legend not trivial for this so need to manually label (use autolabelling output then tweak.)
g_autocorrelation <- g_autocorrelation_3 + g_autocorrelation_10 + # autolabel_rba(g_autocorrelation_3)
  plot_layout(nrow = 2, ncol = 1) +
  multipanel_title_rba(
    title = "Autocorrelations of Liquidity Measures",
    subtitle = "Autocorrelations by lag, excludes futures roll periods"
  )



## 2.2. Review of periods of interest ####

### 2.2.1. March 2020 ####

gd_mar2020 <- gd_intraday %>% filter(date >= as_date("2020-03-09") & date <= as_date("2020-03-27"))

br_mar2020 <- gd_mar2020 %>%
  filter(day_new == 1) %>%
  pull(character) %>%
  unique()

g_mar2020_bo <- ggrba(gd_mar2020, aes(x = reorder(character, datetime), y = bo, group = tenor, colour = tenor)) +
  geom_line() +
  scale_colour_manual(values = colours) +
  scale_x_discrete_rba(breaks = br_mar2020, guide = guide_axis_rba(centre_labels = "labels")) +
  scale_y_continuous_rba(limits = c(0, 2.5), units = "bps") +
  labs(title = "Bid-ask spreads")

g_mar2020_bd <- ggrba(gd_mar2020, aes(x = reorder(character, datetime), y = bd, group = tenor, colour = tenor)) +
  geom_line() +
  scale_colour_manual(values = colours) +
  scale_x_discrete_rba(breaks = br_mar2020, guide = guide_axis_rba(centre_labels = "labels")) +
  scale_y_continuous_rba(limits = c(0.025, 25.6), units = "’000", transform = scales::log_trans(), breaks = c(0.025, 0.1, 0.4, 1.6, 6.4, 25.6), expand = expansion(mult = c(0.001, 0))) +
  axis_titles_rba(ytitle = FALSE) +
  xlab("March 2020") +
  labs(
    title = "Best depth",
    subtitle = "Log scale"
  )

g_mar2020_to <- ggrba(gd_mar2020, aes(x = reorder(character, datetime), y = to, group = tenor, colour = tenor)) +
  geom_line() +
  scale_colour_manual(values = colours) +
  scale_x_discrete_rba(breaks = br_mar2020, guide = guide_axis_rba(centre_labels = "labels")) +
  scale_y_continuous_rba(limits = c(0, 25), units = "’000") +
  labs(title = "Turnover") +
  # Plot: 0
  annotate(
    GeomText,
    x = 768.48, # 468.48
    y = 17.3718592964824,
    label = "3-year",
    colour = "#752fa5ff",
    size = 7.029,
    hjust = 0.5, vjust = 0.5)

g_mar2020_pi <- ggrba(gd_mar2020, aes(x = reorder(character, datetime), y = pi, group = tenor, colour = tenor)) +
  geom_line() +
  scale_colour_manual(values = colours) +
  scale_x_discrete_rba(breaks = br_mar2020, guide = guide_axis_rba(centre_labels = "labels")) +
  scale_y_continuous_rba(limits = c(0, 125), units = "bps", breaks = c(0, 25, 50, 75, 100, 125)) +
  labs(title = "Price impact") + #  autolabel_var_rba(tenor)
  annotate(
    GeomText,
    x = 830.48,
    y = 60.4371859296482,
    label = "10-year",
    colour = "#ff6f20ff",
    size = 7.029,
    hjust = 0.5, vjust = 0.5) 

# autolabel_rba(g_mar2020_pi, print_code = TRUE)

g_mar2020 <- g_mar2020_bo + g_mar2020_to + g_mar2020_pi + g_mar2020_bd + # autolabel_rba(g_mar2020_pi)
  plot_layout(nrow = 4, ncol = 1) +
  plot_size_rba(plot.height = grid::unit(280, "mm")) +
  multipanel_title_rba(
    title = "Measures of Liquidity in March 2020",
    subtitle = "5-minute intervals"
  )

### 2.2.2. October-November 2021 ####

gd_octnov2021 <- gd_intraday %>% filter(date >= as_date("2021-10-25") & date <= as_date("2021-11-12")) |> 
  dplyr::mutate(pi = case_when(pi == 0 ~ NA, 
                               .default = pi))


br_octnov2021 <- gd_octnov2021 %>%
  filter(day_new == 1) %>%
  pull(character) %>%
  unique()

g_octnov2021_bo <- ggrba(gd_octnov2021, aes(x = reorder(character, datetime), y = bo, group = tenor, colour = tenor)) +
  geom_line() +
  scale_colour_manual(values = colours) +
  scale_x_discrete_rba(breaks = br_octnov2021, guide = guide_axis_rba(centre_labels = "labels")) +
  scale_y_continuous_rba(limits = c(0, 1.5), units = "bps") +
  labs(title = "Bid-ask spreads")

g_octnov2021_bd <- ggrba(gd_octnov2021, aes(x = reorder(character, datetime), y = bd, group = tenor, colour = tenor)) +
  geom_line() +
  scale_colour_manual(values = colours) +
  scale_x_discrete_rba(breaks = br_octnov2021, guide = guide_axis_rba(centre_labels = "labels")) +
  scale_y_continuous_rba(limits = c(0.02, 20), 
                         units = "’000", 
                         transform = scales::log_trans(), 
                         breaks = c(0.02,0.2, 2, 20),
                         expand = expansion(mult = c(0.001, 0))) +
  axis_titles_rba(ytitle = FALSE) +
  xlab("October and November 2021") +
  labs(
    title = "Best depth",
    subtitle = "Log scale"
  )

g_octnov2021_to <- ggrba(gd_octnov2021, aes(x = reorder(character, datetime), y = to, group = tenor, colour = tenor)) +
  geom_line() +
  scale_colour_manual(values = colours) +
  scale_x_discrete_rba(breaks = br_octnov2021, guide = guide_axis_rba(centre_labels = "labels")) +
  scale_y_continuous_rba(limits = c(0, 15), units = "’000") +
  labs(title = "Turnover") +
  # Plot: 0
  annotate(
    GeomText,
    x = 577.148148148148,
    y = 7.1005025125628,
    label = "3-year",
    colour = "#752fa5ff",
    size = 7.029,
    hjust = 0.5, vjust = 0.5)

g_octnov2021_pi <- ggrba(gd_octnov2021, aes(x = reorder(character, datetime), y = pi, group = tenor, colour = tenor)) +
  geom_line() +
  scale_colour_manual(values = colours) +
  scale_x_discrete_rba(breaks = br_octnov2021, guide = guide_axis_rba(centre_labels = "labels")) +
  scale_y_continuous_rba(limits = c(0, 60), breaks = c(0, 20, 40, 60), units = "bps") +
  labs(title = "Price impact") + # autolabel_var_rba(tenor) + 
  annotate(
    GeomText,
    x = 650.91851851852,
    y = 30.0251256281407,
    label = "10-year",
    colour = "#ff6f20ff",
    size = 7.029,
    hjust = 0.5, vjust = 0.5) 

autolabel_rba(g_octnov2021_pi, print_code = TRUE)

g_octnov2021 <- g_octnov2021_bo + g_octnov2021_to + g_octnov2021_pi + g_octnov2021_bd + # autolabel_rba(g_octnov2021_pi)
  plot_layout(nrow = 4, ncol = 1) +
  plot_size_rba(plot.height = grid::unit(280, "mm")) +
  multipanel_title_rba(
    title = "Measures of Liquidity in Oct-Nov 2021",
    subtitle = "5-minute intervals"
  )

### 2.2.3. March-April 2025 ####

gd_marapr2025 <- gd_intraday %>% filter(date >= as_date("2025-03-24") & date <= as_date("2025-04-12"))

br_marapr2025 <- gd_marapr2025 %>%
  filter(day_new == 1) %>%
  pull(character) %>%
  unique()

g_marapr2025_bo <- ggrba(gd_marapr2025, aes(x = reorder(character, datetime), y = bo, group = tenor, colour = tenor)) +
  geom_line() +
  scale_colour_manual(values = colours) +
  scale_x_discrete_rba(breaks = br_marapr2025, guide = guide_axis_rba(centre_labels = "labels")) +
  scale_y_continuous_rba(limits = c(0, 2), units = "bps") +
  labs(title = "Bid-ask spreads")

g_marapr2025_bd <- ggrba(gd_marapr2025, aes(x = reorder(character, datetime), y = bd, group = tenor, colour = tenor)) +
  geom_line() +
  scale_colour_manual(values = colours) +
  scale_x_discrete_rba(breaks = br_marapr2025, guide = guide_axis_rba(centre_labels = "labels")) +
  scale_y_continuous_rba(limits = c(0.04, 125), 
                         units = "’000", 
                         transform = scales::log_trans(), 
                         breaks = c(0.04, 0.2, 1, 5, 25, 125),
                         expand = expansion(mult = c(0.0001, 0))) +
  axis_titles_rba(ytitle = FALSE) +
  xlab("March and April 2025") +
  labs(
    title = "Best depth",
    subtitle = "Log scale"
  )

g_marapr2025_to <- ggrba(gd_marapr2025, aes(x = reorder(character, datetime), y = to, group = tenor, colour = tenor)) +
  geom_line() +
  scale_colour_manual(values = colours) +
  scale_x_discrete_rba(breaks = br_marapr2025, guide = guide_axis_rba(centre_labels = "labels")) +
  scale_y_continuous_rba(limits = c(0, 40), units = "’000") +
  labs(title = "Turnover") +
  # Plot: 0
  annotate(
    GeomText,
    x = 565.814074074074,
    y = 25.63819095477387,
    label = "3-year",
    colour = "#752fa5ff",
    size = 7.029,
    hjust = 0.5, vjust = 0.5)


g_marapr2025_pi <- ggrba(gd_marapr2025, aes(x = reorder(character, datetime), y = pi, group = tenor, colour = tenor)) +
  geom_line() +
  scale_colour_manual(values = colours) +
  scale_x_discrete_rba(breaks = br_marapr2025, guide = guide_axis_rba(centre_labels = "labels")) +
  scale_y_continuous_rba(limits = c(0, 20), breaks = c(0, 5, 10, 15, 20), units = "bps") +
  labs(title = "Price impact") + #  autolabel_var_rba(tenor)
  annotate(
    GeomText,
    x = 650.715925925926,
    y = 12.894472361809,
    label = "10-year",
    colour = "#ff6f20ff",
    size = 7.029,
    hjust = 0.5, vjust = 0.5) 

# ggrba::autolabel_rba(g_marapr2025_pi, print_code = TRUE)

g_marapr2025 <- g_marapr2025_bo + g_marapr2025_to + g_marapr2025_pi + g_marapr2025_bd + # autolabel_rba(g_marapr2025_pi)
  plot_layout(nrow = 4, ncol = 1) +
  plot_size_rba(plot.height = grid::unit(280, "mm")) +
  multipanel_title_rba(
    title = "Measures of Liquidity in Mar-Apr 2025",
    subtitle = "5-minute intervals"
  )

## 2.3. News events ####

### 2.3.1. Bid-asks ####
# change geom_vline colour from RBA["Default6"] and to dashed style. 

g_decisions_bo_3 <- ggrba(iplot_data(m_decisions_bo_3, .ci_level = 0.90) %>% mutate(x = str_sub(x, 1, 5))) +
  geom_vline(xintercept = (as.numeric(time_decision) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") + 
  geom_point(aes(x = x, y = y), colour = RBA["Blue10"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Blue10"]) +
  scale_y_continuous_rba(limits = c(-0.1, 0.3), units = "bps") +
  labs(
    title = "RBA decisions",
    subtitle = "3-year"
  ) +
  geom_special_gridline_rba()

g_releases_bo_3 <- ggrba(iplot_data(m_releases_bo_3, .ci_level = 0.90) %>% mutate(x = str_sub(x, 1, 5))) +
  geom_vline(xintercept = (as.numeric(time_release) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Aqua4"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Aqua4"]) +
  scale_y_continuous_rba(limits = c(-0.1, 0.3), units = "bps") +
  labs(
    title = "ABS releases",
    subtitle = "3-year"
  ) +
  geom_special_gridline_rba()

g_decisions_bo_10 <- ggrba(iplot_data(m_decisions_bo_10, .ci_level = 0.90) %>% mutate(x = str_sub(x, 1, 5))) +
  geom_vline(xintercept = (as.numeric(time_decision) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Blue10"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Blue10"]) +
  scale_x_discrete(breaks = times(10, 16, 7200) %>% pull(time) %>% as.character() %>% str_sub(1, 5)) +
  scale_y_continuous_rba(limits = c(-0.05, 0.15), units = "bps") +
  labs(subtitle = "10-year") +
  geom_special_gridline_rba()

g_releases_bo_10 <- ggrba(iplot_data(m_releases_bo_10, .ci_level = 0.90) %>% mutate(x = str_sub(x, 1, 5))) +
  geom_vline(xintercept = (as.numeric(time_release) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Aqua4"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Aqua4"]) +
  scale_x_discrete(breaks = times(10, 16, 7200) %>% tail(-1) %>% pull(time) %>% as.character() %>% str_sub(1, 5)) +
  scale_y_continuous_rba(limits = c(-0.05, 0.15), units = "bps") +
  labs(subtitle = "10-year") +
  geom_special_gridline_rba()

g_news_bo <- g_decisions_bo_3 + g_releases_bo_3 + g_decisions_bo_10 + g_releases_bo_10 +
  plot_layout(nrow = 2, ncol = 2) +
  multipanel_title_rba(
    title = "Bid-ask Spreads for News Events",
    subtitle = "Intraday effects, 5-minute intervals, dashed line = event time"
  )

### 2.3.2. Best depth ####

g_decisions_bd_3 <- ggrba(iplot_data(m_decisions_bd_3, .ci_level = 0.90) %>% mutate(
  x = str_sub(x, 1, 5),
  y = (exp(y) - 1) * 100,
  ci_low = (exp(ci_low) - 1) * 100,
  ci_high = (exp(ci_high) - 1) * 100
)) +
  geom_vline(xintercept = (as.numeric(time_decision) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Blue10"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Blue10"]) +
  scale_y_continuous_rba(limits = c(-100, 50)) +
  labs(
    title = "RBA decisions",
    subtitle = "3-year"
  ) +
  geom_special_gridline_rba()

g_releases_bd_3 <- ggrba(iplot_data(m_releases_bd_3, .ci_level = 0.90) %>% mutate(
  x = str_sub(x, 1, 5),
  y = (exp(y) - 1) * 100,
  ci_low = (exp(ci_low) - 1) * 100,
  ci_high = (exp(ci_high) - 1) * 100
)) +
  geom_vline(xintercept = (as.numeric(time_release) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Aqua4"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Aqua4"]) +
  scale_y_continuous_rba(limits = c(-100, 50)) +
  labs(
    title = "ABS releases",
    subtitle = "3-year"
  ) +
  geom_special_gridline_rba()

g_decisions_bd_10 <- ggrba(iplot_data(m_decisions_bd_10, .ci_level = 0.90) %>% mutate(
  x = str_sub(x, 1, 5),
  y = (exp(y) - 1) * 100,
  ci_low = (exp(ci_low) - 1) * 100,
  ci_high = (exp(ci_high) - 1) * 100
)) +
  geom_vline(xintercept = (as.numeric(time_decision) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Blue10"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Blue10"]) +
  scale_x_discrete(breaks = times(10, 16, 7200) %>% pull(time) %>% as.character() %>% str_sub(1, 5)) +
  scale_y_continuous_rba(limits = c(-100, 50)) +
  labs(subtitle = "10-year") +
  geom_special_gridline_rba()

g_releases_bd_10 <- ggrba(iplot_data(m_releases_bd_10, .ci_level = 0.90) %>% mutate(
  x = str_sub(x, 1, 5),
  y = (exp(y) - 1) * 100,
  ci_low = (exp(ci_low) - 1) * 100,
  ci_high = (exp(ci_high) - 1) * 100
)) +
  geom_vline(xintercept = (as.numeric(time_release) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Aqua4"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Aqua4"]) +
  scale_x_discrete(breaks = times(10, 16, 7200) %>% tail(-1) %>% pull(time) %>% as.character() %>% str_sub(1, 5)) +
  scale_y_continuous_rba(limits = c(-100, 50)) +
  labs(subtitle = "10-year") +
  geom_special_gridline_rba()

g_news_bd <- g_decisions_bd_3 + g_releases_bd_3 + g_decisions_bd_10 + g_releases_bd_10 +
  plot_layout(nrow = 2, ncol = 2) +
  multipanel_title_rba(
    title = "Best Depth for News Events",
    subtitle = "Intraday effects, 5-minute intervals, dashed line = event time"
  )

### 2.3.3. Turnover ####

g_decisions_to_3 <- ggrba(iplot_data(m_decisions_to_3, .ci_level = 0.90) %>% mutate(x = str_sub(x, 1, 5))) +
  geom_vline(xintercept = (as.numeric(time_decision) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Blue10"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Blue10"]) +
  scale_y_continuous_rba(limits = c(-3, 12), units = "’000", breaks = c(-3, 0, 3, 6, 9, 12)) +
  labs(
    title = "RBA decisions",
    subtitle = "3-year"
  ) +
  geom_special_gridline_rba()

g_releases_to_3 <- ggrba(iplot_data(m_releases_to_3, .ci_level = 0.90) %>% mutate(x = str_sub(x, 1, 5))) +
  geom_vline(xintercept = (as.numeric(time_release) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Aqua4"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Aqua4"]) +
  scale_y_continuous_rba(limits = c(-3, 12), units = "’000", breaks = c(-3, 0, 3, 6, 9, 12)) +
  labs(
    title = "ABS releases",
    subtitle = "3-year"
  ) +
  geom_special_gridline_rba()

g_decisions_to_10 <- ggrba(iplot_data(m_decisions_to_10, .ci_level = 0.90) %>% mutate(x = str_sub(x, 1, 5))) +
  geom_vline(xintercept = (as.numeric(time_decision) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Blue10"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Blue10"]) +
  scale_x_discrete(breaks = times(10, 16, 7200) %>% pull(time) %>% as.character() %>% str_sub(1, 5)) +
  scale_y_continuous_rba(limits = c(-1, 4), units = "’000") +
  labs(subtitle = "10-year") +
  geom_special_gridline_rba()

g_releases_to_10 <- ggrba(iplot_data(m_releases_to_10, .ci_level = 0.90) %>% mutate(x = str_sub(x, 1, 5))) +
  geom_vline(xintercept = (as.numeric(time_release) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Aqua4"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Aqua4"]) +
  scale_x_discrete(breaks = times(10, 16, 7200) %>% tail(-1) %>% pull(time) %>% as.character() %>% str_sub(1, 5)) +
  scale_y_continuous_rba(limits = c(-1, 4), units = "’000") +
  labs(subtitle = "10-year") +
  geom_special_gridline_rba()

g_news_to <- g_decisions_to_3 + g_releases_to_3 + g_decisions_to_10 + g_releases_to_10 +
  plot_layout(nrow = 2, ncol = 2) +
  multipanel_title_rba(
    title = "Turnover for News Events",
    subtitle = "Intraday effects, 5-minute intervals, dashed line = event time"
  )

### 2.3.4. Price impact ####

g_decisions_pi_3 <- ggrba(iplot_data(m_decisions_pi_3, .ci_level = 0.90) %>% mutate(x = str_sub(x, 1, 5))) +
  geom_vline(xintercept = (as.numeric(time_decision) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Blue10"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Blue10"]) +
  scale_y_continuous_rba(limits = c(-5, 10), units = "bps") +
  labs(
    title = "RBA decisions",
    subtitle = "3-year"
  ) +
  geom_special_gridline_rba()

g_releases_pi_3 <- ggrba(iplot_data(m_releases_pi_3, .ci_level = 0.90) %>% mutate(x = str_sub(x, 1, 5))) +
  geom_vline(xintercept = (as.numeric(time_release) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Aqua4"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Aqua4"]) +
  scale_y_continuous_rba(limits = c(-5, 10), units = "bps") +
  labs(
    title = "ABS releases",
    subtitle = "3-year"
  ) +
  geom_special_gridline_rba()

g_decisions_pi_10 <- ggrba(iplot_data(m_decisions_pi_10, .ci_level = 0.90) %>% mutate(x = str_sub(x, 1, 5))) +
  geom_vline(xintercept = (as.numeric(time_decision) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Blue10"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Blue10"]) +
  scale_x_discrete(breaks = times(10, 16, 7200) %>% pull(time) %>% as.character() %>% str_sub(1, 5)) +
  scale_y_continuous_rba(limits = c(-5, 10), units = "bps") +
  labs(subtitle = "10-year") +
  geom_special_gridline_rba()

g_releases_pi_10 <- ggrba(iplot_data(m_releases_pi_10, .ci_level = 0.90) %>% mutate(x = str_sub(x, 1, 5))) +
  geom_vline(xintercept = (as.numeric(time_release) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Aqua4"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Aqua4"]) +
  scale_x_discrete(breaks = times(10, 16, 7200) %>% tail(-1) %>% pull(time) %>% as.character() %>% str_sub(1, 5)) +
  scale_y_continuous_rba(limits = c(-5, 10), units = "bps") +
  labs(subtitle = "10-year") +
  geom_special_gridline_rba()

g_news_pi <- g_decisions_pi_3 + g_releases_pi_3 + g_decisions_pi_10 + g_releases_pi_10 +
  plot_layout(nrow = 2, ncol = 2) +
  multipanel_title_rba(
    title = "Price Impact for News Events",
    subtitle = "Intraday effects, 5-minute intervals, dashed line = event time"
  )

## 2.4. Flow events ####

### 2.4.1. Turnover ####

g_purchases_to_3 <- ggrba(iplot_data(m_purchases_to_3, .ci_level = 0.90) %>% mutate(x = str_sub(x, 1, 5))) +
  geom_vline(xintercept = (as.numeric(time_purchase) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Blue10"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Blue10"]) +
  scale_y_continuous_rba(limits = c(-10, 30)) +
  labs(
    title = "RBA purchases",
    subtitle = "3-year"
  ) +
  geom_special_gridline_rba()

g_tenders_to_3 <- ggrba(iplot_data(m_tenders_to_3, .ci_level = 0.90) %>% mutate(x = str_sub(x, 1, 5))) +
  geom_vline(xintercept = (as.numeric(time_tender) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Pink6"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Pink6"]) + 
  scale_y_continuous_rba(limits = c(-10, 30)) +
  labs(
    title = "AOFM tenders",
    subtitle = "3-year"
  ) +
  geom_special_gridline_rba()

g_purchases_to_10 <- ggrba(iplot_data(m_purchases_to_10, .ci_level = 0.90) %>% mutate(x = str_sub(x, 1, 5))) +
  geom_vline(xintercept = (as.numeric(time_purchase) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Blue10"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Blue10"]) +
  scale_x_discrete(breaks = times(10, 16, 7200) %>% pull(time) %>% as.character() %>% str_sub(1, 5)) +
  scale_y_continuous_rba(limits = c(-10, 30)) +
  labs(subtitle = "10-year") +
  geom_special_gridline_rba()

g_tenders_to_10 <- ggrba(iplot_data(m_tenders_to_10, .ci_level = 0.90) %>% mutate(x = str_sub(x, 1, 5))) +
  geom_vline(xintercept = (as.numeric(time_tender) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Pink6"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Pink6"]) +
  scale_x_discrete(breaks = times(10, 16, 7200) %>% tail(-1) %>% pull(time) %>% as.character() %>% str_sub(1, 5)) +
  scale_y_continuous_rba(limits = c(-10, 30)) +
  labs(subtitle = "10-year") +
  geom_special_gridline_rba()

g_flow_to <- g_purchases_to_3 + g_tenders_to_3 + g_purchases_to_10 + g_tenders_to_10 +
  plot_layout(nrow = 2, ncol = 2) +
  multipanel_title_rba(
    title = "Turnover for Flow Events",
    subtitle = "Intraday effects, 5-minute intervals, dashed line = event time"
  )

### 2.4.2. Best depth ####

g_purchases_bd_3 <- ggrba(iplot_data(m_purchases_bd_3, .ci_level = 0.90) %>% mutate(
  x = str_sub(x, 1, 5),
  y = (exp(y) - 1) * 100,
  ci_low = (exp(ci_low) - 1) * 100,
  ci_high = (exp(ci_high) - 1) * 100
)) +
  geom_vline(xintercept = (as.numeric(time_purchase) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Blue10"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Blue10"]) +
  scale_y_continuous_rba(limits = c(-20, 40), breaks = c(-20, 0, 20, 40)) +
  labs(
    title = "RBA purchases",
    subtitle = "3-year"
  ) +
  geom_special_gridline_rba()

g_tenders_bd_3 <- ggrba(iplot_data(m_tenders_bd_3, .ci_level = 0.90) %>% mutate(
  x = str_sub(x, 1, 5),
  y = (exp(y) - 1) * 100,
  ci_low = (exp(ci_low) - 1) * 100,
  ci_high = (exp(ci_high) - 1) * 100
)) +
  geom_vline(xintercept = (as.numeric(time_tender) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Pink6"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Pink6"]) +
  scale_y_continuous_rba(limits = c(-20, 40), breaks = c(-20, 0, 20, 40)) +
  labs(
    title = "AOFM tenders",
    subtitle = "3-year"
  ) +
  geom_special_gridline_rba()

g_purchases_bd_10 <- ggrba(iplot_data(m_purchases_bd_10, .ci_level = 0.90) %>% mutate(
  x = str_sub(x, 1, 5),
  y = (exp(y) - 1) * 100,
  ci_low = (exp(ci_low) - 1) * 100,
  ci_high = (exp(ci_high) - 1) * 100
)) +
  geom_vline(xintercept = (as.numeric(time_purchase) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Blue10"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Blue10"]) +
  scale_x_discrete(breaks = times(10, 16, 7200) %>% pull(time) %>% as.character() %>% str_sub(1, 5)) +
  scale_y_continuous_rba(limits = c(-20, 40), breaks = c(-20, 0, 20, 40)) +
  labs(subtitle = "10-year") +
  geom_special_gridline_rba()

g_tenders_bd_10 <- ggrba(iplot_data(m_tenders_bd_10, .ci_level = 0.90) %>% mutate(
  x = str_sub(x, 1, 5),
  y = (exp(y) - 1) * 100,
  ci_low = (exp(ci_low) - 1) * 100,
  ci_high = (exp(ci_high) - 1) * 100
)) +
  geom_vline(xintercept = (as.numeric(time_tender) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Pink6"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Pink6"]) +
  scale_x_discrete(breaks = times(10, 16, 7200) %>% tail(-1) %>% pull(time) %>% as.character() %>% str_sub(1, 5)) +
  scale_y_continuous_rba(limits = c(-20, 40), breaks = c(-20, 0, 20, 40)) +
  labs(subtitle = "10-year") +
  geom_special_gridline_rba()

g_flow_bd <- g_purchases_bd_3 + g_tenders_bd_3 + g_purchases_bd_10 + g_tenders_bd_10 +
  plot_layout(nrow = 2, ncol = 2) +
  multipanel_title_rba(
    title = "Best Depth for Flow Events",
    subtitle = "Intraday effects, 5-minute intervals, dashed line = event time"
  )

### 2.4.3. Price impact ####

g_purchases_pi_3 <- ggrba(iplot_data(m_purchases_pi_3, .ci_level = 0.90) %>% mutate(x = str_sub(x, 1, 5))) +
  geom_vline(xintercept = (as.numeric(time_purchase) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Blue10"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Blue10"]) +
  scale_y_continuous_rba(limits = c(-2, 2), units = "bps") +
  labs(
    title = "RBA purchases",
    subtitle = "3-year"
  ) +
  geom_special_gridline_rba()

g_tenders_pi_3 <- ggrba(iplot_data(m_tenders_pi_3, .ci_level = 0.90) %>% mutate(x = str_sub(x, 1, 5))) +
  geom_vline(xintercept = (as.numeric(time_tender) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Pink6"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Pink6"]) +
  scale_y_continuous_rba(limits = c(-2, 2), units = "bps") +
  labs(
    title = "AOFM tenders",
    subtitle = "3-year"
  ) +
  geom_special_gridline_rba()

g_purchases_pi_10 <- ggrba(iplot_data(m_purchases_pi_10, .ci_level = 0.90) %>% mutate(x = str_sub(x, 1, 5))) +
  geom_vline(xintercept = (as.numeric(time_purchase) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Blue10"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Blue10"]) +
  scale_x_discrete(breaks = times(10, 16, 7200) %>% pull(time) %>% as.character() %>% str_sub(1, 5)) +
  scale_y_continuous_rba(limits = c(-4, 4), units = "bps") +
  labs(subtitle = "10-year") +
  geom_special_gridline_rba()

g_tenders_pi_10 <- ggrba(iplot_data(m_tenders_pi_10, .ci_level = 0.90) %>% mutate(x = str_sub(x, 1, 5))) +
  geom_vline(xintercept = (as.numeric(time_tender) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Pink6"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Pink6"]) +
  scale_x_discrete(breaks = times(10, 16, 7200) %>% tail(-1) %>% pull(time) %>% as.character() %>% str_sub(1, 5)) +
  scale_y_continuous_rba(limits = c(-4, 4), units = "bps") +
  labs(subtitle = "10-year") +
  geom_special_gridline_rba()

g_flow_pi <- g_purchases_pi_3 + g_tenders_pi_3 + g_purchases_pi_10 + g_tenders_pi_10 +
  plot_layout(nrow = 2, ncol = 2) +
  multipanel_title_rba(
    title = "Price Impact for Flow Events",
    subtitle = "Intraday effects, 5-minute intervals, dashed line = event time"
  )
g_flow_pi
## 2.5. Syndication events ####
# Change geom_vline() from RBA["Default6"] to longdashed line in black for all these 

### 2.5.1. Periods ####

g_syndications_bd_3 <- ggrba(coefplot_data(m_syndications_bd_3, .ci_level = 0.90) %>% mutate(
  x = as.numeric(str_remove(x, "factor\\(day\\)")),
  y = (exp(y) - 1) * 100,
  ci_low = (exp(ci_low) - 1) * 100,
  ci_high = (exp(ci_high) - 1) * 100
)) +
  shading_rba(xmin = -3.5, xmax = -1.5) +
  geom_vline(xintercept = 0, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Aqua6"], size = 5) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Aqua6"]) +
  scale_x_continuous_rba(limits = c(-6.5, 4.5)) +
  scale_y_continuous_rba(limits = c(-20, 60)) +
  labs(
    title = "3-year",
    subtitle = "Best depth"
  ) +
  geom_special_gridline_rba()

g_syndications_bd_10 <- ggrba(coefplot_data(m_syndications_bd_10, .ci_level = 0.90) %>% mutate(
  x = as.numeric(str_remove(x, "factor\\(day\\)")),
  y = (exp(y) - 1) * 100,
  ci_low = (exp(ci_low) - 1) * 100,
  ci_high = (exp(ci_high) - 1) * 100
)) +
  shading_rba(xmin = -3.5, xmax = -1.5) +
  geom_vline(xintercept = 0, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Olive2"], size = 5) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Olive2"]) +
  scale_x_continuous_rba(limits = c(-6.5, 4.5)) +
  scale_y_continuous_rba(limits = c(-10, 30)) +
  labs(
    title = "10-year",
    subtitle = "Best depth"
  ) +
  geom_special_gridline_rba()

g_syndications_to_3 <- ggrba(coefplot_data(m_syndications_to_3, .ci_level = 0.90) %>% mutate(
  x = as.numeric(str_remove(x, "factor\\(day\\)")),
  ci_low = if_else(ci_low < -100, -100, ci_low)
)) +
  shading_rba(xmin = -3.5, xmax = -1.5) +
  geom_vline(xintercept = 0, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Aqua6"], size = 5) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Aqua6"]) +
  scale_x_continuous_rba(limits = c(-6.5, 4.5)) +
  scale_y_continuous_rba(limits = c(-100, 300)) +
  labs(subtitle = "Turnover") +
  geom_special_gridline_rba()

g_syndications_to_10 <- ggrba(coefplot_data(m_syndications_to_10, .ci_level = 0.90) %>% mutate(x = as.numeric(str_remove(x, "factor\\(day\\)")))) +
  shading_rba(xmin = -3.5, xmax = -1.5) +
  geom_vline(xintercept = 0, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Olive2"], size = 5) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Olive2"]) +
  scale_x_continuous_rba(limits = c(-6.5, 4.5)) +
  scale_y_continuous_rba(limits = c(-100, 300)) +
  labs(subtitle = "Turnover") +
  geom_special_gridline_rba()

g_syndications_pi_3 <- ggrba(coefplot_data(m_syndications_pi_3, .ci_level = 0.90) %>% mutate(x = as.numeric(str_remove(x, "factor\\(day\\)")))) +
  shading_rba(xmin = -3.5, xmax = -1.5) +
  geom_vline(xintercept = 0, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Aqua6"], size = 5) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Aqua6"]) +
  scale_x_continuous_rba(limits = c(-6.5, 4.5), units = "") +
  scale_y_continuous_rba(limits = c(-1, 1), units = "bps") +
  labs(subtitle = "Price impact") +
  geom_special_gridline_rba()

g_syndications_pi_10 <- ggrba(coefplot_data(m_syndications_pi_10, .ci_level = 0.90) %>% mutate(x = as.numeric(str_remove(x, "factor\\(day\\)")))) +
  shading_rba(xmin = -3.5, xmax = -1.5) +
  geom_vline(xintercept = 0, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Olive2"], size = 5) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Olive2"]) +
  scale_x_continuous_rba(limits = c(-6.5, 4.5), units = "", keep_first_label = "keep") +
  scale_y_continuous_rba(limits = c(-1, 1), units = "bps") +
  labs(subtitle = "Price impact") +
  geom_special_gridline_rba()

g_syndications <- g_syndications_bd_3 + g_syndications_bd_10 + g_syndications_to_3 + g_syndications_to_10 + g_syndications_pi_3 + g_syndications_pi_10 +
  plot_layout(nrow = 3, ncol = 2) +
  plot_size_rba(plot.height = grid::unit(210, "mm")) +
  multipanel_title_rba(
    title = "Syndication Period Liquidity",
    subtitle = "Multiday effects, dashed = pricings, shading = announcements"
  )

### 2.5.2. Announcements ####

g_announcements_bd_3 <- ggrba(iplot_data(m_announcements_bd_3, .ci_level = 0.90) %>% mutate(
  x = str_sub(x, 1, 5),
  y = (exp(y) - 1) * 100,
  ci_low = (exp(ci_low) - 1) * 100,
  ci_high = (exp(ci_high) - 1) * 100
)) +
  geom_vline(xintercept = (as.numeric(time_announcement) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Aqua6"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Aqua6"]) +
  scale_y_continuous_rba(limits = c(-50, 150)) +
  labs(
    title = "3-year",
    subtitle = "Best depth"
  ) +
  geom_special_gridline_rba()

g_announcements_bd_10 <- ggrba(iplot_data(m_announcements_bd_10, .ci_level = 0.90) %>% mutate(
  x = str_sub(x, 1, 5),
  y = (exp(y) - 1) * 100,
  ci_low = (exp(ci_low) - 1) * 100,
  ci_high = (exp(ci_high) - 1) * 100
)) +
  geom_vline(xintercept = (as.numeric(time_announcement) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Olive2"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Olive2"]) +
  scale_y_continuous_rba(limits = c(-50, 150)) +
  labs(
    title = "10-year",
    subtitle = "Best depth"
  ) +
  geom_special_gridline_rba()

g_announcements_to_3 <- ggrba(iplot_data(m_announcements_to_3, .ci_level = 0.90) %>% mutate(x = str_sub(x, 1, 5))) +
  geom_vline(xintercept = (as.numeric(time_announcement) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Aqua6"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Aqua6"]) +
  scale_x_discrete(breaks = times(10, 16, 7200) %>% pull(time) %>% as.character() %>% str_sub(1, 5)) +
  scale_y_continuous_rba(limits = c(-5, 15), units = "’000") +
  labs(subtitle = "Turnover") +
  geom_special_gridline_rba()

g_announcements_to_10 <- ggrba(iplot_data(m_announcements_to_10, .ci_level = 0.90) %>% mutate(x = str_sub(x, 1, 5))) +
  geom_vline(xintercept = (as.numeric(time_announcement) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Olive2"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Olive2"]) +
  scale_x_discrete(breaks = times(10, 16, 7200) %>% tail(-1) %>% pull(time) %>% as.character() %>% str_sub(1, 5)) +
  scale_y_continuous_rba(limits = c(-5, 15), units = "’000") +
  labs(subtitle = "Turnover") +
  geom_special_gridline_rba()

g_announcements_pi_3 <- ggrba(iplot_data(m_announcements_pi_3, .ci_level = 0.90) %>% mutate(x = str_sub(x, 1, 5))) +
  geom_vline(xintercept = (as.numeric(time_announcement) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Aqua6"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Aqua6"]) +
  scale_x_discrete(breaks = times(10, 16, 7200) %>% pull(time) %>% as.character() %>% str_sub(1, 5)) +
  scale_y_continuous_rba(limits = c(-10, 10), units = "bps") +
  labs(subtitle = "Price impact") +
  geom_special_gridline_rba()

g_announcements_pi_10 <- ggrba(iplot_data(m_announcements_pi_10, .ci_level = 0.90) %>% mutate(x = str_sub(x, 1, 5))) +
  geom_vline(xintercept = (as.numeric(time_announcement) - as.numeric(time_min)) * 12 / 60 / 60 + 1, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Olive2"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Olive2"]) +
  scale_x_discrete(breaks = times(10, 16, 7200) %>% tail(-1) %>% pull(time) %>% as.character() %>% str_sub(1, 5)) +
  scale_y_continuous_rba(limits = c(-10, 10), units = "bps") +
  labs(subtitle = "Price impact") +
  geom_special_gridline_rba()

g_announcements <- g_announcements_bd_3 + g_announcements_bd_10 + g_announcements_to_3 + g_announcements_to_10 + g_announcements_pi_3 + g_announcements_pi_10 +
  plot_layout(nrow = 3, ncol = 2) +
  plot_size_rba(plot.height = grid::unit(210, "mm")) +
  multipanel_title_rba(
    title = "Syndication Announcement Liquidity",
    subtitle = "Intraday effects, 5-minute intervals, dashed line = event time"
  )


### 2.5.3. Pricings ####

#### 2.5.3.1. Best depth ####
# Adding breaks to scale_y_continuous_rba() to avoid splitting the "per cent" over a gridline

g_pricings_bd_3_treatment <- ggrba(iplot_data(m_pricings_bd_3_treatment, .ci_level = 0.90) %>% mutate(
  x = (as.numeric(x) - 1) / 12 - 3.75,
  y = (exp(y) - 1) * 100,
  ci_low = (exp(ci_low) - 1) * 100,
  ci_high = (exp(ci_high) - 1) * 100
)) +
  geom_vline(xintercept = 0, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Aqua6"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Aqua6"]) +
  scale_y_continuous_rba(limits = c(-100, 200), breaks = c(-100, 0, 100, 200)) +
  scale_x_continuous_rba(limits = c(-3.85, 1.35), units = "") +
  labs(
    title = "3-year",
    subtitle = "Absolute"
  ) +
  geom_special_gridline_rba()

g_pricings_bd_10_treatment <- ggrba(iplot_data(m_pricings_bd_10_treatment, .ci_level = 0.90) %>% mutate(
  x = (as.numeric(x) - 1) / 12 - 3.75,
  y = (exp(y) - 1) * 100,
  ci_low = (exp(ci_low) - 1) * 100,
  ci_high = (exp(ci_high) - 1) * 100
)) +
  geom_vline(xintercept = 0, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Olive2"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Olive2"]) +
  scale_y_continuous_rba(limits = c(-100, 200), breaks = c(-100, 0, 100, 200)) +
  scale_x_continuous_rba(limits = c(-3.85, 1.35), units = "") +
  labs(
    title = "10-year",
    subtitle = "Absolute"
  ) +
  geom_special_gridline_rba()

g_pricings_bd_3 <- ggrba(iplot_data(m_pricings_bd_3, .ci_level = 0.90) %>% mutate(
  x = (as.numeric(x) - 1) / 12 - 3.75,
  y = (exp(y) - 1) * 100,
  ci_low = (exp(ci_low) - 1) * 100,
  ci_high = (exp(ci_high) - 1) * 100
)) +
  geom_vline(xintercept = 0, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Aqua6"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Aqua6"]) +
  scale_y_continuous_rba(limits = c(-20, 40), breaks = c(-20, 0, 20, 40)) +
  scale_x_continuous_rba(limits = c(-3.85, 1.35), units = "") +
  labs(subtitle = "Relative (per $b adj flow)") +
  geom_special_gridline_rba()

g_pricings_bd_10 <- ggrba(iplot_data(m_pricings_bd_10, .ci_level = 0.90) %>% mutate(
  x = (as.numeric(x) - 1) / 12 - 3.75,
  y = (exp(y) - 1) * 100,
  ci_low = (exp(ci_low) - 1) * 100,
  ci_high = (exp(ci_high) - 1) * 100
)) +
  geom_vline(xintercept = 0, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Olive2"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Olive2"]) +
  scale_y_continuous_rba(limits = c(-20, 40), breaks = c(-20, 0, 20, 40)) +
  scale_x_continuous_rba(limits = c(-3.85, 1.35), units = "", keep_first_label = "keep") +
  labs(subtitle = "Relative (per $b adj flow)") +
  geom_special_gridline_rba()

g_pricings_bd <- g_pricings_bd_3_treatment + g_pricings_bd_10_treatment + g_pricings_bd_3 + g_pricings_bd_10 +
  plot_layout(nrow = 2, ncol = 2) +
  multipanel_title_rba(
    title = "Best Depth for Syndication Pricings",
    subtitle = "Intraday effects, 5-minute intervals, dashed line = event time"
  )

#### 2.5.3.2. Turnover ####

g_pricings_to_3_treatment <- ggrba(iplot_data(m_pricings_to_3_treatment, .ci_level = 0.90) %>% mutate(x = (as.numeric(x) - 1) / 12 - 3.75)) +
  geom_vline(xintercept = 0, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Aqua6"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Aqua6"]) +
  scale_y_continuous_rba(limits = c(-5, 20), units = "’000") +
  scale_x_continuous_rba(limits = c(-3.85, 1.35), units = "") +
  labs(
    title = "3-year",
    subtitle = "Absolute (number of contracts)"
  ) +
  geom_special_gridline_rba()

g_pricings_to_10_treatment <- ggrba(iplot_data(m_pricings_to_10_treatment, .ci_level = 0.90) %>% mutate(x = (as.numeric(x) - 1) / 12 - 3.75)) +
  geom_vline(xintercept = 0, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Olive2"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Olive2"]) +
  scale_y_continuous_rba(limits = c(-5, 20), units = "’000") +
  scale_x_continuous_rba(limits = c(-3.85, 1.35), units = "") +
  labs(
    title = "10-year",
    subtitle = "Absolute (number of contracts)"
  ) +
  geom_special_gridline_rba()

g_pricings_to_3 <- ggrba(iplot_data(m_pricings_to_3, .ci_level = 0.90) %>% mutate(x = (as.numeric(x) - 1) / 12 - 3.75)) +
  geom_vline(xintercept = 0, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Aqua6"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Aqua6"]) +
  scale_y_continuous_rba(limits = c(-10, 40)) +
  scale_x_continuous_rba(limits = c(-3.85, 1.35), units = "") +
  labs(subtitle = "Relative (share of adj flow)") +
  geom_special_gridline_rba()

g_pricings_to_10 <- ggrba(iplot_data(m_pricings_to_10, .ci_level = 0.90) %>% mutate(x = (as.numeric(x) - 1) / 12 - 3.75)) +
  geom_vline(xintercept = 0, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Olive2"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Olive2"]) +
  scale_y_continuous_rba(limits = c(-10, 40)) +
  scale_x_continuous_rba(limits = c(-3.85, 1.35), units = "", keep_first_label = "keep") +
  labs(subtitle = "Relative (share of adj flow)") +
  geom_special_gridline_rba()

g_pricings_to <- g_pricings_to_3_treatment + g_pricings_to_10_treatment + g_pricings_to_3 + g_pricings_to_10 +
  plot_layout(nrow = 2, ncol = 2) +
  multipanel_title_rba(
    title = "Turnover for Syndication Pricings",
    subtitle = "Intraday effects, 5-minute intervals, dashed line = event time"
  )

#### 2.5.3.3. Price impact ####

g_pricings_pi_3_treatment <- ggrba(iplot_data(m_pricings_pi_3_treatment, .ci_level = 0.90) %>% mutate(x = (as.numeric(x) - 1) / 12 - 3.75)) +
  geom_vline(xintercept = 0, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Aqua6"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Aqua6"]) +
  scale_y_continuous_rba(limits = c(-2, 4), breaks = c(-2, 0, 2, 4), units = "bps") +
  scale_x_continuous_rba(limits = c(-3.85, 1.35), units = "") +
  labs(
    title = "3-year",
    subtitle = "Absolute (per $b net flow)"
  ) +
  geom_special_gridline_rba()

g_pricings_pi_10_treatment <- ggrba(iplot_data(m_pricings_pi_10_treatment, .ci_level = 0.90) %>% mutate(x = (as.numeric(x) - 1) / 12 - 3.75)) +
  geom_vline(xintercept = 0, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Olive2"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Olive2"]) +
  scale_y_continuous_rba(limits = c(-2, 4), breaks = c(-2, 0, 2, 4), units = "bps") +
  scale_x_continuous_rba(limits = c(-3.85, 1.35), units = "") +
  labs(
    title = "10-year",
    subtitle = "Absolute (per $b net flow)"
  ) +
  geom_special_gridline_rba()

g_pricings_pi_3 <- ggrba(iplot_data(m_pricings_pi_3, .ci_level = 0.90) %>% mutate(x = (as.numeric(x) - 1) / 12 - 3.75)) +
  geom_vline(xintercept = 0, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Aqua6"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Aqua6"]) +
  scale_y_continuous_rba(limits = c(-0.2, 0.4), breaks = c(-0.2, 0, 0.2, 0.4),  units = "bps") +
  scale_x_continuous_rba(limits = c(-3.85, 1.35), units = "") +
  labs(subtitle = "Relative (per $b adj flow)") +
  geom_special_gridline_rba()

g_pricings_pi_10 <- ggrba(iplot_data(m_pricings_pi_10, .ci_level = 0.90) %>% mutate(x = (as.numeric(x) - 1) / 12 - 3.75)) +
  geom_vline(xintercept = 0, colour = RBA["Grey10"], linetype = "longdash") +
  geom_point(aes(x = x, y = y), colour = RBA["Olive2"]) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high), colour = RBA["Olive2"]) +
  scale_y_continuous_rba(limits = c(-0.2, 0.4), breaks = c(-0.2, 0, 0.2, 0.4), units = "bps") +
  scale_x_continuous_rba(limits = c(-3.85, 1.35), units = "", keep_first_label = "keep") +
  labs(subtitle = "Relative (per $b adj flow)") +
  geom_special_gridline_rba()

g_pricings_pi <- g_pricings_pi_3_treatment + g_pricings_pi_10_treatment + g_pricings_pi_3 + g_pricings_pi_10 +
  plot_layout(nrow = 2, ncol = 2) +
  multipanel_title_rba(
    title = "Price Impact for Syndication Pricings",
    subtitle = "Intraday effects, 5-minute intervals, dashed line = event time"
  )

## 2.6. Increment events ####

### 2.6.1. 3-year rolls ####

#### 2.6.1.1. Graph data ####

gd_rolls_bo_3 <- bind_rows(
  coefplot_data(m_rolls_bo_3_wide, .ci_level = 0.80) %>% mutate(type = "Pre-narrow"),
  coefplot_data(m_rolls_bo_3_narrow, .ci_level = 0.80) %>% mutate(type = "Post-narrow"),
  coefplot_data(m_rolls_bo_3_widening, .ci_level = 0.80) %>% mutate(type = "Post-widen"),
  coefplot_data(m_rolls_bo_3_delinked, .ci_level = 0.80) %>% mutate(type = "Post-delink")
) %>%
  mutate(
    x = as.numeric(str_remove(x, "factor\\(day\\)")),
    type = factor(type, levels = c("Pre-narrow", "Post-narrow", "Post-widen", "Post-delink"))
  )

gd_rolls_bd_3 <- bind_rows(
  coefplot_data(m_rolls_bd_3_wide, .ci_level = 0.80) %>% mutate(type = "Pre-narrow"),
  coefplot_data(m_rolls_bd_3_narrow, .ci_level = 0.80) %>% mutate(type = "Post-narrow"),
  coefplot_data(m_rolls_bd_3_widening, .ci_level = 0.80) %>% mutate(type = "Post-widen"),
  coefplot_data(m_rolls_bd_3_delinked, .ci_level = 0.80) %>% mutate(type = "Post-delink")
) %>%
  mutate(
    x = as.numeric(str_remove(x, "factor\\(day\\)")),
    type = factor(type, levels = c("Pre-narrow", "Post-narrow", "Post-widen", "Post-delink")),
    y = (exp(y) - 1) * 100,
    ci_low = (exp(ci_low) - 1) * 100,
    ci_high = (exp(ci_high) - 1) * 100,
    ci_high = if_else(ci_high > 50, 50, ci_high)
  )

#### 2.6.1.2. Graph ####
# replace this to move from coloured to dashed liines 
# geom_vline(xintercept = 0.5, colour = RBA["Grey2"]) +

g_rolls_bo_3_wide <- ggrba(gd_rolls_bo_3 %>% filter(type == "Pre-narrow")) +
  geom_vline(xintercept = -4.5, colour = RBA["Grey10"], linetype = "longdash") +
  geom_vline(xintercept = 0.5, colour = RBA["Grey10"], linetype = "dotted") +
  geom_point(aes(x = x, y = y, colour = type), size = 3) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high, colour = type)) +
  scale_colour_manual(values = colours) +
  scale_x_continuous_rba(limits = c(-8.5, 2.5), breaks = c(-8, -6, -4, -2, 0, 2), units = "") +
  scale_y_continuous_rba(limits = c(-1, 0.5), units = "bps") +
  labs(title = "Bid-ask spreads") +
  annotate("text", x = -2, y = -0.25, label = "Pre-narrow", size = round(20 / ggplot2::.pt), colour = RBA[["Olive1"]]) +
  geom_special_gridline_rba()

g_rolls_bo_3_narrow <- ggrba(gd_rolls_bo_3 %>% filter(type == "Post-narrow")) +
  geom_vline(xintercept = -4.5, colour = RBA["Grey10"], linetype = "longdash") +
  geom_vline(xintercept = 0.5, colour = RBA["Grey10"], linetype = "dotted") +
  geom_point(aes(x = x, y = y, colour = type), size = 3) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high, colour = type)) +
  scale_colour_manual(values = colours) +
  scale_x_continuous_rba(limits = c(-8.5, 2.5), breaks = c(-8, -6, -4, -2, 0, 2), units = "") +
  scale_y_continuous_rba(limits = c(-1, 0.5), units = "bps") +
  annotate("text", x = -2, y = -0.75, label = "Post-narrow", size = round(20 / ggplot2::.pt), colour = RBA[["Olive4"]]) +
  geom_special_gridline_rba()

g_rolls_bo_3_widening <- ggrba(gd_rolls_bo_3 %>% filter(type == "Post-widen")) +
  geom_vline(xintercept = -4.5, colour = RBA["Grey10"], linetype = "longdash") +
  geom_vline(xintercept = 0.5, colour = RBA["Grey10"], linetype = "dotted") +
  geom_point(aes(x = x, y = y, colour = type), size = 3) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high, colour = type)) +
  scale_colour_manual(values = colours) +
  scale_x_continuous_rba(limits = c(-8.5, 2.5), breaks = c(-8, -6, -4, -2, 0, 2), units = "") +
  scale_y_continuous_rba(limits = c(-1, 0.5), units = "bps") +
  annotate("text", x = -2, y = -0.25, label = "Post-widen", size = round(20 / ggplot2::.pt), colour = RBA[["Olive7"]]) +
  geom_special_gridline_rba()

g_rolls_bo_3_delinked <- ggrba(gd_rolls_bo_3 %>% filter(type == "Post-delink")) +
  geom_vline(xintercept = -4.5, colour = RBA["Grey10"], linetype = "longdash") +
  geom_vline(xintercept = 0.5, colour = RBA["Grey10"], linetype = "dotted") +
  geom_point(aes(x = x, y = y, colour = type), size = 3) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high, colour = type)) +
  scale_colour_manual(values = colours) +
  scale_x_continuous_rba(limits = c(-8.5, 2.5), breaks = c(-8, -6, -4, -2, 0, 2), units = "") +
  scale_y_continuous_rba(limits = c(-1, 0.5), units = "bps") +
  annotate("text", x = -2, y = -0.25, label = "Post-delink", size = round(20 / ggplot2::.pt), colour = RBA[["Olive10"]]) +
  geom_special_gridline_rba()

g_rolls_bd_3_wide <- ggrba(gd_rolls_bd_3 %>% filter(type == "Pre-narrow")) +
  geom_vline(xintercept = -4.5, colour = RBA["Grey10"], linetype = "longdash") +
  geom_vline(xintercept = 0.5, colour = RBA["Grey10"], linetype = "dotted") +
  geom_point(aes(x = x, y = y, colour = type), size = 3) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high, colour = type)) +
  scale_colour_manual(values = colours) +
  scale_x_continuous_rba(limits = c(-8.5, 2.5), breaks = c(-8, -6, -4, -2, 0, 2), units = "") +
  scale_y_continuous_rba(limits = c(-100, 50)) +
  labs(title = "Best depth") +
  geom_special_gridline_rba()

g_rolls_bd_3_narrow <- ggrba(gd_rolls_bd_3 %>% filter(type == "Post-narrow")) +
  geom_vline(xintercept = -4.5, colour = RBA["Grey10"], linetype = "longdash") +
  geom_vline(xintercept = 0.5, colour = RBA["Grey10"], linetype = "dotted") +
  geom_point(aes(x = x, y = y, colour = type), size = 3) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high, colour = type)) +
  scale_colour_manual(values = colours) +
  scale_x_continuous_rba(limits = c(-8.5, 2.5), breaks = c(-8, -6, -4, -2, 0, 2), units = "") +
  scale_y_continuous_rba(limits = c(-100, 50)) +
  geom_special_gridline_rba()

g_rolls_bd_3_widening <- ggrba(gd_rolls_bd_3 %>% filter(type == "Post-widen")) +
  geom_vline(xintercept = -4.5, colour = RBA["Grey10"], linetype = "longdash") +
  geom_vline(xintercept = 0.5, colour = RBA["Grey10"], linetype = "dotted") +
  geom_point(aes(x = x, y = y, colour = type), size = 3) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high, colour = type)) +
  scale_colour_manual(values = colours) +
  scale_x_continuous_rba(limits = c(-8.5, 2.5), breaks = c(-8, -6, -4, -2, 0, 2), units = "") +
  scale_y_continuous_rba(limits = c(-100, 50)) +
  geom_special_gridline_rba()

g_rolls_bd_3_delinked <- ggrba(gd_rolls_bd_3 %>% filter(type == "Post-delink")) +
  geom_vline(xintercept = -4.5, colour = RBA["Grey10"], linetype = "longdash") +
  geom_vline(xintercept = 0.5, colour = RBA["Grey10"], linetype = "dotted") +
  geom_point(aes(x = x, y = y, colour = type), size = 3) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high, colour = type)) +
  scale_colour_manual(values = colours) +
  scale_x_continuous_rba(limits = c(-8.5, 2.5), breaks = c(-8, -6, -4, -2, 0, 2), units = "", keep_first_label = "keep") +
  scale_y_continuous_rba(limits = c(-100, 50)) +
  geom_special_gridline_rba()

g_rolls_3 <- g_rolls_bo_3_wide + g_rolls_bd_3_wide +
  g_rolls_bo_3_narrow + g_rolls_bd_3_narrow +
  g_rolls_bo_3_widening + g_rolls_bd_3_widening +
  g_rolls_bo_3_delinked + g_rolls_bd_3_delinked +
  plot_layout(nrow = 4, ncol = 2) +
  plot_size_rba(plot.height = grid::unit(280, "mm")) +
  multipanel_title_rba(
    title = "3-year Futures Roll Period Liquidity",
    subtitle = "Multiday effects, dashed = start of roll, dotted = end of roll"
  )

### 2.6.2. 10-year rolls ####

#### 2.6.2.1. Graph data ####

gd_rolls_bo_10 <- bind_rows(
  coefplot_data(m_rolls_bo_10_wide, .ci_level = 0.80) %>% mutate(type = "Pre-narrow"),
  coefplot_data(m_rolls_bo_10_narrow, .ci_level = 0.80) %>% mutate(type = "Post-narrow"),
  coefplot_data(m_rolls_bo_10_delinked, .ci_level = 0.80) %>% mutate(type = "Post-delink")
) %>%
  mutate(
    x = as.numeric(str_remove(x, "factor\\(day\\)")),
    type = factor(type, levels = c("Pre-narrow", "Post-narrow", "Post-delink"))
  )

gd_rolls_bd_10 <- bind_rows(
  coefplot_data(m_rolls_bd_10_wide, .ci_level = 0.80) %>% mutate(type = "Pre-narrow"),
  coefplot_data(m_rolls_bd_10_narrow, .ci_level = 0.80) %>% mutate(type = "Post-narrow"),
  coefplot_data(m_rolls_bd_10_delinked, .ci_level = 0.80) %>% mutate(type = "Post-delink")
) %>%
  mutate(
    x = as.numeric(str_remove(x, "factor\\(day\\)")),
    type = factor(type, levels = c("Pre-narrow", "Post-narrow", "Post-delink")),
    y = (exp(y) - 1) * 100,
    ci_low = (exp(ci_low) - 1) * 100,
    ci_high = (exp(ci_high) - 1) * 100,
    ci_high = if_else(ci_high > 50, 50, ci_high)
  )

#### 2.6.2.2. Graph ####

g_rolls_bo_10_wide <- ggrba(gd_rolls_bo_10 %>% filter(type == "Pre-narrow")) +
  geom_vline(xintercept = -4.5, colour = RBA["Grey10"], linetype = "longdash") +
  geom_vline(xintercept = 0.5, colour = RBA["Grey10"], linetype = "dotted") +
  geom_point(aes(x = x, y = y, colour = type), size = 3) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high, colour = type)) +
  scale_colour_manual(values = colours) +
  scale_x_continuous_rba(limits = c(-8.5, 2.5), breaks = c(-8, -6, -4, -2, 0, 2), units = "") +
  scale_y_continuous_rba(limits = c(-0.5, 0.25), breaks = c(-0.5, -0.25, 0, 0.25), units = "bps") +
  labs(title = "Bid-ask spreads") +
  annotate("text", x = -2, y = -0.125, label = "Pre-narrow", size = round(20 / ggplot2::.pt), colour = RBA[["Olive1"]]) +
  geom_special_gridline_rba()

g_rolls_bo_10_narrow <- ggrba(gd_rolls_bo_10 %>% filter(type == "Post-narrow")) +
  geom_vline(xintercept = -4.5, colour = RBA["Grey10"], linetype = "longdash") +
  geom_vline(xintercept = 0.5, colour = RBA["Grey10"], linetype = "dotted") +
  geom_point(aes(x = x, y = y, colour = type), size = 3) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high, colour = type)) +
  scale_colour_manual(values = colours) +
  scale_x_continuous_rba(limits = c(-8.5, 2.5), breaks = c(-8, -6, -4, -2, 0, 2), units = "") +
  scale_y_continuous_rba(limits = c(-0.5, 0.25), breaks = c(-0.5, -0.25, 0, 0.25), units = "bps") +
  annotate("text", x = -2, y = -0.125, label = "Post-narrow", size = round(20 / ggplot2::.pt), colour = RBA[["Olive4"]]) +
  geom_special_gridline_rba()

g_rolls_bo_10_delinked <- ggrba(gd_rolls_bo_10 %>% filter(type == "Post-delink")) +
  geom_vline(xintercept = -4.5, colour = RBA["Grey10"], linetype = "longdash") +
  geom_vline(xintercept = 0.5, colour = RBA["Grey10"], linetype = "dotted") +
  geom_point(aes(x = x, y = y, colour = type), size = 3) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high, colour = type)) +
  scale_colour_manual(values = colours) +
  scale_x_continuous_rba(limits = c(-8.5, 2.5), breaks = c(-8, -6, -4, -2, 0, 2), units = "") +
  scale_y_continuous_rba(limits = c(-0.5, 0.25), breaks = c(-0.5, -0.25, 0, 0.25), units = "bps") +
  annotate("text", x = -2, y = -0.125, label = "Post-delink", size = round(20 / ggplot2::.pt), colour = RBA[["Olive10"]]) +
  geom_special_gridline_rba()

g_rolls_bd_10_wide <- ggrba(gd_rolls_bd_10 %>% filter(type == "Pre-narrow")) +
  geom_vline(xintercept = -4.5, colour = RBA["Grey10"], linetype = "longdash") +
  geom_vline(xintercept = 0.5, colour = RBA["Grey10"], linetype = "dotted") +
  geom_point(aes(x = x, y = y, colour = type), size = 3) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high, colour = type)) +
  scale_colour_manual(values = colours) +
  scale_x_continuous_rba(limits = c(-8.5, 2.5), breaks = c(-8, -6, -4, -2, 0, 2), units = "") +
  scale_y_continuous_rba(limits = c(-100, 50)) +
  labs(title = "Best depth") +
  geom_special_gridline_rba()

g_rolls_bd_10_narrow <- ggrba(gd_rolls_bd_10 %>% filter(type == "Post-narrow")) +
  geom_vline(xintercept = -4.5, colour = RBA["Grey10"], linetype = "longdash") +
  geom_vline(xintercept = 0.5, colour = RBA["Grey10"], linetype = "dotted") +
  geom_point(aes(x = x, y = y, colour = type), size = 3) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high, colour = type)) +
  scale_colour_manual(values = colours) +
  scale_x_continuous_rba(limits = c(-8.5, 2.5), breaks = c(-8, -6, -4, -2, 0, 2), units = "") +
  scale_y_continuous_rba(limits = c(-100, 50)) +
  geom_special_gridline_rba()

g_rolls_bd_10_delinked <- ggrba(gd_rolls_bd_10 %>% filter(type == "Post-delink")) +
  geom_vline(xintercept = -4.5, colour = RBA["Grey10"], linetype = "longdash") +
  geom_vline(xintercept = 0.5, colour = RBA["Grey10"], linetype = "dotted") +
  geom_point(aes(x = x, y = y, colour = type), size = 3) +
  geom_errorbar(aes(x = x, ymin = ci_low, ymax = ci_high, colour = type)) +
  scale_colour_manual(values = colours) +
  scale_x_continuous_rba(limits = c(-8.5, 2.5), breaks = c(-8, -6, -4, -2, 0, 2), units = "", keep_first_label = "keep") +
  scale_y_continuous_rba(limits = c(-100, 50)) +
  geom_special_gridline_rba()

g_rolls_10 <- g_rolls_bo_10_wide + g_rolls_bd_10_wide +
  g_rolls_bo_10_narrow + g_rolls_bd_10_narrow +
  g_rolls_bo_10_delinked + g_rolls_bd_10_delinked +
  plot_layout(nrow = 3, ncol = 2) +
  plot_size_rba(plot.height = grid::unit(210, "mm")) +
  multipanel_title_rba(
    title = "10-year Futures Roll Period Liquidity",
    subtitle = "Multiday effects, dashed = start of roll, dotted = end of roll"
  )

### 2.6.3. Widening ####

g_widening_bo <- ggrba(gd_widening) +
  shading_rba(xmin = as_date("2022-09-09") - 0.5, xmax = as_date("2022-09-15") + 0.5) +
  shading_rba(xmin = as_date("2022-12-09") - 0.5, xmax = as_date("2022-12-15") + 0.5) +
  geom_vline(xintercept = date_widening - 0.5, colour = RBA["Grey10"], linetype = "longdash") +
  geom_line(aes(x = date, y = bo, colour = tenor)) +
  scale_colour_manual(values = colours) +
  scale_y_continuous_rba(limits = c(0, 2.4), breaks = c(0, 0.6, 1.2, 1.8, 2.4), units = "bps") +
  labs(title = "Bid-ask spreads") + # autolabel_var_rba(tenor)
  annotate(
    GeomText,
    x = as.Date("2022-10-04"),
    y = 0.356065162907268,
    label = "10-year",
    colour = "#ff6f20ff",
    size = 7.029,
    hjust = 0.5, vjust = 0.5) +
  annotate(
    GeomText,
    x = as.Date("2022-11-25"),
    y = 1.3986967418546,
    label = "3-year",
    colour = "#752fa5ff",
    size = 7.029,
    hjust = 0.5, vjust = 0.5)
# autolabel_rba(g_widening_bo, print_code = TRUE)

g_widening_bd <- ggrba(gd_widening) +
  shading_rba(xmin = as_date("2022-09-09") - 0.5, xmax = as_date("2022-09-15") + 0.5, ymin = 0.008) +
  shading_rba(xmin = as_date("2022-12-09") - 0.5, xmax = as_date("2022-12-15") + 0.5, ymin = 0.008) +
  geom_vline(xintercept = date_widening - 0.5, colour = RBA["Grey10"], linetype = "longdash") +
  geom_line(aes(x = date, y = bd, colour = tenor)) +
  scale_colour_manual(values = colours) +
  scale_y_continuous_rba(limits = c(0.008, 5), 
                         units = "’000", 
                         transform = scales::log_trans(), 
                         breaks = c(0.008,0.04, 0.2, 1, 5),
                         expand = expansion(mult = c(0.008, 0))) +
  labs(
    title = "Best depth",
    subtitle = "Log scale"
  )

g_widening_to <- ggrba(gd_widening) +
  shading_rba(xmin = as_date("2022-09-09") - 0.5, xmax = as_date("2022-09-15") + 0.5) +
  shading_rba(xmin = as_date("2022-12-09") - 0.5, xmax = as_date("2022-12-15") + 0.5) +
  geom_vline(xintercept = date_widening - 0.5, colour = RBA["Grey10"], linetype = "longdash") +
  geom_line(aes(x = date, y = to, colour = tenor)) +
  scale_colour_manual(values = colours) +
  scale_y_continuous_rba(limits = c(0, 240), breaks = c(0, 60, 120, 180, 240), units = "’000") +
  labs(title = "Turnover")

g_widening_pi <- ggrba(gd_widening) +
  shading_rba(xmin = as_date("2022-09-09") - 0.5, xmax = as_date("2022-09-15") + 0.5) +
  shading_rba(xmin = as_date("2022-12-09") - 0.5, xmax = as_date("2022-12-15") + 0.5) +
  geom_vline(xintercept = date_widening - 0.5, colour = RBA["Grey10"], linetype = "longdash") +
  geom_line(aes(x = date, y = pi, colour = tenor)) +
  scale_colour_manual(values = colours) +
  scale_y_continuous_rba(limits = c(0, 40), units = "bps") +
  labs(title = "Price impact")

g_widening <- autolabel_rba(g_widening_bo) + g_widening_to + g_widening_pi + g_widening_bd +
  plot_layout(nrow = 4, ncol = 1) +
  plot_size_rba(plot.height = grid::unit(280, "mm")) +
  multipanel_title_rba(
    title = "Increment Increase Liquidity Measures",
    subtitle = "Daily, dashed = increment increase, shading = roll periods"
  )

# 3. Produce outputs ####

## 3.1. RDP ####

ggsave_rba(format_rdp(g_turnover), filename = "Outputs/Average Daily Turnover in AGS Markets.svg")
ggsave_rba(format_rdp(g_sessions), filename = "Outputs/Average Intraday Turnover in 3- and 10-year Futures.svg")
ggsave_rba(format_rdp(g_daily_3), filename = "Outputs/3-year Futures Measures of Liquidity.svg")
ggsave_rba(format_rdp(g_daily_10), filename = "Outputs/10-year Futures Measures of Liquidity.svg")
ggsave_rba(format_rdp(g_autocorrelation), filename = "Outputs/Autocorrelations of Liquidity Measures.svg")

ggsave_rba(format_rdp(g_mar2020), filename = "Outputs/Measures of Liquidity in March 2020.svg")
ggsave_rba(format_rdp(g_octnov2021), filename = "Outputs/Measures of Liquidity in Oct-Nov 2021.svg")
ggsave_rba(format_rdp(g_marapr2025), filename = "Outputs/Measures of Liquidity in Mar-Apr 2025.svg")

ggsave_rba(format_rdp(g_news_bo), filename = "Outputs/Bid-ask Spreads for News Events.svg")
ggsave_rba(format_rdp(g_news_bd), filename = "Outputs/Best Depth for News Events.svg")
ggsave_rba(format_rdp(g_news_to), filename = "Outputs/Turnover for News Events.svg")
ggsave_rba(format_rdp(g_news_pi), filename = "Outputs/Price Impact for News Events.svg")

ggsave_rba(format_rdp(g_flow_to), filename = "Outputs/Turnover for Flow Events.svg")
ggsave_rba(format_rdp(g_flow_bd), filename = "Outputs/Best Depth for Flow Events.svg")
ggsave_rba(format_rdp(g_flow_pi), filename = "Outputs/Price Impact for Flow Events.svg")

ggsave_rba(format_rdp(g_syndications), filename = "Outputs/Syndication Period Liquidity.svg")
ggsave_rba(format_rdp(g_announcements), filename = "Outputs/Syndication Announcement Liquidity.svg")
ggsave_rba(format_rdp(g_pricings_bd), filename = "Outputs/Best Depth for Syndication Pricings.svg")
ggsave_rba(format_rdp(g_pricings_to), filename = "Outputs/Turnover for Syndication Pricings.svg")
ggsave_rba(format_rdp(g_pricings_pi), filename = "Outputs/Price Impact for Syndication Pricings.svg")

ggsave_rba(format_rdp(g_rolls_3), filename = "Outputs/3-year Futures Roll Period Liquidity.svg")
ggsave_rba(format_rdp(g_rolls_10), filename = "Outputs/10-year Futures Roll Period Liquidity.svg")
ggsave_rba(format_rdp(g_widening), filename = "Outputs/Increment Increase Liquidity Measures.svg")

## 3.2. Non-RDP ####

ggsave_rba(g_turnover, filename = "Outputs/Non-RDP/Average Daily Turnover in AGS Markets.png")
ggsave_rba(g_sessions, filename = "Outputs/Non-RDP/Average Intraday Turnover in 3- and 10-year Futures.png")
ggsave_rba(g_daily_3, filename = "Outputs/Non-RDP/3-year Futures Measures of Liquidity.png")
ggsave_rba(g_daily_10, filename = "Outputs/Non-RDP/10-year Futures Measures of Liquidity.png")
ggsave_rba(g_autocorrelation, filename = "Outputs/Non-RDP/Autocorrelations of Liquidity Measures.png")

ggsave_rba(g_mar2020, filename = "Outputs/Non-RDP/Measures of Liquidity in March 2020.png")
ggsave_rba(g_octnov2021, filename = "Outputs/Non-RDP/Measures of Liquidity in Oct-Nov 2021.png")
ggsave_rba(g_marapr2025, filename = "Outputs/Non-RDP/Measures of Liquidity in Mar-Apr 2025.png")

ggsave_rba(g_news_bo, filename = "Outputs/Non-RDP/Bid-ask Spreads for News Events.png")
ggsave_rba(g_news_bd, filename = "Outputs/Non-RDP/Best Depth for News Events.png")
ggsave_rba(g_news_to, filename = "Outputs/Non-RDP/Turnover for News Events.png")
ggsave_rba(g_news_pi, filename = "Outputs/Non-RDP/Price Impact for News Events.png")

ggsave_rba(g_flow_to, filename = "Outputs/Non-RDP/Turnover for Flow Events.png")
ggsave_rba(g_flow_bd, filename = "Outputs/Non-RDP/Best Depth for Flow Events.png")
ggsave_rba(g_flow_pi, filename = "Outputs/Non-RDP/Price Impact for Flow Events.png")

ggsave_rba(g_syndications, filename = "Outputs/Non-RDP/Syndication Period Liquidity.png")
ggsave_rba(g_announcements, filename = "Outputs/Non-RDP/Syndication Announcement Liquidity.png")
ggsave_rba(g_pricings_bd, filename = "Outputs/Non-RDP/Best Depth for Syndication Pricings.png")
ggsave_rba(g_pricings_to, filename = "Outputs/Non-RDP/Turnover for Syndication Pricings.png")
ggsave_rba(g_pricings_pi, filename = "Outputs/Non-RDP/Price Impact for Syndication Pricings.png")

ggsave_rba(g_rolls_3, filename = "Outputs/Non-RDP/3-year Futures Roll Period Liquidity.png")
ggsave_rba(g_rolls_10, filename = "Outputs/Non-RDP/10-year Futures Roll Period Liquidity.png")
ggsave_rba(g_widening, filename = "Outputs/Non-RDP/Increment Increase Liquidity Measures.png")
