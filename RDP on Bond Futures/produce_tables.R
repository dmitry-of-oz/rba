# Produce Tables
# Dmitry Titkov / MPI / DM
# July 2025

# 0. Preliminaries ####

## 0.1. Packages ####

library(tidyverse)

## 0.2. Functions ####

source("define_functions.R")

# 1. Load data ####

source("model_rolls.R")
source("model_widening.R")

# 2. Process data ####

## 2.1. Local randomisation for rolls ####

t_lr <- bind_rows(
  lr_estimates(m_rolls_bo_3_wide_lr, "bo_3_wide"),
  lr_estimates(m_rolls_bd_3_wide_lr, "bd_3_wide"),
  lr_estimates(m_rolls_to_3_wide_lr, "to_3_wide"),
  lr_estimates(m_rolls_pi_3_wide_lr, "pi_3_wide"),
  lr_estimates(m_rolls_bo_3_narrow_lr, "bo_3_narrow"),
  lr_estimates(m_rolls_bd_3_narrow_lr, "bd_3_narrow"),
  lr_estimates(m_rolls_to_3_narrow_lr, "to_3_narrow"),
  lr_estimates(m_rolls_pi_3_narrow_lr, "pi_3_narrow"),
  lr_estimates(m_rolls_bo_3_widening_lr, "bo_3_widening"),
  lr_estimates(m_rolls_bd_3_widening_lr, "bd_3_widening"),
  lr_estimates(m_rolls_to_3_widening_lr, "to_3_widening"),
  lr_estimates(m_rolls_pi_3_widening_lr, "pi_3_widening"),
  lr_estimates(m_rolls_bo_3_delinked_lr, "bo_3_delinked"),
  lr_estimates(m_rolls_bd_3_delinked_lr, "bd_3_delinked"),
  lr_estimates(m_rolls_to_3_delinked_lr, "to_3_delinked"),
  lr_estimates(m_rolls_pi_3_delinked_lr, "pi_3_delinked"),
  lr_estimates(m_rolls_bo_10_wide_lr, "bo_10_wide"),
  lr_estimates(m_rolls_bd_10_wide_lr, "bd_10_wide"),
  lr_estimates(m_rolls_to_10_wide_lr, "to_10_wide"),
  lr_estimates(m_rolls_pi_10_wide_lr, "pi_10_wide"),
  lr_estimates(m_rolls_bo_10_narrow_lr, "bo_10_narrow"),
  lr_estimates(m_rolls_bd_10_narrow_lr, "bd_10_narrow"),
  lr_estimates(m_rolls_to_10_narrow_lr, "to_10_narrow"),
  lr_estimates(m_rolls_pi_10_narrow_lr, "pi_10_narrow"),
  lr_estimates(m_rolls_bo_10_delinked_lr, "bo_10_delinked"),
  lr_estimates(m_rolls_bd_10_delinked_lr, "bd_10_delinked"),
  lr_estimates(m_rolls_to_10_delinked_lr, "to_10_delinked"),
  lr_estimates(m_rolls_pi_10_delinked_lr, "pi_10_delinked")
) %>%
  separate(name, c("measure", "contract", "period")) %>%
  mutate(
    contract = as.numeric(contract),
    y = if_else(measure == "bd", (exp(y) - 1) * 100, y),
    ci_low = if_else(measure == "bd", (exp(ci_low) - 1) * 100, ci_low),
    ci_high = if_else(measure == "bd", (exp(ci_high) - 1) * 100, ci_high),
    stars = case_when(
      p <= 0.01 ~ "***",
      p > 0.01 & p <= 0.05 ~ "**",
      p > 0.05 & p <= 0.1 ~ "*",
      p > 0.1 ~ "",
      TRUE ~ as.character(NA)
    )
  )

t_lr_y <- t_lr %>%
  mutate(
    y = case_when(
      measure %in% c("bo", "pi") ~ paste0(round(y, 1), stars),
      TRUE ~ paste0(round(y, 0), stars)
    ),
    name = paste0(contract, "_", period)
  ) %>%
  pivot_wider(
    id_cols = measure,
    names_from = name,
    values_from = y
  )

t_lr_ci <- t_lr %>%
  mutate(
    ci = case_when(
      measure %in% c("bo", "pi") ~ paste0("(", round(ci_low, 1), ", ", round(ci_high, 1), ")"),
      TRUE ~ paste0("(", round(ci_low, 0), ", ", round(ci_high, 0), ")")
    ),
    name = paste0(contract, "_", period)
  ) %>%
  pivot_wider(
    id_cols = measure,
    names_from = name,
    values_from = ci
  )

## 2.2. Difference in differences for widening ####

t_did <- bind_rows(
  did_estimates(m_widening_bo_att, "bo_full"),
  did_estimates(m_widening_bd_att, "bd_full"),
  did_estimates(m_widening_to_att, "to_full"),
  did_estimates(m_widening_pi_att, "pi_full"),
  did_estimates(m_widening_bo_att_20, "bo_20"),
  did_estimates(m_widening_bd_att_20, "bd_20"),
  did_estimates(m_widening_to_att_20, "to_20"),
  did_estimates(m_widening_pi_att_20, "pi_20"),
  did_estimates(m_widening_bo_att_10, "bo_10"),
  did_estimates(m_widening_bd_att_10, "bd_10"),
  did_estimates(m_widening_to_att_10, "to_10"),
  did_estimates(m_widening_pi_att_10, "pi_10")
) %>%
  separate(name, c("measure", "period")) %>%
  mutate(
    y = if_else(measure == "bd", (exp(y) - 1) * 100, y),
    ci_low = if_else(measure == "bd", (exp(ci_low) - 1) * 100, ci_low),
    ci_high = if_else(measure == "bd", (exp(ci_high) - 1) * 100, ci_high),
    stars = case_when(
      p <= 0.01 ~ "***",
      p > 0.01 & p <= 0.05 ~ "**",
      p > 0.05 & p <= 0.1 ~ "*",
      p > 0.1 ~ "",
      TRUE ~ as.character(NA)
    )
  )

t_did_y <- t_did %>%
  mutate(y = case_when(
    measure %in% c("bo", "to", "pi") ~ paste0(round(y, 1), stars),
    TRUE ~ paste0(round(y, 0), stars)
  )) %>%
  pivot_wider(
    id_cols = measure,
    names_from = period,
    values_from = y
  )

t_did_se <- t_did %>%
  mutate(ci = case_when(
    measure %in% c("bo", "to", "pi") ~ paste0("(", round(se, 1), ")"),
    TRUE ~ paste0("(", round(ci_low, 0), ", ", round(ci_high, 0), ")")
  )) %>%
  pivot_wider(
    id_cols = measure,
    names_from = period,
    values_from = ci
  )

# 3. Produce outputs ####

write_csv(t_lr_y, "Outputs/Local Randomisation Estimates.csv")
write_csv(t_lr_ci, "Outputs/Local Randomisation Confidence Intervals.csv")

write_csv(t_did_y, "Outputs/Difference-in-differences Estimates.csv")
write_csv(t_did_se, "Outputs/Difference-in-differences Standard Errors.csv")
