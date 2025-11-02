# Define Functions
# Ben Jackman and Dmitry Titkov / MPI / DM
# November 2024

load_ags_futures <- function(tenor = "all", path = "Inputs/") {
  if (!(tenor %in% c("all", "3", "5", "10"))) {
    stop('tenor must be "all", "3", "5" or "10"')
  }
  if (tenor == "all") {
    df_3s_futs <- readRDS(file = paste0(path, "df_3s_futures_cleaned.rds"))
    df_5s_futs <- readRDS(file = paste0(path, "df_5s_futures_cleaned.rds"))
    df_10s_futs <- readRDS(file = paste0(path, "df_10s_futures_cleaned.rds"))
    ags_futures <- list()
    ags_futures$df_3s_futs <- df_3s_futs
    ags_futures$df_5s_futs <- df_5s_futs
    ags_futures$df_10s_futs <- df_10s_futs
    return(ags_futures)
  } else if (tenor == "3") {
    df_3s_futs <- readRDS(file = paste0(path, "df_3s_futures_cleaned.rds"))
    return(df_3s_futs)
  } else if (tenor == "5") {
    df_5s_futs <- readRDS(file = paste0(path, "df_5s_futures_cleaned.rds"))
    return(df_5s_futs)
  } else if (tenor == "10") {
    df_10s_futs <- readRDS(file = paste0(path, "df_10s_futures_cleaned.rds"))
    return(df_10s_futs)
  }
}

times <- function(hour_start, hour_end, frequency = 60) {
  times_all <- dplyr::mutate(tibble::enframe(seq(0, 86399, by = frequency), name = NULL, value = "time"),
                             time = hms::new_hms(as.numeric(time))
  )
  times <- dplyr::filter(
    times_all,
    lubridate::hour(time) >= hour_start &
      lubridate::hour(time) <= hour_end
  )
  return(times)
}

floor_hms <- function(x, secs = NULL, digits = NULL) {
  return(vctrs::vec_restore(floor(as.numeric(x) / secs) * secs, x))
}

month_start <- function(date) {
  return(lubridate::floor_date(date, "month"))
}

month_end <- function(date) {
  return(lubridate::ceiling_date(date, "month") - lubridate::days(1))
}

tidy.RD <- function(model, ...) {
  ret <- data.frame(
    term = model$bw,
    estimate = model$est,
    std.error = model$se,
    p.value = model$p
  )
  row.names(ret) <- NULL
  return(ret)
}

coefplot_data <- function(object,
                          .ci_level = 0.90,
                          .keep = NULL,
                          .drop = NULL,
                          .group = "auto",
                          .dict = fixest::getFixest_dict(),
                          .internal.only.i = FALSE,
                          .i.select = 1,
                          .aggr_es = "none",
                          .vcov = NULL,
                          .cluster = NULL,
                          .se = NULL) {
  return(iplot_data(object,
                    .ci_level = .ci_level,
                    .dict = .dict,
                    .keep = .keep,
                    .drop = .drop,
                    .internal.only.i = .internal.only.i,
                    .group = .group,
                    .vcov = .vcov,
                    .cluster = .cluster,
                    .se = .se
  ))
}

lr_estimates <- function(model, name) {
  y <- model$obs.stat
  ci <- model$interf.ci
  p <- model$p.value
  return(dplyr::tribble(
    ~name, ~y, ~ci_low, ~ci_high, ~p,
    name, y, ci[1], ci[2], p
  ))
}

did_estimates <- function(model, name) {
  y <- model$coefficients
  se <- model$se
  ci_low <- y - se
  ci_high <- y + se
  p <- fixest::pvalue(model)
  return(dplyr::tribble(
    ~name, ~y, ~se, ~ci_low, ~ci_high, ~p,
    name, y, se, ci_low, ci_high, p
  ))
}
