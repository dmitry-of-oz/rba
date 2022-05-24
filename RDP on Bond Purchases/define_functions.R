# Define Functions (to Work with Dates)
# Dmitry Titkov / MPI / DM
# July 2021

month_start <- function (date) {
  return(lubridate::floor_date(date, "month"))
}

month_end <- function (date) {
  return(lubridate::ceiling_date(date, "month") - lubridate::days(1))
}

month_ends <- function (date_start, date_end) {
  return(enframe(month_end(seq(month_start(date_start), month_start(date_end), "months")), name = NULL, value = "date"))
}

week_ags <- function (date) {
  return(lubridate::ceiling_date(date, "week", week_start = 5, change_on_boundary = FALSE)) # for weekly AGS purchases, starting Mondays
}

week_semis <- function (date) {
  return(lubridate::ceiling_date(date, "week", week_start = 2, change_on_boundary = FALSE)) # for weekly semis purchases, starting Wednesdays
}

days <- function (date_start, date_end, frequency = "days") {
  return(enframe(seq(date_start, date_end, frequency), name = NULL, value = "date"))
}

days_to_month_ends <- function (data) {
  
  # Define dates ####
  data_days <- days(min(data$date), max(data$date))
  data_month_ends <- month_ends(min(data$date), max(data$date))
  
  # Apply dates ####
  data_joined <- dplyr::right_join(data,
                                   data_days,
                                   by = "date")
  data_arranged <- dplyr::arrange(data_joined,
                                  date)
  data_mutated <- dplyr::mutate_if(data_arranged,
                                   is.numeric,
                                   ~ zoo::na.locf(., na.rm = FALSE))
  data_filtered <- dplyr::filter(data_mutated,
                                 date %in% data_month_ends$date)
  return(data_filtered)
  
}