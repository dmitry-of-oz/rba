## AGS Futures Market 
## Load raw data files, aggregate, and save to a drive location.
# Ben Jackman 2024

# Takes about 8 minutes and around 25gb of RAM. 

# Load libraries ####
library(tidyverse)
library(purrr)

# Locations ####
raw_data_location <- "//your/path/here/Raw AGS Futures Data/"

cleaned_data_output_location <- "//your/path/here/Cleaned/"

# Load and arrange data ####
load_futs_files <- function(path = raw_data_location) {
  
  # list of the data files
  file_list <- list.files(path = path)
  file_list <- purrr::map(file_list, function(x) paste0(path,x))
  
  # Load data into the global environment (7gb RAM 1min on HO desktop).
  lapply(file_list,load,.GlobalEnv)
}

# load dataframes. Takes around 1 min on head office PC; 8gb RAM. 
load_futs_files()

# list objects in global environment for various futures contracts by tenor
# must be outside functions as function environment returns nothing for ls()
list3s <- ls()[str_detect(ls(),"^Y[A-Z0-9]{3}$")]
list5s <- ls()[str_detect(ls(),"^V[A-Z0-9]{4}$")]
list10s <- ls()[str_detect(ls(),"^X[A-Z0-9]{3}$")]

# combine futures dataframes into one long dataframe by tenor
# removes individual contract dataframes from environment
combine_3s_futs <- function() {
  
  df_3s_futs <- lapply(list3s, function(xx){
    df <- get(xx)
    df <- df |> 
      mutate(futs_contract = xx)
  }) |> bind_rows()
  rm(list = list3s, pos = ".GlobalEnv") # remove old df objects to save RAM
  return(df_3s_futs)
}

combine_5s_futs <- function() {
  
  df_5s_futs <- lapply(list5s, function(xx){
    df <- get(xx)
    df <- df |> 
      mutate(futs_contract = xx)
  }) |> bind_rows()
  rm(list = list5s, pos = ".GlobalEnv") # remove old df objects to save RAM
  return(df_5s_futs)
}

combine_10s_futs <- function() {
  
  df_10s_futs <- lapply(list10s, function(xx){
    df <- get(xx)
    df <- df |> 
      mutate(futs_contract = xx)
  }) |> bind_rows()
  rm(list = list10s, pos = ".GlobalEnv") # remove old df objects to save RAM
  return(df_10s_futs)
}

ags_futures <- list()
ags_futures$df_3s_futs <- combine_3s_futs()
ags_futures$df_5s_futs <- combine_5s_futs()
ags_futures$df_10s_futs <- combine_10s_futs()

# Memory cleanup to further manage ram usage ####
rm(end, events, list10s, list3s, list5s, start, combine_10s_futs, 
   combine_5s_futs, combine_3s_futs, load_futs_files)
gc()

# Clean data ####
remove_condcode_xt <- function(df) {
  df <- df |> 
    dplyr::mutate(condcode_XT = grepl("XT", condcode)) |> 
    dplyr::filter(condcode_XT == FALSE) |> 
    select(-c(condcode_XT))
  return(df)
}

ags_futures <- purrr::map(ags_futures, remove_condcode_xt)

remove_zero_size_and_value_trades <- function(df) {
  df <- df |> 
    filter(!(size == 0 & value == 0))
  return(df)
}

ags_futures <- purrr::map(ags_futures, remove_zero_size_and_value_trades)

# Save clean data ####
# Takes about 7 or 8 minutes as of June 2024
tictoc::tic()
saveRDS(ags_futures$df_3s_futs, file = paste0(cleaned_data_output_location,"df_3s_futures_cleaned.rds"))
saveRDS(ags_futures$df_5s_futs, file = paste0(cleaned_data_output_location,"df_5s_futures_cleaned.rds"))
saveRDS(ags_futures$df_10s_futs, file = paste0(cleaned_data_output_location,"df_10s_futures_cleaned.rds"))
tictoc::toc()
