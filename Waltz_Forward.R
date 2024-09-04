# dependencies
library(tidyverse)

# constants
time_str1 = "09:30:00 AM" # open Quotes
time_str2 = "12:30:00 PM" # Midday quotes
time_str3 = "03:30:00 PM" # close quotes

# functions

source("C:\\Users/Charlie/Desktop/LETF_Options/Options_Functions.R")

data.import = function(filename, source_dir) {
  raw_data <<- read.csv(paste(source_dir, filename, sep = "/"))
  print(paste(filename, "Succesfully loaded as 'raw_data'"))
}

  # price as % function
time.price = function(timevalue, underlying) {
  return(
    timevalue / underlying
  )
}

  # pre_process

pre_process = function(x) {
  # filter criteria: has to be traded ie float>1, has to have impvol, dates have to make sense
  
  
    x = x |>
      filter(!is.na(Last.Trade.Date),
             !is.na(Scaled.ImpVol),
             as.Date(ODay) <= as.Date(Expiry),
             ImpVol > .03
             ) |>
      mutate(TimeValue = Price - Intrinsic,
             vol_price = time.price(TimeValue, S0),
             Open = as.numeric(as.POSIXct(paste(ODay,time_str1), format = "%Y-%m-%d %I:%M:%S %p")),
             Midday = as.numeric(as.POSIXct(paste(ODay,time_str2), format = "%Y-%m-%d %I:%M:%S %p")),
             Close = as.numeric(as.POSIXct(paste(ODay,time_str3), format = "%Y-%m-%d %I:%M:%S %p"))
             ) |>
      group_by(QuoteTime, Contract.Name) |>
      slice_head(n=1) |>
      ungroup()
    
    open_quotes = x |>
      mutate(time_dif = abs(QuoteTime - Open)) |>
      group_by(Contract.Name) |>
      slice_min(time_dif) |>
      select(-time_dif) |>
      ungroup()
    
    mid_quotes = x |>
      mutate(time_dif = abs(QuoteTime - Midday)) |>
      group_by(Contract.Name) |>
      slice_min(time_dif) |>
      select(-time_dif) |>
      ungroup()
    
    close_quotes = x |>
      mutate(time_dif = abs(QuoteTime - Close)) |>
      group_by(Contract.Name) |>
      slice_min(time_dif) |>
      select(-time_dif) |>
      ungroup()
    
    rbind(open_quotes, mid_quotes, close_quotes) |>
      distinct()
      
  
}

# locations
data_lake = "C:/Users/Charlie/Desktop/R Stuff/Cleaned_Option_Data"

# set a loop of files to load and manipulate

data_files = list.files(data_lake)

for (i in data_files[70]) {
  
  data.import(i, data_lake) # returns df n x 30 called "raw_data"
  
  p_data = raw_data |>
    pre_process()
  
  # pre_process
    # has to have been traded 
    # split into quotetimes
    # remove duplicates from time rounding (if any)
    # add price % var (price / underlying)
}

pp_data = raw_data |>
  pre_process()

raw_data_2 = raw_data
data.import(data_files[69], data_lake)
