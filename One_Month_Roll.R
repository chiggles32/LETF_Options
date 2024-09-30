# Compare returns on a 1 month roll strategy for letfs vs market (index). Directionally long market

library(tidyverse)
library(lubridate)
library(quantmod)
library(bizdays)

# lets try a 5% and 10% otm long options 
# roll contracts on weekly basis ie every monday add contracts to track
# plot contracts against eachother on return, can work in trading costs after

# will kind of resemble the backtesting engine
# list item will represent one observation time, in this case EOD


# calendar loading 
start_date = as.Date("2023-10-29")
end_date = Sys.Date()
load_quantlib_calendars("UnitedStates/NYSE", from = start_date, to = end_date)


pull_days = bizseq(start_date, end_date, cal = "QuantLib/UnitedStates/NYSE")
pull_days = adjust.next(unique(floor_date(pull_days, "weeks") + 1), "QuantLib/UnitedStates/NYSE")

source_folder = "C:/Users/Charlie/Desktop/R Stuff/Cleaned_Option_Data"

source_files = list.files(source_folder)

get_date = function(x) {
  substr(x, nchar(x) - 9, nchar(x))
}

source_days = sapply(source_files, get_date, USE.NAMES = FALSE)

source_days = as.Date(source_days)

source_paths = paste0(source_folder, "/", source_files)

source_paths = source_paths[source_days %in% pull_days]

all_data = data.frame(source_paths, 
                      source_days[source_days %in% pull_days])

names(all_data) = c("path", "date")

all_data = all_data |>
  mutate(date = as.Date(date)) |>
  arrange(date)

# now with our days and paths, we can loop through each day to create a daily order book

x = read.csv(all_data[1,1])

get_monthly_contracts = function(x) {
  
  #parameters to target ie month left, slightly out of the money
  target_moneyness = .03
  target_maturity = 30
  
  x |>
    filter(!is.na(Scaled.ImpVol), QuoteTime == max(QuoteTime)) |>
    mutate(Analog = if_else(B < 0, if_else(Type == "p", "c", "p"), Type),
           mat_dif = abs(t - target_maturity),
           mon_dif = abs(Scaled.LM - target_moneyness)) |>
    filter(Analog == "c") |>
    group_by(U) |>
    filter(mat_dif == min(mat_dif)) |> # cleaner way to do this?
    slice_min(mon_dif)
    
}

y = get_monthly_contracts(x)

# create_entry = function() {
#   list(`Date` = NA,
#         `Buy` = NA,
#         `Inventory` = NA,
#         `Sell` = NA,)
# }

# book = create_entry()

# append a data frame with columns indicating buy/sell/hold in one column, PnL in another

# need to write an exit function that depends on contracts

# use this function to keep track of trades and book pnl after exit 
# can update to mark at each observation for triple barrier testing in future
# all info is contained in the new rows

book_update = function(new_trades, close_trades, book = NA) {
  
  if (is.na(book)) {
    
    updated_book = new_trades |>
      mutate(Transaction = "Open",
             PnL = NA,
             TIP = NA,
             ImpVolDif = NA)
  } else {
  
  updated_book = book
  
  # Shift buys to hold
  updated_book = updated_book |>
    filter(Transaction != "Close") |>
    mutate(Transaction = if_else(Transaction == "Open",
                                 "Hold",
                                 Transaction))
  
  # add columns to new trades
  new_trades = new_trades |>
    mutate(Transaction = "Open",
           PnL = NA,
           TIP = NA,
           ImpVolDif = NA)
  
  # add columns to close trades (calculate PnL later)
  close_trades = close_trades |>
    mutate(Transaction = "Close")
  
  # add transactions to book
  updated_book = rbind(updated_book, new_trades, close_trades)
  
  # calculate PnL for close
  closed_contracts = close_trades$Contract.Name
  
  positions = updated_book |>
    filter(Contract.Name %in% closed_contracts, Transaction != "Hold") |>
    group_by(Contract.Name) |>
    summarize(
      Contract.Name = first(Contract.Name),
      Entry_Price = first(Price[Transaction == "Open"]),
      Exit_Price = first(Price[Transaction == "Close"]),
      Entry_Date = first(ODay[Transaction == "Open"]),
      Exit_Date = first(ODay[Transaction == "Close"]),
      Entry_IV = first(Scaled.ImpVol[Transaction == "Open"]),
      Exit_IV = first(Scaled.ImpVol[Transaction == "Close"])
    )
  
  # pnl is table to join and coalesce with
  
  pnl = positions |>
    mutate(n_PnL = (Exit_Price - Entry_Price) / Entry_Price,
           n_TIP = as.numeric(as_date(Exit_Date) - as_date(Entry_Date)),
           n_ImpVolDif = Exit_IV - Entry_IV,
           Transaction = "Close") |>
    select(Contract.Name, Transaction, n_PnL, n_TIP, n_ImpVolDif)
  
  # join book with calculated pnl
  
  updated_book = updated_book |>
    left_join(pnl, by = c("Contract.Name", "Transaction")) |>
    mutate(PnL = coalesce(n_PnL, PnL),
           TIP = coalesce(n_TIP, TIP),
           ImpVolDif = coalesce(n_ImpVolDif, ImpVolDif)) |>
    select(-n_ImpVolDif, -n_PnL, -n_TIP)
  
  updated_book
  
  }
}

# have to think about how to handle all types of added orders

for (i in 1:nrow(all_data)) {
  
  x = read.csv(all_data[i, 1])
  
  buy_orders = get_monthly_contracts(x)
  
  if (i == 1) {
    order_book <<- 
  }
  
  
}
