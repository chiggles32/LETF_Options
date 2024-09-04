# So, we want to create a program that can capture the trades and keep track of P&L, exposures, etc
# of a trading strategy across multiple assets and multiple time frames

# Issues:
#   Exiting trades before expiration - if IVs cross than it would be optimal to close the position early
#   Do we rebalance trades? If IVs diverge even further do we double down
#     How do we size each individual trade
#     Stop loss/ profit taking/ expiration (triple barrier)
#     New greek calculations
#     Walk forward nature
#     Data LOL
# Grouping by underlying
#     
# How real should it be? like buying the actual contracts or just the returns. Using the minimum contracts function?
# 
# Things to consider:
#   MLdP machine learning 
# Throwing the Kelly Criterion in here somewhere

# And it begins 

# Given that each input to this function is a the dataframe of all the trades we want to execute 
# for this time step
#   - need to check the "book" to see if that specific trade exists, if not add
#   - check all existing trades to see if any should be closed out due to barriers
#       - Lends itself to calcualting current P&L

book = new.env()

# trade.book needs to have the following: essentially just trade meta data that will allow for computations
#   - Identity of legs of the trade to compare
#   - Initial weight for each leg
#   - Entry price (and greeks?)
#   - Based on the above, PnL 
# 
# Columns
#   Trade_Time - This will be whenever a new trade set gets brought over
#   ID - combination of both contract names formatted as "Long.Short"
#   Starting_Price L/S
#   Long_Weight - weight of long leg
#   Short_Weight - weight of short leg
#   Action - Specifies what the hell is going on ie maintaining position, entry, exit, etc
#   PnL - profit and loss at a given point in time - 0 when action = "entry"
#   

#### Structure for book keeping ####

assign("open.trades", data.frame(), envir = book)

assign("closed.trades", data.frame(), envir = book)

# Need a function to bring closed trades over to the closed.trades book

Book_Keep = function(open = book$open.trades, closed = book$closed.trades) {
  
  out.closed = open |>
    filter(Action == "Exit") |>
    rbind(closed)
  
  out.open = open |>
    filter(Action != "Exit")
  
  assign("open.trades", out.open, envir = book)
  assign("closed.trades", out.closed, envir = book)
  
}
  
#### Updating the book ####

new.trades




##### Creating Synthetic Trade Data ####

library(zoo)
library(xts)
library(tidyverse)

securities = replicate(10, toupper(paste0(sample(letters,4, replace = TRUE), collapse = "")))

quotes = new.env()



start_time = as.POSIXct("2024-01-01 00:00:00")
time_index = seq(from = start_time, by = "30 mins", length.out = 10)

generate.contract = function(x){
  paste0(sample(securities[1:x], 1), 
         paste0(sample(0:9, 6, replace = TRUE), collapse = ""),
         paste0(sample(c("P", "C"), 1), collapse = ""),
         paste0(sample(0:9, 8, replace = TRUE), collapse = ""),
         collapse = "")
}

leg_names = function(legs, x){
  replicate(legs, generate.contract(x))
}

time_series = xts(order.by = time_index)

new.trades = function(n_securities, n, legs, time){
  
  n_trades = abs(round(rnorm(1, n, 5)))
  
  trades = t(replicate(n_trades, leg_names(legs = legs, x = n_securities)))
  
  trades = as.data.frame(trades)
  
  names(trades) = paste0("leg", 1:legs)
  
  x = 1:legs
  categ = c("Strike", "Current_Price", "Greeks", "IV", "S0")
  new_columns <- set_names(rep(list(abs(rnorm(n_trades))), length(x)*length(categ)), paste(rep(categ, each = length(x)), x, sep = ""))
  
  trades = trades |>
    mutate(ID = pmap_chr(across(everything()), paste, sep = ","),
           Observation_Time = time,
           Action = "Open",
           !!!new_columns
           )
    
    
  print(trades)
}
# adding the price columns and more


create.price.series = function(underlying, timeIndex, n_strike){
  
}


##### Updating Open Book ####

 # need an underlying/option price data base - each time step will have all necessary info, so call into that

get.price = function(underlying, otime){
  
}

update_book = function(new_trades, open = book$open.trades){
  # update the open book first
  
  
  # append book
  new_trades = new_trades |>
    filter(!(ID %in% book$open.trades$ID))
  
  book$open.trades =  bind_rows(list(new_trades, book$open.trades))
  
}