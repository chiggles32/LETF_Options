# "feature" engineering"

library(tidyverse)
library(lubridate)
library(quantmod)

# get()

##source functions

source("C:\\Users/Charlie/Desktop/LETF_Options/Options_Functions.R")

path_to_raw = "C:/Users/Charlie/Desktop/R Stuff/Options Pull"
path_to_clean = "C:/Users/Charlie/Desktop/R Stuff/Cleaned_Option_Data"

raw_data = list.files(path_to_raw)

clean_data = list.files(path_to_clean)

need_to_clean = raw_data %in% clean_data

#####

underlying = data.frame(c("TQQQ","SQQQ","QQQ", "SPY", "SPXL", "SPXS", "TNA",  "TZA",  "IWM",  "NVDA", "NVDU", "NVDD"), 
                        c("y", "y", "n", "n", "y", "y", "y", "y", "n","n", "y", "y"), 
                        c(3,-3,1,1,3,-3,3,-3,1,1,2,-2),
                        c("QQQ", "QQQ", "QQQ", "SPY", "SPY", "SPY", "IWM", "IWM", "IWM","NVDA", "NVDA", "NVDA"))

names(underlying) = c("U","L","B", "Index")



index = underlying |>
  filter(L == "n") |>
  pull(U)

######

for (i in index){
  getSymbols(i, from = "2023-01-01")
}

#gets hvol from quote date to lookback day
#create table and join after grouping into single quoteday
HVol = function(prices, quotedate, lookback){
  #annualized
  quotedate = ifelse(as.Date(quotedate) > max(index(prices)), as.Date(quotedate) - 1, as.Date(quotedate))
  start_d = which(index(prices)==quotedate)
  end_d = start_d - lookback
  trimmed = prices[end_d:start_d,]
  trimmed$returns = Delt(trimmed[,6], type = "log")
  
  out = sd(trimmed$returns, na.rm = TRUE)
  out*(252^.5)
}
VolTrend = function(prices, quotedate){
  #90 60 30 10 vol change
  x = c()
  trade_days = c(90,60,30,10)
  for (i in 1:length(trade_days)) {
    x[i] = HVol(prices, quotedate, trade_days[i])
  }
  mean(Delt(x, type = "log"), na.rm = TRUE)
}

######
old_names = c("Contract.Name", "Last.Trade.Date", "Strike", "Bid", "Ask", "QuoteTime", "HVol", "Type", "U", "Expiry", "Price", "S0", "ODay", "t", "Intrinsic", "N.Price", "ImpVol", "delta", "gama", "theta", "vega", "rho", "L", "B", "LM", "MeanVol", "Scaled.LM", "Scaled.ImpVol")


setwd("C:\\Users\\Charlie/Desktop/R Stuff/Options Pull")


#and hvol
#intrinsic/nprice/greeks/LB:Scaled
#utc vs est




for (i in 1:length(need_to_clean)){
  if (need_to_clean[i]){ print("Cleaned")} else {
    
    setwd("C:\\Users\\Charlie/Desktop/R Stuff/Options Pull")
    print(raw_data[i])
    x = read.csv(raw_data[i])
    
    x = x |>
      filter(!(is.na(bid) | is.na(ask)))
    
    x = x |>
      select(-theoreticalOptionValue, -theoreticalVolatility, -bidAskSize, -mark, -description, -exchangeName, -netChange, -daysToExpiration, -expirationType, -lastTradingDay, -multiplier, -settlementType, -deliverableNote, -percentChange, -markChange,-markPercentChange, - exerciseType, -nonStandard, -pennyPilot, -mini, -timeValue, -extrinsicValue, -high52Week, -low52Week, -lastSize:-closePrice) |>
      mutate(S0 = ifelse(tolower(putCall) == "call", strikePrice + intrinsicValue, strikePrice - intrinsicValue),
             Type = ifelse(tolower(putCall) == "call", "c", "p"),
             ODay = as.Date(strftime(as_datetime(quoteTimeInLong/1000), '%Y-%m-%d %H:%M:%OS3')),
             DotW = weekdays(ODay),
             RiskFree = .045,
             ImpVol = volatility/100,
             QT = floor_date(as_datetime(quoteTimeInLong/1000 - (4*60*60) ), unit = "30 mins"),# need to get rid of duplicates in truncated time
             U = optionRoot,
             Last.Trade.Date = as.Date(strftime(as_datetime(as.numeric(tradeTimeInLong)/1000), '%Y-%m-%d %H:%M:%OS3')),
             Contract.Name = gsub("\\s+", "", symbol),
             Strike = strikePrice,
             Expiry = as.Date(strftime(as_datetime(quoteTimeInLong/1000), '%Y-%m-%d %H:%M:%OS3')),
             t = as.numeric(Expiry - ODay),
             gama = `gamma`,
             Last.Trade.Date = if_else(Last.Trade.Date == "1969-12-31",
                                       NA,
                                       Last.Trade.Date),
             Price = ave.V(bid, ask),
             QuoteTime = as.numeric(QT),
             Ask = ask,
             Bid = bid,
             Intrinsic = intrinsicValue,
             N.Price = NA) |>
      left_join(underlying, by = "U") |>
      Scaling.Function() |>
      filter(ODay == names(table(ODay))[as.vector(table(ODay))==max(table(ODay))]) 
    
    ODAY = x$ODay[1]
    
    HVOL_tbl = underlying |>
      select(Index) |>
      distinct() |>
      rowwise() |>
      mutate("HVol" = HVol(get(Index), ODAY, 40),
             "Vol_Trend" = VolTrend(get(Index), ODAY)) |>
      ungroup()
    
    x = x |>
      left_join(HVOL_tbl, by = "Index") |>
      select(all_of(old_names), Index, Vol_Trend)
    
    setwd("C:\\Users\\Charlie/Desktop/R Stuff/Cleaned_Option_Data")
    
    op = "C:\\Users\\Charlie/Desktop/R Stuff/Cleaned_Option_Data"
    opf = file.path(op, raw_data[i])
    
    write.csv(x, file = opf, row.names = FALSE)
    
  }
  
}


