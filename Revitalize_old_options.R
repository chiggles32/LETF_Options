library(lubridate)
library(quantmod)
library(tidyverse)

all_data = list.files("C:/Users/Charlie/Desktop/R Stuff/Cleaned_Option_Data", recursive = TRUE)

old_data = all_data[which(!grepl("CS_", all_data))]

underlying = data.frame(c("TQQQ","SQQQ","QQQ", "SPY", "SPXL", "SPXS", "TNA",  "TZA",  "IWM",  "NVDA", "NVDU", "NVDD"), 
                        c("y", "y", "n", "n", "y", "y", "y", "y", "n","n", "y", "y"), 
                        c(3,-3,1,1,3,-3,3,-3,1,1,2,-2),
                        c("QQQ", "QQQ", "QQQ", "SPY", "SPY", "SPY", "IWM", "IWM", "IWM","NVDA", "NVDA", "NVDA"))

names(underlying) = c("U","L","B", "Index")


index = underlying |>
  filter(L == "n") |>
  pull(U)

index_table = underlying |>
  select(U, Index)

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

for (i in old_data){
  
    setwd("C:\\Users\\Charlie/Desktop/R Stuff/Cleaned_Option_Data")
    print(i)
    x = read.csv(i)
    
    if (dim(x)[2] < 30){
    
      x = x |>
        left_join(index_table, by = "U")
      
    ODAY = x$ODay[1]
    
    HVOL_tbl = underlying |>
      select(Index) |>
      distinct() |>
      rowwise() |>
      mutate("Vol_Trend" = VolTrend(get(Index), ODAY)) |>
      ungroup()
    
    x = x |>
      left_join(HVOL_tbl, by = "Index")
    
    setwd("C:\\Users\\Charlie/Desktop/R Stuff/Cleaned_Option_Data")
    
    op = "C:\\Users\\Charlie/Desktop/R Stuff/Cleaned_Option_Data"
    opf = file.path(op, i)
    
    write.csv(x, file = opf, row.names = FALSE)
    }
    else {print("Ooops")}
  
  
}