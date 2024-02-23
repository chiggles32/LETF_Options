library(tidyverse)
################
underlying = data.frame(c("TQQQ","SQQQ","QQQ"), c("y", "y", "n"), c(3,-3,1))
names(underlying) = c("U","L","B")

index = underlying |>
  filter(L == "n") |>
  pull(U)

####Custom Functions####

ave = function(x, y) {
  if (x == 0 | y == 0) {
    return(0)
  } else {
    return((x + y) / 2)
  }
}

ave.V = Vectorize(ave)

#Generalized BSM Equations

GBSM.Price = function(spot, strike, volatility, riskfree, yield, dte, type){
  
  t = dte/365.25
  
  d1 = (1 / (volatility * sqrt(t))) * (log(spot/strike) + (riskfree - yield + (.5 * volatility^2)) * t)
  d2 = d1 - (volatility * sqrt(t))
  
  price = ifelse(type == "c",
                 (spot * exp(-yield * t) * pnorm(d1)) - (strike * exp(-riskfree * t) * pnorm(d2)),
                 (strike * exp(-riskfree * t) * pnorm(-d2)) - (spot * exp(-yield * t) * pnorm(-d1))
  )
  price
}

GBSM.Greeks = function(spot, strike, volatility, riskfree, yield, dte, type){
  
  t = dte/365.25
  
  d1 = (1 / (volatility * sqrt(t))) * (log(spot/strike) + (riskfree - yield + (.5 * volatility^2)) * t)
  d2 = d1 - (volatility * sqrt(t))
  
  price = ifelse(type == "c",
                 (spot * exp(-yield * t) * pnorm(d1)) - (strike * exp(-riskfree * t) * pnorm(d2)),
                 (strike * exp(-riskfree * t) * pnorm(-d2)) - (spot * exp(-yield * t) * pnorm(-d1))
  )
  
  delta = ifelse(type == "c",
                 exp(-yield*t) * pnorm(d1),
                 exp(-yield*t) * (pnorm(d1)-1)
  )
  
  gama = (dnorm(d1) * exp(-yield*t)) / (spot * volatility * sqrt(t))
  
  theta = ifelse(type == "c",
                 ((-spot * dnorm(d1) * volatility * exp(-yield * t)) / (2 *sqrt(t))) + (yield * spot * pnorm(d1) * exp(-yield*t)) - (riskfree * strike * exp(-riskfree * t) * pnorm(d2)),
                 ((-spot * dnorm(d1) * volatility * exp(-yield * t)) / (2 *sqrt(t))) - (yield * spot * pnorm(-d1) * exp(-yield*t)) + (riskfree * strike * exp(-riskfree * t) * pnorm(-d2))
  )
  vega = spot * sqrt(t) * dnorm(d1) * exp(-yield * t)
  
  rho = ifelse(type == "c",
               strike * t * exp(-riskfree * t) * pnorm(d2),
               -strike * t * exp(-riskfree * t) * pnorm(-d2)
  )
  results = cbind(delta, gama, theta, vega, rho)
  
  results
  
  
}

#IV Functions

error = function(price, spot, strike, volatility, riskfree, yield, dte, type){
  x = price - GBSM.Price(spot, strike, volatility, riskfree, yield, dte, type)
  x
}

Implied.Volatility = function(price, spot, strike, riskfree, yield, dte, type) {
  x = tryCatch({
    uniroot(
      f = error,
      interval = c(.01,3),
      price = price,
      spot = spot,
      strike = strike,
      riskfree = riskfree,
      yield = yield,
      dte = dte,
      type = type)}, 
    error = function(err) {
      cat("Error: ", conditionMessage(err), "\n")
      return(NA)
    })
  x[[1]]
}

IV = Vectorize(Implied.Volatility)

yahoo.clean.data = function(x){
  
  out = x |>
    select(-Last.Price, -Change, -X..Change, -Volume, -Open.Interest, -Implied.Volatility) |>
    mutate(across(Strike:HVol, as.numeric),
           Bid = if_else(is.na(Bid), 0, Bid),
           Ask = if_else(is.na(Bid), 0, Bid)) |>
    distinct()
  
  out = out 
}

data.step1 = function(x){#Making it usable #assuming 0 dividend yield
  out = x |>
    mutate(Type = substr(Contract.Name, nchar(Contract.Name) - 8, nchar(Contract.Name) - 8), 
           U = sub("\\d.*", "", Contract.Name),
           Expiry = as_date(substr(Contract.Name, str_length(Contract.Name)-14, str_length(Contract.Name)-9)), 
           Price = ave.V(Bid,Ask), 
           S0 = case_when(U == "QQQ" ~ QQQ,
                          U == "TQQQ" ~ TQQQ,
                          U == "SQQQ" ~ SQQQ), 
           ODay = as_date(as.POSIXct.numeric(QuoteTime)),
           t = as.numeric(Expiry - ODay), 
           Last.Trade.Date = as_date(substr(Last.Trade.Date,1,10)),
           HVol = if_else(U == "QQQ", HVol/3, HVol),
           Type = tolower(Type),
           Intrinsic = if_else(Type == "c", S0 - Strike, Strike - S0),
           N.Price = if_else(Intrinsic > 0 & Price == 0, Intrinsic, Price)) |>
    #filter(ODay == Last.Trade.Date) |>
    mutate(ImpVol = IV(Price, S0, Strike, .045, 0, t, Type) ) |>
    select(-TQQQ, -SQQQ, -QQQ) 
}

add.Greeks = function(x){
  out = GBSM.Greeks(x$S0, x$Strike, x$ImpVol, .045, 0, x$t, x$Type)
  out = cbind(x, out)
  out
  
}

add.meta = function(x){
  out = left_join(x, underlying, by = "U")
  out
}

Scaling.Function = function(x){
  out = x |>
    group_by(U, Type) |>
    mutate(LM = if_else(Type == "c", log(S0/Strike), log(Strike/S0)),
           MeanVol = mean(ImpVol, na.rm = TRUE),
           Scaled.LM = if_else(L == "n", LM, 
                               (LM/abs(B) + ((.045 * ( abs(B) - 1) + 0) * (t/365.25))/abs(B) + ((abs(B) -1)/2) * (t/365.25) * MeanVol^2)),
           Scaled.ImpVol = if_else(L == "y", ImpVol/abs(B), ImpVol)) |>
    ungroup()
  out
  
}



##################

path_to_raw = "C:/Users/Charlie/Desktop/R Stuff/Options Pull"
path_to_clean = "C:/Users/Charlie/Desktop/R Stuff/Cleaned_Option_Data"

raw_data = list.files(path_to_raw)

clean_data = list.files(path_to_clean)

need_to_clean = raw_data %in% clean_data

setwd("C:\\Users\\Charlie/Desktop/R Stuff/Options Pull")


for (i in 1:length(need_to_clean)){
  if (need_to_clean[i]){ print("Cleaned")} else {
    
    setwd("C:\\Users\\Charlie/Desktop/R Stuff/Options Pull")
    
    x = read.csv(raw_data[i])
    
    x = x |>
      yahoo.clean.data() |>
      data.step1() |>
      add.Greeks() |>
      add.meta() |>
      Scaling.Function()
    
    setwd("C:\\Users\\Charlie/Desktop/R Stuff/Cleaned_Option_Data")
    
    op = "C:\\Users\\Charlie/Desktop/R Stuff/Cleaned_Option_Data"
    opf = file.path(op, raw_data[i])
    
    write.csv(x, file = opf, row.names = FALSE)
    
  }
  
}

