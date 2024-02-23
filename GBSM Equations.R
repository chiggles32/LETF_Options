library(RQuantLib)
library(tidyverse)
library(akima)
library(pracma)

###GBSM Model

GBSM.Price = function(spot, strike, volatility, riskfree, yield, dte, type){
  
  t = dte/365.25
  
  d1 = (1 / (volatility * sqrt(t))) * (log(spot/strike) + (riskfree - yield + (.5 * volatility^2)) * t)
  d2 = d1 - (volatility * sqrt(t))
  
  price = ifelse(type == "call",
                 (spot * exp(-yield * t) * pnorm(d1)) - (strike * exp(-riskfree * t) * pnorm(d2)),
                 (strike * exp(-riskfree * t) * pnorm(-d2)) - (spot * exp(-yield * t) * pnorm(-d1))
                 )
  price
}

GBSM.Greeks = function(spot, strike, volatility, riskfree, yield, dte, type){
  
  t = dte/365.25
  
  d1 = (1 / (volatility * sqrt(t))) * (log(spot/strike) + (riskfree - yield + (.5 * volatility^2)) * t)
  d2 = d1 - (volatility * sqrt(t))
  
  price = ifelse(type == "call",
                 (spot * exp(-yield * t) * pnorm(d1)) - (strike * exp(-riskfree * t) * pnorm(d2)),
                 (strike * exp(-riskfree * t) * pnorm(-d2)) - (spot * exp(-yield * t) * pnorm(-d1))
  )
  
  delta = ifelse(type == "call",
                 exp(-yield*t) * pnorm(d1),
                 exp(-yield*t) * (pnorm(d1)-1)
  )
  
  gama = (dnorm(d1) * exp(-yield*t)) / (spot * volatility * sqrt(t))
  
  theta = ifelse(type == "call",
                 ((-spot * dnorm(d1) * volatility * exp(-yield * t)) / (2 *sqrt(t))) + (yield * spot * pnorm(d1) * exp(-yield*t)) - (riskfree * strike * exp(-riskfree * t) * pnorm(d2)),
                 ((-spot * dnorm(d1) * volatility * exp(-yield * t)) / (2 *sqrt(t))) - (yield * spot * pnorm(-d1) * exp(-yield*t)) + (riskfree * strike * exp(-riskfree * t) * pnorm(-d2))
  )
  vega = spot * sqrt(t) * dnorm(d1) * exp(-yield * t)
  
  rho = ifelse(type == "call",
               strike * t * exp(-riskfree * t) * pnorm(d2),
               -strike * t * exp(-riskfree * t) * pnorm(-d2)
  )
  results = cbind(price, delta, gama, theta, vega, rho)
  
  results
  
  
}

#####Figure out why this doesn't work####

error = function(price, spot, strike, volatility, riskfree, yield, dte, type){
  abs(price - GBSM.Price(spot, strike, volatility, riskfree, yield, dte, type))
}

Implied.Volatility = function(price, spot, strike, riskfree, yield, dte, type){
  IV = optim(par = 1, fn = error, lower = .01, upper = 3, price = price, spot = spot, strike = strike, riskfree = riskfree, yield = yield, dte = dte, type = type, method = "L-BFGS-B")
  IV$par
}
#####

setwd("C:\\Users\\Charlie/Desktop/R Stuff/Options Pull")

pull_date = "2023-11-13"
time_str = "12:30:00 PM"

datetime = paste(pull_date,time_str)

datetime = as.numeric(as.POSIXct(datetime, format = "%Y-%m-%d %I:%M:%S %p"))

path = paste0("Options_Pull_", pull_date)

td = read.csv(path)

qt = td$QuoteTime[which.min(abs(td$QuoteTime - datetime))]

td = td |>
  filter(QuoteTime == qt)

td = td |>
  select(-Change, -X..Change) |>
  mutate(across(c(Strike:Open.Interest, TQQQ:HVol), ~as.numeric(gsub('[-,%+]', "", .x)))) |>
  mutate(across(Last.Price:Ask, ~ifelse(is.na(.x), 0, .x))) |>
  na.omit() |>
  distinct()

td = td |>
  mutate(Type = ifelse(grepl("P", Contract.Name), "p","c"), S0 = ifelse(grepl("S", Contract.Name), SQQQ, ifelse(grepl("T", Contract.Name), TQQQ, QQQ)),
         Expiry = as_date(substr(Contract.Name, str_length(Contract.Name)-14, str_length(Contract.Name)-9)), Price = rowMeans(td[,5:6]), U = ifelse(grepl("S", Contract.Name), "SQQQ", ifelse(grepl("T", Contract.Name), "TQQQ", "QQQ")), ODay = as_date(as.POSIXct.numeric(QuoteTime)),
         t = as.numeric(Expiry - ODay), Implied.Volatility = as.numeric(gsub("%","", Implied.Volatility))/100, Last.Trade.Date = as_date(substr(Last.Trade.Date,1,10)) ) |>
  filter(ODay == Last.Trade.Date) |>
  select(Type, Strike, S0, Price, U, t, ODay, QuoteTime, Implied.Volatility)

od = td |>
  filter(U == "QQQ", Type == "c")

od |>
  group_by(t) |>
  summarise(n())

od = od |>
  filter(t == 8, Implied.Volatility != 0) |>
  arrange(Strike)

test = od |>
  select(Strike, Implied.Volatility)
  
vmin = test |>
  filter(Strike == min(Strike)) |>
  pull(Implied.Volatility)

vmax = test |>
  filter(Strike == max(Strike)) |>
  pull(Implied.Volatility)

smax = max(test$Strike)
smin = min(test$Strike)

xstep = round((smax-smin)/dim(test)[1])

smaxs = seq(from = smax, by = xstep, length.out = 10)
smins = seq(from = smin, by = -xstep, length.out = 10)

top = cbind(smaxs, vmax)
bot = cbind(smins, vmin)
cn = c("Strike", "Implied.Volatility")
colnames(top) = cn
colnames(bot) = cn

test = rbind(test, top, bot) |>
  arrange(Strike)



####taking yahoo finance imp vol as correct for now, search up uniroot for solving IV issue or quantlib


s = smooth.spline(test$Strike, test$Implied.Volatility, w = NULL, df = 10)

plot(test$Strike,test$Implied.Volatility)
lines(s)

strike = seq(from = smin, to = smax, length.out = 1000)
iv = as.vector(predict(s,strike)$y)

vg = Vectorize(GBSM.Price)

d = vg(373.9001, strike, iv, .04, .004, 8, "call")

####Numerical Differentiation

delt = (strike-lag(strike))[2]

implied.prob = function(delt, X, C, rf, t){
  
    x = exp(rf*t/365.25)
    
    y = 1/delt
    z = (C + lead(C,1))/2 - (C + lag(C,1))/2
    CDF = 1 + x * y * z
    
    y2 = 1/(delt^2)
    z2 = lead(C,1) + lag(C,1) - 2 * C
    PDF = x*y2*z2
    
    distr = cbind(X,PDF,CDF)
    
    distr
    
}

prob.select = function(distr, val){
  index = which.min(abs(distr[,1] - val))
  print(distr[index,])
}


