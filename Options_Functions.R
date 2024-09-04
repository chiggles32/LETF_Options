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

#LM, scaled LM, MeanVol (IV)
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

error2 = function(price, spot, strike, volatility, riskfree, yield, dte, type){
  x = price - GBSM.Price(spot, strike, volatility, riskfree, yield, dte, type)
  x
}

Implied.RiskFree = function(price, spot, strike, volatility, yield, dte, type) {
  x = tryCatch({
    uniroot(
      f = error2,
      interval = c(-.05,1),
      price = price,
      spot = spot,
      strike = strike,
      volatility = volatility,
      yield = yield,
      dte = dte,
      type = type)}, 
    error = function(err) {
      cat("Error: ", conditionMessage(err), "\n")
      return(NA)
    })
  x[[1]]
}

IRF = Vectorize(Implied.RiskFree)