#p(LT > L0 |ET > E0)

library(tidyverse)

a = function(x){
  
  #x is a vector of c(days, rf, vol) in daily terms
  
  etf = rnorm(x[[1]][1], x[[1]][2]/252 , x[[1]][3]/sqrt(252))
  letf = 3*etf
  setf = -3*etf
  
  y = sd(etf)
  
  etf = cumprod(1+etf)
  letf = cumprod(1+letf)
  setf = cumprod(1+setf)
  
  z = data.frame(etf,letf,setf)
  
  x = as.numeric(tail(z, n=1))
  
  x = c(x,y)
  
  x
}

results = as.data.frame(t(replicate(50000,a(50,0,.01))))

colnames(results) = c("ETF", "LETF", "SETF", "RVol")



x = results |>
  filter(ETF > 1 & LETF < 1)


r = seq(from = 0, to = .1, by = .05)
ti = seq(from = 1, to = 50, by = 5)
vol = seq(from = .01, to = .5, by = .05)

combinations = as.list(apply(as.data.frame(expand.grid(ti,r, vol)), 1, list))

lfun = function(x){
  results = as.data.frame(t(replicate(100,a(x)))) |>
    mutate(ti = x[[1]][1], vol = x[[1]][3], rf = x[[1]][2])
  names(results) = c("ETF", "LETF", "SETF", "RVol", "Days", "AVol", "Rf")
  results
}

comp = lapply(combinations, lfun)

all_data = do.call(rbind, comp)
