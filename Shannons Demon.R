library(tidyverse)
library(quantmod)
library(PerformanceAnalytics)

rr = function(assets, steps, vol){
  asset_rets =  as.data.frame(replicate(assets, rnorm(steps,0,vol))) 

  
  asset_rets = asset_rets|>
    mutate_all(~ifelse(. < -1, -1, .))
  
  
  wt = 1/assets
  
  wtret = asset_rets*wt
  
  x = rowSums(wtret)
  
  y = tail(cumprod(x+1), n=1)
  y
  
}

r = function(x, assets, steps, vol){
 y = replicate(x,rr(assets, steps, vol))
 hist(y, breaks = 50)
 mean(y)
}