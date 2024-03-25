#p(LT > L0 |ET > E0)

library(tidyverse)
library(diversityForest)

# Data functions
ret.gen = function(x){
  
  #x is a vector of c(days, rf, vol) in annual terms
  
  etf = rnorm(x[1], x[2]/252 , x[3]/sqrt(252))
  letf = 3*etf
  setf = -3*etf
  
  etf = cumprod(1+etf)
  letf = cumprod(1+letf)
  setf = cumprod(1+setf)
  
  z = data.frame(etf,letf,setf)
  
  x = as.numeric(tail(z, n=1))
  
  x
}

data.gen = function(x){
  results = as.data.frame(t(replicate(750, ret.gen(x)))) |>
    mutate(Trading_Days = x[1], Annual_Vol = x[3], Rf = x[2])
  names(results) = c("ETF", "LETF", "SETF", "Days", "AVol", "Rf")
  results
}

# Parameter Creation
Rf = seq(from = 0, to = .1, by = .025)
Trading_Days = seq(from = 1, to = 50, by = 2)
Annual_Vol = seq(from = .1, to = .75, by = .025)

Parameters = as.list(apply(as.data.frame(expand.grid(Trading_Days,Rf, Annual_Vol)), 1, list))

unname.fun = function(x){
  x = unname(x[[1]], force = TRUE)
  x
}
Parameters = lapply(Parameters, unname.fun)

#Data creation

raw_data = lapply(Parameters, data.gen)

all_data = do.call(rbind, raw_data) |>
  mutate(ID = row_number())

rm(raw_data)

adjusted = all_data |>
  pivot_longer(cols = c("ETF", "LETF", "SETF" ), names_to = "underlying", values_to = "EV") |>
  mutate(SM = if_else(underlying == "ETF", 
                      log(EV),
                      (log(EV)/3 + ((Rf * ( 3 - 1) + 0) * (Days/252))/3 + ((3 -1)/2) * (Days/252) * AVol^2)),
         SM = if_else(underlying == "SETF", -SM, SM),
         SM = if_else(Days == 1,
                      EV,
                      SM))
rm(all_data)

meta = adjusted |>
  group_by(ID) |>
  filter(row_number() ==1) |>
  select(Days, AVol, Rf, ID)

pairing = adjusted |>
  select(-Days, -AVol, -Rf, -EV)

paired_df = pairing |>
  inner_join(pairing, by = "ID", relationship = "many-to-many") |>
  filter(underlying.x != underlying.y) |>
  mutate(UPair = paste0(underlying.x,"-", underlying.y)) |>
  rowwise() |>
  mutate(NID = str_flatten(sort(strsplit(UPair, "")[[1]]))) |>
  ungroup() |>
  group_by(ID, NID) |>
  filter(row_number()==1) |>
  ungroup()

rm(pairing)

paired_df = paired_df |>
  mutate(`0MC` = if_else(sign(SM.x - 0) > sign(SM.y - 0),
                        paste0(underlying.x, "-",underlying.y),
                        if_else(sign(SM.x - 0) < sign(SM.y - 0),
                                paste0(underlying.y, "-",underlying.x),
                                "Same")),
         `.01MC` = if_else(sign(SM.x - .01) > sign(SM.y - .01),
                           paste0(underlying.x, "-",underlying.y),
                           if_else(sign(SM.x - .01) < sign(SM.y - .01),
                                   paste0(underlying.y, "-",underlying.x),
                                   "Same")),
         `.02MC` = if_else(sign(SM.x - .02) > sign(SM.y - .02),
                           paste0(underlying.x, "-",underlying.y),
                           if_else(sign(SM.x - .02) < sign(SM.y - .02),
                                   paste0(underlying.y, "-",underlying.x),
                                   "Same")),
         `.03MC` = if_else(sign(SM.x - .03) > sign(SM.y - .03),
                           paste0(underlying.x, "-",underlying.y),
                           if_else(sign(SM.x - .03) < sign(SM.y - .03),
                                   paste0(underlying.y, "-",underlying.x),
                                   "Same")),
         `.04MC` = if_else(sign(SM.x - .04) > sign(SM.y - .04),
                           paste0(underlying.x, "-",underlying.y),
                           if_else(sign(SM.x - .04) < sign(SM.y - .04),
                                   paste0(underlying.y, "-",underlying.x),
                                   "Same")),
         `-.01MC` = if_else(sign(SM.x + .01) > sign(SM.y + .01),
                           paste0(underlying.x, "-",underlying.y),
                           if_else(sign(SM.x + .01) < sign(SM.y + .01),
                                   paste0(underlying.y, "-",underlying.x),
                                   "Same")),
         `-.02MC` = if_else(sign(SM.x + .02) > sign(SM.y + .02),
                           paste0(underlying.x, "-",underlying.y),
                           if_else(sign(SM.x + .02) < sign(SM.y + .02),
                                   paste0(underlying.y, "-",underlying.x),
                                   "Same")),
         `-.03MC` = if_else(sign(SM.x + .03) > sign(SM.y + .03),
                           paste0(underlying.x, "-",underlying.y),
                           if_else(sign(SM.x + .03) < sign(SM.y + .03),
                                   paste0(underlying.y, "-",underlying.x),
                                   "Same")),
         `-.04MC` = if_else(sign(SM.x + .04) > sign(SM.y + .04),
                           paste0(underlying.x, "-",underlying.y),
                           if_else(sign(SM.x + .04) < sign(SM.y + .04),
                                   paste0(underlying.y, "-",underlying.x),
                                   "Same")))
         
long_df = paired_df |>
  pivot_longer(cols = `0MC`:`-.04MC`, names_to = "Contract_Moneyness") |>
  mutate(Contract_Moneyness = as.numeric(gsub("MC", "", Contract_Moneyness)))

rm(paired_df)

long_df |>
  ungroup() |>
  group_by(UPair) |>
  summarise(count = n())

final_df = long_df |>
  left_join(meta, by = "ID") |>
  ungroup() |>
  select(-underlying.x, -underlying.y, -ID, -NID) 

comps = final_df |>
  group_by(Days,AVol, Rf, Contract_Moneyness, value) |>
  summarize(Obs = n()) |>
  ungroup() |>
  group_by(Days, AVol, Rf, Contract_Moneyness) |>
  mutate(MMR = Obs/sum(Obs))|>
  filter(value != "Same") |>
  select(-Obs)

P_Out_And_In = interactionfor(dependent.variable.name = "MMR", data = comps, importance = "both", num.trees = 2000)


ggplot(comps, aes(x = Days, y = AVol, z = MMR)) +
  geom_contour_filled() +
  scale_color_viridis_c()

