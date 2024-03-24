#p(LT > L0 |ET > E0)

library(tidyverse)

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



# r = seq(from = 0, to = .1, by = .05)
r = 0.025
ti = seq(from = 1, to = 50, by = 1)
vol = seq(from = .1, to = .5, by = .025)

combinations = as.list(apply(as.data.frame(expand.grid(ti,r, vol)), 1, list))

lfun = function(x){
  results = as.data.frame(t(replicate(500,a(x)))) |>
    mutate(ti = x[[1]][1], vol = x[[1]][3], rf = x[[1]][2])
  names(results) = c("ETF", "LETF", "SETF", "RVol", "Days", "AVol", "Rf")
  results
}

comp = lapply(combinations, lfun)

all_data = do.call(rbind, comp) |>
  mutate(ID = row_number())

ed = all_data |>
  pivot_longer(cols = c("ETF", "LETF", "SETF" ), names_to = "underlying", values_to = "EV") |>
  mutate(SM = if_else(underlying == "ETF", 
                      log(EV),
                      (log(EV)/3 + ((Rf * ( 3 - 1) + 0) * (Days/365.25))/3 + ((3 -1)/2) * (Days/365.25) * AVol^2)),
         SM = if_else(underlying == "SETF", -SM, SM))

info = ed |>
  group_by(ID) |>
  filter(row_number() ==1) |>
  select(RVol, Days, AVol, Rf, ID)

comb_df = ed |>
  select(-RVol, -Days, -AVol, -Rf, -EV)

paired_df = comb_df |>
  inner_join(comb_df, by = "ID", relationship = "many-to-many") |>
  filter(underlying.x != underlying.y) |>
  mutate(UPair = paste0(underlying.x,"-", underlying.y)) |>
  rowwise() |>
  mutate(NID = str_flatten(sort(strsplit(UPair, "")[[1]]))) |>
  ungroup() |>
  group_by(ID, NID) |>
  filter(row_number()==1)
         
paired_df |>
  ungroup() |>
  group_by(UPair) |>
  summarise(count = n())

final_df = paired_df |>
  left_join(info, by = "ID") |>
  ungroup() |>
  select(-underlying.x, -underlying.y, -ID, -NID) 
  
comp_df = final_df |>
  mutate(Difs = if_else(sign(SM.x) != sign(SM.y), "1 OTM", "Same M"))


comps = comp_df |>
  group_by(Days,AVol, Difs) |>
  summarize(Obs = n()) |>
  pivot_wider(id_cols = c("Days", "AVol"), names_from = "Difs", values_from = "Obs") |>
  mutate(MMR = `1 OTM`/`Same M`) |>
  select(Days, AVol, MMR) |>
  ungroup()

ggplot(comps, aes(x = Days, y = AVol, z = MMR)) +
  geom_contour_filled() +
  scale_color_viridis_c()

