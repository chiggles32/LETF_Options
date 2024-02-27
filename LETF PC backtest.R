####Dependencies#####
library(bizdays)
library(tidyverse)
#library(PerformanceAnalytics)
library(quantmod)
library(lubridate)


#### Some general structure####
#   custom functions will be used that have names with "." as a spacer
#   data sets will have the spacer be "_"
# Trying to keep the data structured as a list in order to use lapply for efficiency/ease

####Some Initial Meta Data####

setwd("C:/Users/Charlie/Desktop/R Stuff/Cleaned_Option_Data")

time_str = "12:30:00 PM"


#Calendars
load_quantlib_calendars("UnitedStates/NYSE", from = "2023-10-29", to = Sys.Date())

trade_days = bizseq("2023-10-29", Sys.Date() - 80 , cal = "QuantLib/UnitedStates/NYSE")

gathered = file.exists(paste0("Options_Pull_", trade_days))

trade_days = trade_days[gathered]

# trade_days = list("2024-02-01", "2024-02-01")

#Underlying Assets
underlying = data.frame(c("TQQQ","SQQQ","QQQ"), c("y", "y", "n"), c(3,-3,1))
names(underlying) = c("U","L","B")

index = underlying |>
  filter(L == "n") |>
  pull(U)

####Custom Functions####

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

####Initial Data Pull####


pull.data.function = function(x){
  path = paste0("Options_Pull_", x)
  out = read.csv(path)
  out
}

all_data = lapply(trade_days, pull.data.function)

####Manipulating Option Data####


all_data = do.call(rbind, all_data)

all_data = all_data |>
  mutate(RP = (Price - Intrinsic) / Strike,
         Scaled.RP = if_else(abs(B) > 1, RP/abs(B), RP),
         Intrinsic = if_else(Intrinsic < 0, 0, Intrinsic),
         Midday = as.numeric(as.POSIXct(paste(ODay,time_str), format = "%Y-%m-%d %I:%M:%S %p"))) |>
  filter(Expiry < Sys.Date())

Tex = all_data |>
  filter(U == "TQQQ") |>
  pull(Expiry) |>
  unique()

Sex = all_data |>
  filter(U == "SQQQ") |>
  pull(Expiry) |>
  unique()

common = intersect(Tex, Sex)

Shared_Data = all_data |>
  filter(Expiry %in% common)

rm(all_data)

expiry_prices = Shared_Data |>
  filter(ODay == Expiry) |>
  mutate(
    ET = as.numeric(as.POSIXct(paste0(Expiry, " 15:00:00"), tz = "America/New_York")),
    TD = abs(ET - QuoteTime),
    TC = as.POSIXct(QuoteTime)
  ) |>
  group_by(Contract.Name) |>
  slice(which.min(TD)) |>
  select(Contract.Name, N.Price, ET, S0) |>
  rename("End_MidPrice" = N.Price, "St" = S0) |>
  ungroup()
####
#Pricing when IV is positive but bid ask is 0



Shared_Data = Shared_Data |>
  left_join(expiry_prices, by = "Contract.Name")

Missing_Close = Shared_Data |>
  filter(is.na(St), Expiry < Sys.Date()) |>
  pull(Expiry) |>
  unique()

for (i in 1:length(underlying$U)) {
  
  getSymbols(underlying$U[i], from = Missing_Close[1], to = Missing_Close[length(Missing_Close)])
  
}

QQQ = QQQ |>
  Cl() |>
  as.data.frame() |>
  rownames_to_column(var = "Expiry") |>
  filter(Expiry %in% Missing_Close) |>
  mutate(U = "QQQ") |>
  rename("Lookup" = QQQ.Close)

TQQQ = TQQQ |>
  Cl() |>
  as.data.frame() |>
  rownames_to_column(var = "Expiry") |>
  filter(Expiry %in% Missing_Close) |>
  mutate(U = "TQQQ") |>
  rename("Lookup" = TQQQ.Close)

SQQQ = SQQQ |>
  Cl() |>
  as.data.frame() |>
  rownames_to_column(var = "Expiry") |>
  filter(Expiry %in% Missing_Close) |>
  mutate(U = "SQQQ") |>
  rename("Lookup" = SQQQ.Close)

closes = rbind(QQQ, TQQQ, SQQQ)

rm(SQQQ,TQQQ, QQQ)

test = Shared_Data |>
  left_join(closes, by = c("Expiry", "U"))

test = test |>
  mutate(St = if_else(is.na(St), Lookup, St)) 

midday_times = test |>
  ungroup() |>
  group_by(Expiry, ODay) |>
  select(QuoteTime, Midday) |>
  mutate(Time_Dif = abs(QuoteTime - Midday)) |>
  slice_min(Time_Dif, n = 1) |>
  pull(QuoteTime) |>
  unique()
  



##########

test1 = test |>
  filter(QuoteTime %in% midday_times) |>
  mutate(Analog = if_else(B < 0, if_else(Type == "p", "c", "p"), Type))

# times1 = test1 |>
#   pull(QuoteTime) |>
#   unique() |>
#   sort()

# test |>
#   group_by(U, QuoteTime) |>
#   filter(Price == 0 & Intrinsic != 0) |>
#   summarise(Count = n()) |>
#   view()

test2 = test1 |>
  filter(!is.na(ImpVol))

gtest = test2 |>
  filter((Bid != 0 & Ask != 0)) |>
  group_by(U, QuoteTime, Expiry, Strike) |>
  # arrange(Strike, .by_group = T) |>
  # mutate(gvar = cur_group_id()) |>
  distinct() |>
  filter(Expiry < Sys.Date()) 

##how to ensure price is accurate

ctest = gtest |>
  filter(!is.na(St), Last.Trade.Date == ODay) |>
  mutate(ET = if_else(is.na(ET), as.numeric(as.POSIXct(paste0(Expiry, " 16:00:00"))), ET),
         deltat = time_length(difftime(QuoteTime, ET), unit = "day"),
         End.Intrinsic = if_else(Type == "c",
                                 max(0, St - Strike),
                                 max(0, Strike - St)))

  
  

###Clean this up then move on to create a way to compare across and profits.
## basic buy strangle comparison + correlation to market.

# creating list of exp/qt
p.c = ctest |>
  ungroup() |>
  group_by(Analog) |>
  group_split()

group.exp.qt = function(x){
  x |>
    ungroup() |>
    group_by(Expiry, QuoteTime) |>
    filter(n_distinct(U) != 1) |>
    mutate(ID = row_number()) |>
    group_split()
}

p.c.x = lapply(p.c, group.exp.qt)

Calls = p.c.x[[1]]
Puts = p.c.x[[2]]

  



match.fun = function(J) {
  X = J |>
    select(U, Scaled.LM, ID)
  Y = X |>
    expand_grid(Comparison = X) |> 
    filter(U != Comparison$U) |>
    mutate(Difference = abs(Scaled.LM - Comparison$Scaled.LM)) |>
    group_by(ID, Comparison_U = Comparison$U) |>
    slice_min(Difference, n = 1) |>
    unnest_wider(col = Comparison, names_repair = "unique") |>
    select(ID = ID...3, Comparison_U, Comparison_ID = ID...6, Difference) |>
    pivot_wider(names_from = Comparison_U, values_from = Comparison_ID) |>
    ungroup() 
  
  Z = Y |>
    select(-Difference)
  
  row.f = function(r){
    as.vector(na.omit(as.numeric(r)))
  }
  
  Z = t(apply(Z,1,row.f))
  
  Z.s = t(apply(Z,1,sort)) |>
    as.data.frame() |>
    duplicated()
  
  
  Y = Y |>
    filter(!Z.s) 
  Y = Y |>
    pivot_longer(cols = 3:dim(Y)[2], values_drop_na = TRUE) 
  
  # Need to either do this calculation as a grouped df or map with multiple data frames
  
  Q = map2_df(Y$ID, Y$value, ~ {
    row1 = J[.x, , drop = FALSE]
    row2 = J[.y, , drop = FALSE]
    
    crow = cbind(row1, row2)
    
    colnames(crow) = make.unique(c(colnames(row1), colnames(row2)))
    
    return(crow)
  })
  
}  

Matched_Calls = lapply(Calls, match.fun)
Matched_Puts = lapply(Puts, match.fun)

final = do.call(rbind, Matched_Calls) |>
  mutate(TTE = abs(deltat),
         M_Dif = abs(Scaled.LM - Scaled.LM.1),
         Rn = row_number(),
         RP_Dif = abs(Scaled.RP - Scaled.RP.1))


final = final |>
  mutate(Simple_Ret = (End.Intrinsic - Price)/Price,
         Simple_Ret.1 = (End.Intrinsic.1 - Price.1)/Price.1,
         SRet_Dif = abs(Simple_Ret - Simple_Ret.1),
         LongShort = if_else(
           Scaled.ImpVol > Scaled.ImpVol.1,
           paste(sub("\\d.*", "", Contract.Name), sub("\\d.*", "", Contract.Name.1), sep = ">"),
           paste(sub("\\d.*", "", Contract.Name.1), sub("\\d.*", "", Contract.Name), sep = ">")
         )
  )

# functionize? the above, then create the final dataframe for regression
# fix ret calculation
# why is it showing the opposite of what it should?


tr = final |>
  filter(M_Dif < .005, abs(Scaled.LM) < .01)

tr = tr |>
  mutate(LS_Ret = if_else(Scaled.ImpVol > Scaled.ImpVol.1,
                          (-Simple_Ret + Simple_Ret.1)/2,
                          (Simple_Ret - Simple_Ret.1)/2),
         IV_Dif = abs(Scaled.ImpVol-Scaled.ImpVol.1)
                                 )

thold = tr$IV_Dif |>
  sd(na.rm = TRUE)

tr = tr |>
  filter(IV_Dif > thold,
         End.Intrinsic != 0 & End.Intrinsic.1 != 0)

model = lm(LS_Ret ~ IV_Dif + RP_Dif + IV_Dif*RP_Dif*M_Dif*Scaled.LM*TTE , data = tr)
summary(model)
par(mfrow=c(2,2)) # Set up the plotting area to display 4 plots at once
plot(model) 

###Why oh why 

tr |>
  ggplot( aes(x = IV_Dif, y = LS_Ret, color = LongShort)) +
  geom_point()

tr |>
  filter(abs(LS_Ret) < .005) |>
  select(Contract.Name, Contract.Name.1, Expiry, QuoteTime, ODay, TTE:IV_Dif, Strike, Strike.1, Price, Price.1, S0, St, S0.1, St.1, End.Intrinsic, End.Intrinsic.1 ) |>
  view()


t.stats = tr |>
  select(LS_Ret, IV_Dif)

new_data = data.frame(IV_Dif = seq(from = .01, to = .5, by = .05))
predict(model, new_data)
