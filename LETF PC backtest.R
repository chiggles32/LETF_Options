####Dependencies#####
library(bizdays)
library(tidyverse)
library(PerformanceAnalytics)
library(quantmod)
library(lubridate)


#### Some general structure####
#   custom functions will be used that have names with "." as a spacer
#   data sets will have the spacer be "_"
# Trying to keep the data structured as a list in order to use lapply for efficiency/ease

####Some Initial Meta Data####

setwd("C:/Users/Charlie/Desktop/R Stuff/Cleaned_Option_Data")

time_str = "12:30:00 PM"

start_date = "2023-10-29"
end_date = Sys.Date() - 50

#Calendars
load_quantlib_calendars("UnitedStates/NYSE", from = "2023-10-29", to = Sys.Date())

trade_days = bizseq(start_date, end_date, cal = "QuantLib/UnitedStates/NYSE")

gathered = file.exists(paste0("Options_Pull_", trade_days))

trade_days = trade_days[gathered]

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
  filter(Expiry < Sys.Date(),
         abs(Scaled.LM) < .03) |>
  mutate(RP = (Price - Intrinsic) / Strike,
         Scaled.RP = if_else(abs(B) > 1, RP/abs(B), RP),
         Intrinsic = if_else(Intrinsic < 0, 0, Intrinsic),
         Midday = as.numeric(as.POSIXct(paste(ODay,time_str), format = "%Y-%m-%d %I:%M:%S %p")))

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
  
  getSymbols(underlying$U[i], from = start_date, to = end_date)
  
}

QQQe = QQQ |>
  Cl() |>
  as.data.frame() |>
  rownames_to_column(var = "Expiry") |>
  filter(Expiry %in% Missing_Close) |>
  mutate(U = "QQQ") |>
  rename("Lookup" = QQQ.Close)

TQQQe = TQQQ |>
  Cl() |>
  as.data.frame() |>
  rownames_to_column(var = "Expiry") |>
  filter(Expiry %in% Missing_Close) |>
  mutate(U = "TQQQ") |>
  rename("Lookup" = TQQQ.Close)

SQQQe = SQQQ |>
  Cl() |>
  as.data.frame() |>
  rownames_to_column(var = "Expiry") |>
  filter(Expiry %in% Missing_Close) |>
  mutate(U = "SQQQ") |>
  rename("Lookup" = SQQQ.Close)

closes = rbind(QQQe, TQQQe, SQQQe)

rm(SQQQe,TQQQe, QQQe)

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

all_calls = do.call(rbind, Matched_Calls) 
all_puts = do.call(rbind, Matched_Puts)



all_data = rbind(all_calls, all_puts) |>
  mutate(Time_Period = paste(ODay, Expiry, sep = ","))

period_dates = all_data |> 
  pull(Time_Period) |>
  unique() |>
  as.list()

period.stats.fun = function(x) {
  d1 = substr(x, 1, 10)
  d2 = substr(x, 12, 22)
  dates = window(QQQ$QQQ.Close, start = d1, end = d2)
  Period_Return = drop(coredata(last(dates)))/drop(coredata(first(dates)))-1
  Period_Vol = drop(coredata(StdDev.annualized(Return.calculate(dates))))
  
  data.frame(x, Period_Return, Period_Vol) |>
    rename("Time_Period" = x)
}

Period_Stats = lapply(period_dates, period.stats.fun) 
Period_Stats = do.call(rbind, Period_Stats) 
rownames(Period_Stats) = NULL
Period_Stats = Period_Stats |>
  mutate(Period_Vol = if_else(is.na(Period_Vol),
                              0,
                              Period_Vol))

all_data = all_data |>
  left_join(Period_Stats, by = "Time_Period")


all_data = all_data |>
  mutate(
    Ave_IV = (Scaled.ImpVol + Scaled.ImpVol.1) / 2,
    IV_RV_Dif = Ave_IV - Period_Vol,
    TTE = abs(deltat),
    M_Dif = if_else(Scaled.ImpVol > Scaled.ImpVol.1,
                    (-Scaled.LM + Scaled.LM.1),
                    (Scaled.LM - Scaled.LM.1)),
    Rn = row_number(),
    RP_Dif = if_else(Scaled.ImpVol > Scaled.ImpVol.1,
                     (-Scaled.RP + Scaled.RP.1),
                     (Scaled.RP - Scaled.RP.1)),
    Simple_Ret = (End.Intrinsic - Price)/Price,
    Simple_Ret.1 = (End.Intrinsic.1 - Price.1)/Price.1,
    SRet_Dif = abs(Simple_Ret - Simple_Ret.1),
    LongShort = as.factor(if_else(
      Scaled.ImpVol < Scaled.ImpVol.1,
      paste(sub("\\d.*", "", Contract.Name), sub("\\d.*", "", Contract.Name.1), sep = "."),
      paste(sub("\\d.*", "", Contract.Name.1), sub("\\d.*", "", Contract.Name), sep = "."))),
    Lambda = delta * (S0 / Price),
    Lambda.1 = delta.1 * (S0.1 / Price.1),
    Adjusted.Theta =  theta * (1/Price),
    Adjusted.Theta.1 = theta.1 * (1/Price.1),
    Adjusted.Gamma = gama * (S0 / delta),
    Adjusted.Gamma.1 = gama.1 * (S0.1 / delta.1),
    Adjusted.Vega = vega * (ImpVol/Price),
    Adjusted.Vega.1 = vega.1 * (ImpVol.1/Price.1),
    LS_Ret = if_else(Scaled.ImpVol > Scaled.ImpVol.1,
                     (-Simple_Ret + Simple_Ret.1)/2,
                     (Simple_Ret - Simple_Ret.1)/2),
    IV_Dif = abs(Scaled.ImpVol-Scaled.ImpVol.1),
    Lambda_Dif = if_else(Scaled.ImpVol > Scaled.ImpVol.1,
                     (-Lambda + Lambda.1),
                     (-Lambda.1 + Lambda)),
    Theta_Dif = if_else(Scaled.ImpVol > Scaled.ImpVol.1,
                         (-Adjusted.Theta + Adjusted.Theta.1),
                         (-Adjusted.Theta.1 + Adjusted.Theta)),
    Gamma_Dif = if_else(Scaled.ImpVol > Scaled.ImpVol.1,
                        (-Adjusted.Gamma + Adjusted.Gamma.1),
                        (-Adjusted.Gamma.1 + Adjusted.Gamma)),
    Vega_Dif = if_else(Scaled.ImpVol > Scaled.ImpVol.1,
                        (-Adjusted.Vega + Adjusted.Vega.1),
                        (-Adjusted.Vega.1 + Adjusted.Vega)),
    LS_Contract = if_else(Scaled.ImpVol > Scaled.ImpVol.1,
                          paste(Contract.Name.1, Contract.Name),
                          paste(Contract.Name, Contract.Name.1)),
    Moneyness = as.factor(if_else(End.Intrinsic > 0 & End.Intrinsic.1 > 0, "Both ITM", ">1 OTM")))

iv = all_data$Scaled.ImpVol.1-all_data$Scaled.ImpVol



contract_summary = all_data |>
  ungroup() |>
  filter(M_Dif < 0.005, abs(Scaled.LM) < 0.025, Analog == "c") |>
  group_by(LS_Contract) |>
  summarise(Count = n())

# hi

final = all_data |>
  group_by(LS_Contract) |>
  slice_min(QuoteTime) |>
  select(LS_Ret,IV_RV_Dif:Vega_Dif, Scaled.LM, Analog, -SRet_Dif, Simple_Ret, Simple_Ret.1, Rn, Period_Return, Moneyness) 

  

lf = final |>
  filter(Analog == "c") |>
  ungroup() |>
  select(-Simple_Ret, -Simple_Ret.1, -Rn, -Lambda:-Adjusted.Vega.1, -Analog, -LS_Contract)
  
   
# Random Forest 

library(caTools)

# Make sure to get rid of all variables that have look ahead bias *wink

cr = lf |>
  filter(M_Dif < .005, abs(Scaled.LM) < .025)



# split = sample.split(cr$LS_Ret, SplitRatio = .5)
# 
# train = cr[!split,]

thold = cr$IV_Dif |>
  sd(na.rm = TRUE)

cr = cr |>
  filter(IV_Dif > 1.5*thold) |>
  ungroup()

cr = as.data.frame(cr)

# 
# test = cr[split, ] |>
#   filter(IV_Dif > 2*thold)

set.seed(100)

#### Interaction FOrest
library(diversityForest)



int.model = interactionfor(dependent.variable.name = "LS_Ret", data = cr, importance = "both", num.trees = 2500 )

model = lm(LS_Ret ~ Period_Return , data = cr)
summary(model)

put_spread = cr |>
  group_by(Moneyness) |>
  summarise(Ave.Ret = mean(LS_Ret),
            Std.Ret = sd(LS_Ret),
            Count = n())

plot(int.model, numpairsquant = 5)

plotEffects(int.model, allwith = "Scaled.LM")

pdt = predict(int.model, data = cr[,-1])

ggplot(data.frame(Predicted = pdt$predictions, Actual = cr$LS_Ret),
       aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Random Forest Predictions on LS Trade Return",
       x = "Actual Values", y = "Predicted Values")

x = data.frame(P = pdt$predictions, A = cr$LS_Ret) |>
  mutate(Quadrant = case_when(P > 0 & A > 0 ~ 1,
                              P > 0 & A < 0 ~ 2,
                              P < 0 & A < 0 ~ 3,
                              P < 0 & A > 0 ~ 4)) |>
  group_by(Quadrant) |>
  summarise(Count = n(),
            Average.P = mean(P),
            Average.A = mean(A),
            StD.P = sd(P),
            StD.A = sd(A))

######################################################################
######################################################################
############################################################################################################################################
######################################################################
############################################################################################################################################
######################################################################
######################################################################
    
 ### Test on unseen expiry####

trade_days = bizseq(Sys.Date() - 49, Sys.Date() , cal = "QuantLib/UnitedStates/NYSE")

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



test1 = test |>
  filter(QuoteTime %in% midday_times) |>
  mutate(Analog = if_else(B < 0, if_else(Type == "p", "c", "p"), Type))

test2 = test1 |>
  filter(!is.na(ImpVol))

gtest = test2 |>
  filter((Bid != 0 & Ask != 0)) |>
  group_by(U, QuoteTime, Expiry, Strike) |>
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

all_calls = do.call(rbind, Matched_Calls) 
all_puts = do.call(rbind, Matched_Puts)



all_data = rbind(all_calls, all_puts) |>
  mutate(
    TTE = abs(deltat),
    M_Dif = abs(Scaled.LM - Scaled.LM.1),
    Rn = row_number(),
    RP_Dif = abs(Scaled.RP - Scaled.RP.1),
    Simple_Ret = (End.Intrinsic - Price)/Price,
    Simple_Ret.1 = (End.Intrinsic.1 - Price.1)/Price.1,
    SRet_Dif = abs(Simple_Ret - Simple_Ret.1),
    LongShort = as.factor(if_else(
      Scaled.ImpVol < Scaled.ImpVol.1,
      paste(sub("\\d.*", "", Contract.Name), sub("\\d.*", "", Contract.Name.1), sep = "."),
      paste(sub("\\d.*", "", Contract.Name.1), sub("\\d.*", "", Contract.Name), sep = "."))),
    Lambda = delta * (S0 / Price),
    Lambda.1 = delta.1 * (S0.1 / Price.1),
    Adjusted.Theta = (1/365) * theta * (1/S0),
    Adjusted.Theta.1 = (1/365) * theta.1 * (1/S0.1),
    Adjusted.Gamma = gama * (S0 / Price),
    Adjusted.Gamma.1 = gama.1 * (S0.1 / Price.1),
    LS_Ret = if_else(Scaled.ImpVol > Scaled.ImpVol.1,
                     (-Simple_Ret + Simple_Ret.1)/2,
                     (Simple_Ret - Simple_Ret.1)/2),
    IV_Dif = abs(Scaled.ImpVol-Scaled.ImpVol.1),
    Lambda_Dif = if_else(Scaled.ImpVol > Scaled.ImpVol.1,
                         (-Lambda + Lambda.1),
                         (-Lambda.1 + Lambda)),
    Theta_Dif = if_else(Scaled.ImpVol > Scaled.ImpVol.1,
                        (-Adjusted.Theta + Adjusted.Theta.1),
                        (-Adjusted.Theta.1 + Adjusted.Theta)),
    Gamma_Dif = if_else(Scaled.ImpVol > Scaled.ImpVol.1,
                        (-Adjusted.Gamma + Adjusted.Gamma.1),
                        (-Adjusted.Gamma.1 + Adjusted.Gamma)),
    LS_Contract = if_else(Scaled.ImpVol > Scaled.ImpVol.1,
                          paste(Contract.Name.1, Contract.Name),
                          paste(Contract.Name, Contract.Name.1))
  )

contract_summary = all_data |>
  ungroup() |>
  filter(M_Dif < 0.005, abs(Scaled.LM) < 0.025, Analog == "c") |>
  group_by(LS_Contract) |>
  summarise(Count = n())

final = all_data |>
  group_by(LS_Contract) |>
  slice_min(QuoteTime) |>
  select(LS_Ret,TTE:Gamma_Dif, Scaled.LM, Analog, -SRet_Dif, Simple_Ret, Simple_Ret.1, Rn) 

lf = final |>
  filter(Analog == "p") |>
  ungroup() |>
  select(-Simple_Ret, -Simple_Ret.1, -Rn, -Lambda:-Adjusted.Gamma.1, -Analog, -LS_Contract)


# Make sure to get rid of all variables that have look ahead bias *wink

cr = lf |>
  filter(M_Dif < .005, abs(Scaled.LM) < .025)



cr = cr |>
  filter(IV_Dif > 2*thold)


set.seed(500)



#### Interaction FOrest


pdt1 = predict(int.model, data = cr[,-1])

ggplot(data.frame(Predicted = pdt1$predictions, Actual = cr$LS_Ret),
       aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Random Forest Predictions on LS Trade Return",
       x = "Actual Values", y = "Predicted Values")

x = data.frame(P = pdt1$predictions, A = cr$LS_Ret) |>
  mutate(Quadrant = case_when(P > 0 & A > 0 ~ 1,
                              P > 0 & A < 0 ~ 2,
                              P < 0 & A < 0 ~ 3,
                              P < 0 & A > 0 ~ 4)) |>
  group_by(Quadrant) |>
  summarise(Count = n(),
            Average.P = mean(P),
            Average.A = mean(A),
            StD.P = sd(P),
            StD.A = sd(A))

######################################################################
######################################################################
######################################################################



#####


  


# functionize? the above, then create the final dataframe for regression
# fix ret calculation
# why is it showing the opposite of what it should?


cr = final |>
  filter(M_Dif < .005, abs(Scaled.LM) < .025, Analog == "c")



thold = cr$IV_Dif |>
  sd(na.rm = TRUE)

cr = cr |>
  filter(IV_Dif > 2.5 * thold)

model = lm(LS_Ret ~ IV_Dif + RP_Dif + TTE , data = cr)
summary(model)
par(mfrow=c(1,1)) # Set up the plotting area to display 4 plots at once
plot(model) 

###Why oh why 

cr |>
  ggplot( aes(x = LS_Ret, y = Gamma_Dif, color = LongShort)) +
  geom_point()

tr |>
  filter(abs(LS_Ret) < .005) |>
  select(Contract.Name, Contract.Name.1, Expiry, QuoteTime, ODay, TTE:IV_Dif, Strike, Strike.1, Price, Price.1, S0, St, S0.1, St.1, End.Intrinsic, End.Intrinsic.1 ) |>
  view()


t.stats = tr |>
  select(LS_Ret, IV_Dif)

new_data = data.frame(IV_Dif = .009, RP_Dif = 0.01, TTE = 50, Scaled.LM = .01)
predict(model, new_data)
