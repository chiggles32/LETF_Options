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


#Calendars
load_quantlib_calendars("UnitedStates/NYSE", from = "2023-10-29", to = Sys.Date())

trade_days = bizseq("2023-10-29", Sys.Date() - 70 , cal = "QuantLib/UnitedStates/NYSE")

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
         Intrinsic = if_else(Intrinsic < 0, 0, Intrinsic))

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
  rename("Expiry_Price" = N.Price, "Ending_Price" = S0) |>
  ungroup()
####
#Pricing when IV is positive but bid ask is 0



Shared_Data = Shared_Data |>
  left_join(expiry_prices, by = "Contract.Name")

Missing_Close = Shared_Data |>
  filter(is.na(Ending_Price), Expiry < Sys.Date()) |>
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
  mutate(Ending_Price = if_else(is.na(Ending_Price), Lookup, Ending_Price)) |>
  select(-Lookup)
  
  


  
##########

test1 = test |>
  mutate(Analog = if_else(B < 0, if_else(Type == "p", "c", "p"), Type))

times1 = test1 |>
  pull(QuoteTime) |>
  unique() |>
  sort()

test |>
  group_by(U, QuoteTime) |>
  filter(Price == 0 & Intrinsic != 0) |>
  summarise(Count = n()) |>
  view()
  

gtest = test |>
  filter((Bid != 0 & Ask != 0)) |>
  group_by(U, QuoteTime, Expiry, Strike) |>
  # arrange(Strike, .by_group = T) |>
  # mutate(gvar = cur_group_id()) |>
  distinct() |>
  filter(n() == 2, Expiry < Sys.Date()) 

##how to ensure price is accurate

ctest = gtest |>
  summarize(
    C1 = first(Contract.Name),
    C2 = last(Contract.Name),
    Quoted = as.POSIXct(first(QuoteTime)),
    IV1 = first(Scaled.ImpVol),
    IV2 = last(Scaled.ImpVol),
    LM1 = first(Scaled.LM),
    LM2 = last(Scaled.LM),
    Expiry = first(Expiry),
    ET = first(ET),
    Start_Price = sum(Price),
    End_Price = sum(Expiry_Price),
    Strike = first(Strike),
    S01 = first(S0),
    S0t = first(Ending_Price)
  ) |>
  mutate(End_Price = if_else(is.na(End_Price), abs(S0t - Strike), End_Price),
         ET = if_else(is.na(ET), as.numeric(as.POSIXct(paste0(Expiry, " 16:00:00"))), ET),
         deltat = time_length(difftime(QuoteTime, ET), unit = "day"),
         logret = log(End_Price/Start_Price),
         RP = if_else(U == "QQQ", (Start_Price - abs(Strike-S01))/ Strike, ((Start_Price - abs(Strike-S01))/ Strike) * (1/3)))


###Clean this up then move on to create a way to compare across and profits.
## basic buy strangle comparison + correlation to market.

# creating list of exp/qt
exp.qt = ctest |>
  ungroup() |>
  group_by(Expiry, QuoteTime) |>
  filter(n_distinct(U) != 1) |>
  group_split()

clean.fun = function(x) {
  x |>
    rowwise() |>
    mutate(Moneyness = if_else(Strike > S01, abs(LM1), -abs(LM1)),
           ImpVol = if_else( (is.na(IV1) | is.na(IV2) ), sum(IV1,IV2, na.rm = TRUE) , sum(IV1, IV2) / 2 ) ) |>
    ungroup() |>
    mutate(ID = row_number()) |>
    select(-IV1, -IV2, -LM1, -LM2) 
}

cleaned_exp = lapply(exp.qt, clean.fun)


# data_check = do.call(rbind, cleaned_exp)
# 
# data_check |>
#   group_by(Expiry, QuoteTime, U) |>
#   summarise(n()) |>
#   view()

# 
# sample_df = ctest |>
#   ungroup() |>
#   filter(Expiry == "2023-11-10", QuoteTime == 1698674472) |>
#   rowwise() |>
#   mutate(Moneyness = if_else(Strike > S01, abs(LM1), -abs(LM1)),
#          ImpVol = if_else( (is.na(IV1) | is.na(IV2) ), sum(IV1,IV2, na.rm = TRUE) , sum(IV1, IV2) / 2 ) ) |>
#   ungroup() |>
#   mutate(ID = row_number()) |>
#   select(-IV1, -IV2, -LM1, -LM2) 

match.fun = function(J) {
  X = J |>
    select(U, Moneyness, ID)
  Y = X |>
    expand_grid(Comparison = X) |> 
    filter(U != Comparison$U) |>
    mutate(Difference = abs(Moneyness - Comparison$Moneyness)) |>
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

matched = lapply(cleaned_exp, match.fun)

final = do.call(rbind, matched) |>
  mutate(IV_Dif = abs(ImpVol - ImpVol.1),
         TTE = abs(deltat),
         M_Dif = abs(Moneyness - Moneyness.1),
         Rn = row_number(),
         RP_Dif = abs(RP - RP.1))


final = final |>
  mutate(Simple_Ret = (End_Price - Start_Price)/Start_Price,
         Simple_Ret.1 = (End_Price.1 - Start_Price.1)/Start_Price.1,
         SRet_Dif = abs(Simple_Ret - Simple_Ret.1),
         LongShort = if_else(
           ImpVol > ImpVol.1,
           paste(sub("\\d.*", "", C1), sub("\\d.*", "", C1.1), sep = ">"),
           paste(sub("\\d.*", "", C1.1), sub("\\d.*", "", C1), sep = ">")
         )
         )

# functionize? the above, then create the final dataframe for regression
# fix ret calculation
# why is it showing the opposite of what it should?


tr = final |>
  filter(M_Dif < .001, RP_Dif > 0, abs(Moneyness) > .02)

tr = tr |>
  mutate(LS_Ret = if_else(ImpVol > ImpVol.1,
                          (-Simple_Ret + Simple_Ret.1)/2,
                          (Simple_Ret - Simple_Ret.1)/2))

model = lm(LS_Ret ~ IV_Dif + M_Dif + RP_Dif + TTE + IV_Dif * RP_Dif * TTE , data = tr)
summary(model)
par(mfrow=c(2,2)) # Set up the plotting area to display 4 plots at once
plot(model) 

###Why oh why 

tr |>
  filter(LS_Ret > 0) |>
ggplot( aes(x = IV_Dif, y = LS_Ret, color = LongShort)) +
  geom_point()

new_data = data.frame(IV_Dif = seq(from = .01, to = .5, by = .05))
predict(model, new_data)
