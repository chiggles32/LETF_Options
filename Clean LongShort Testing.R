####Dependencies#####
library(bizdays)
library(tidyverse)
library(PerformanceAnalytics)
library(quantmod)
library(lubridate)
library(diversityForest)

####Some Initial Meta Data####

setwd("C:/Users/Charlie/Desktop/R Stuff/Cleaned_Option_Data")

time_str = "12:30:00 PM" #Midday quotes

# partitioning test/training data
train_date1 = "2023-10-29"
train_date2 = "2024-03-05"

test_date1 = "2024-03-25"
test_date2 = Sys.Date()

#Calendar for trading days
load_quantlib_calendars("UnitedStates/NYSE", from = "2023-10-29", to = Sys.Date())

train_days = bizseq(train_date1, train_date2, cal = "QuantLib/UnitedStates/NYSE")
gathered = file.exists(paste0("Options_Pull_", train_days))
train_days = train_days[gathered]

test_days = bizseq(test_date1, test_date2, cal = "QuantLib/UnitedStates/NYSE")
gathered = file.exists(paste0("Options_Pull_", test_days))
test_days = test_days[gathered]

#Underlying Assets
underlying = data.frame(c("TQQQ","SQQQ","QQQ"), c("y", "y", "n"), c(3,-3,1))
names(underlying) = c("U","L","B")

index = underlying |>
  filter(L == "n") |>
  pull(U)

####Initial Data Pull####
pull.data.function = function(x,y){
  path = paste0(x,"Options_Pull_", y)
  out = read.csv(path)
  out
}



training_data = lapply(train_days, pull.data.function)

testing_data = lapply(test_days, pull.data.function)

####Manipulating Option Data####

all_training_data = do.call(rbind, training_data)
all_testing_data = do.call(rbind, testing_data)

all_training_data = all_training_data |>
  mutate(GVar = "Train") |>
  filter(Expiry < train_date2,
         abs(Scaled.LM) < .03)
all_testing_data = all_testing_data |>
  mutate(GVar = "Test") |>
  filter(Expiry < test_date2,
         abs(Scaled.LM) < .03)

all_data = rbind(all_training_data, all_testing_data)

rm(all_training_data)
rm(all_testing_data)
rm(testing_data)
rm(training_data)

all_data = all_data |>
  mutate(RP = (Price) / Strike,
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

Shared_Expiry_Data = all_data |>
  filter(Expiry %in% common)

rm(all_data)

expiry_prices = Shared_Expiry_Data |>
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



Shared_Expiry_Data = Shared_Expiry_Data |>
  left_join(expiry_prices, by = "Contract.Name")

Missing_Close = Shared_Expiry_Data |>
  filter(is.na(St), Expiry < Sys.Date()) |>
  pull(Expiry) |>
  unique()

for (i in 1:length(underlying$U)) {
  
  getSymbols(underlying$U[i], from = train_date1, to = test_date2)
  
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

Complete_Data = Shared_Expiry_Data |>
  left_join(closes, by = c("Expiry", "U"))

rm(Shared_Expiry_Data)

Complete_Data = Complete_Data |>
  mutate(St = if_else(is.na(St), Lookup, St)) 

midday_times = Complete_Data |>
  ungroup() |>
  group_by(Expiry, ODay) |>
  select(QuoteTime, Midday) |>
  mutate(Time_Dif = abs(QuoteTime - Midday)) |>
  slice_min(Time_Dif, n = 1) |>
  pull(QuoteTime) |>
  unique()

##########

Raw_Data = Complete_Data |>
  filter(QuoteTime %in% midday_times) |>
  mutate(Analog = if_else(B < 0, if_else(Type == "p", "c", "p"), Type))

rm(Complete_Data)

Filtered_Data = Raw_Data |>
  filter(!is.na(ImpVol),
         (Bid != 0 & Ask != 0),
         !is.na(St),
         Last.Trade.Date == ODay) |>
  group_by(U, QuoteTime, Expiry, Strike) |>
  distinct() |>
  mutate(ET = if_else(is.na(ET), as.numeric(as.POSIXct(paste0(Expiry, " 16:00:00"))), ET),
         deltat = time_length(difftime(QuoteTime, ET), unit = "day"),
         End.Intrinsic = if_else(Type == "c",
                                 max(0, St - Strike),
                                 max(0, Strike - St)))

# creating list of exp/qt
p.c = Filtered_Data |>
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

# Make more interpretable
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


final_all_data = all_data |>
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
    Moneyness = as.factor(if_else(End.Intrinsic > 0 & End.Intrinsic.1 > 0, "Both ITM", ">1 OTM")),
    Vega_Weight = Adjusted.Vega / Adjusted.Vega.1,
    Theta_Weight = Adjusted.Theta / Adjusted.Theta.1,
    Lambda_Weight = Lambda / Lambda.1,
    Gamma_Weight = Adjusted.Gamma / Adjusted.Gamma.1,
    IV_Weight = Scaled.ImpVol / Scaled.ImpVol.1,
    VW_Ret = if_else(Scaled.ImpVol > Scaled.ImpVol.1,
                     (-(Simple_Ret / Vega_Weight) + Simple_Ret.1 ) / (1 + 1 / Vega_Weight),
                     ((Simple_Ret / Vega_Weight) - Simple_Ret.1) / (1 + 1 / Vega_Weight)),
    TW_Ret = if_else(Scaled.ImpVol > Scaled.ImpVol.1,
                     (-(Simple_Ret / Theta_Weight) + Simple_Ret.1) / (1 + 1 / Theta_Weight) ,
                     ((Simple_Ret / Theta_Weight) - Simple_Ret.1) / (1 + 1 / Theta_Weight)),
    LW_Ret = if_else(Scaled.ImpVol > Scaled.ImpVol.1,
                 (-(Simple_Ret / Lambda_Weight) + Simple_Ret.1) / (1 + 1 / Lambda_Weight),
                 ((Simple_Ret / Lambda_Weight) - Simple_Ret.1) / (1 + 1 / Lambda_Weight)),
    GW_Ret = if_else(Scaled.ImpVol > Scaled.ImpVol.1,
                     (-(Simple_Ret / Gamma_Weight) + Simple_Ret.1) / (1 + 1 / Gamma_Weight),
                     ((Simple_Ret / Gamma_Weight) - Simple_Ret.1) / (1 + 1 / Gamma_Weight)),
    IW_Ret = if_else(Scaled.ImpVol > Scaled.ImpVol.1,
                     (-(Simple_Ret / IV_Weight) + Simple_Ret.1) / (1 + 1 / IV_Weight),
                     ((Simple_Ret / IV_Weight) - Simple_Ret.1) / (1 + 1 / IV_Weight))
    
    )

final = final_all_data |>
  group_by(LS_Contract) |>
  # slice_min(QuoteTime) |>
  ungroup() |>
  select(LS_Ret, Scaled.LM, Period_Return:M_Dif, RP_Dif, LongShort, LS_Ret:Moneyness, -LS_Contract, GVar, Analog, Ave_IV, Expiry, VW_Ret, TW_Ret, LW_Ret, GW_Ret, IW_Ret ) |>
  group_by(GVar, Analog) |>
  group_split() 
  
lnames = c()
cleaned = list()

for (i in 1:length(final)){
  x = final[[i]] 
  datatype = first(x$GVar)
  Analog = ifelse(first(x$Analog) == "c", "Calls", "Puts")
  lnames[i] =  paste(datatype, Analog, sep = "_" )
  cleaned[[i]] = x |>
    select(-GVar, -Analog)
  
}

names(cleaned) = lnames

# Make sure to get rid of all variables that have look ahead bias *wink
#TTE_Threshold = 3
M_Dif_Threshold = .005
Scaled.LM_Threshold = .03

# loading in conditional model
setwd("C:/Users/Charlie/Desktop/LETF_Options")
conditional_model = readRDS("Linear_Model_for_prob_of_loss.RDS")


Train_Calls = cleaned$Train_Calls |>
  filter(M_Dif < M_Dif_Threshold, abs(Scaled.LM) < Scaled.LM_Threshold) |>
  select(-Period_Return:-IV_RV_Dif, -Moneyness, Ave_IV, VW_Ret, TW_Ret, LW_Ret, GW_Ret, IW_Ret)

Train_Puts = cleaned$Train_Puts |>
  filter(M_Dif < M_Dif_Threshold, abs(Scaled.LM) < Scaled.LM_Threshold) |>
  select(-Period_Return:-IV_RV_Dif, -Moneyness, Ave_IV, VW_Ret, TW_Ret, LW_Ret, GW_Ret, IW_Ret)

Test_Calls = cleaned$Test_Calls |>
  filter(M_Dif < M_Dif_Threshold, abs(Scaled.LM) < Scaled.LM_Threshold) |>
    select(-Period_Return:-IV_RV_Dif, -Moneyness, Ave_IV, VW_Ret, TW_Ret, LW_Ret, GW_Ret, IW_Ret)

Test_Puts = cleaned$Test_Puts |>
  filter(M_Dif < M_Dif_Threshold, abs(Scaled.LM) < Scaled.LM_Threshold) |>
  select(-Period_Return:-IV_RV_Dif, -Moneyness, Ave_IV, VW_Ret, TW_Ret, LW_Ret, GW_Ret, IW_Ret)


Call_IV_SD = Train_Calls$IV_Dif |>
  sd(na.rm = TRUE)
Put_IV_SD = Train_Puts$IV_Dif |>
  sd(na.rm = TRUE)

Train_Calls = Train_Calls |>
  filter(IV_Dif > 2 * Call_IV_SD) |>
  as.data.frame()
Test_Calls = Test_Calls |>
  filter(IV_Dif > 2 * Call_IV_SD)|>
  as.data.frame()
Train_Puts = Train_Puts |>
  filter(IV_Dif > 2 * Put_IV_SD)|>
  as.data.frame()
Test_Puts = Test_Puts |>
  filter(IV_Dif > 2 * Put_IV_SD)|>
  as.data.frame()

mapping = data.frame("value" = as.factor(c("SETF-ETF", "SETF-LETF", "LETF-ETF", "LETF-SETF", "ETF-SETF", "ETF-LETF")), "TD" = c("SQQQ-QQQ", "SQQQ-TQQQ", "TQQQ-QQQ", "TQQQ-SQQQ", "QQQ-SQQQ", "QQQ-TQQQ"))

train = Train_Calls |>
  select(Scaled.LM, TTE, LongShort, Ave_IV) |>
  mutate(Rf = .045,
         LongShort = as.factor(paste(rev(strsplit(as.character(LongShort), "\\.")[[1]]), collapse = "-"))) |>
  rename("Days" = TTE, "Contract_Moneyness" = Scaled.LM, "TD" = LongShort, "AVol" = Ave_IV ) |>
  left_join(mapping, by = "TD") |>
  select(-TD)
  
Conditional_Moneyness = unname(predict(conditional_model, newdata = train), force = TRUE)

Train_Calls = Train_Calls |>
  cbind(Conditional_Moneyness)

test = Test_Calls |>
  select(Scaled.LM, TTE, LongShort, Ave_IV) |>
  mutate(Rf = .045,
         LongShort = as.factor(paste(rev(strsplit(as.character(LongShort), "\\.")[[1]]), collapse = "-"))) |>
  rename("Days" = TTE, "Contract_Moneyness" = Scaled.LM, "TD" = LongShort, "AVol" = Ave_IV ) |>
  left_join(mapping, by = "TD") |>
  select(-TD)

Conditional_Moneyness = unname(predict(conditional_model, newdata = test), force = TRUE)

Test_Calls = Test_Calls |>
  cbind(Conditional_Moneyness)

set.seed(123)

#### Data graphs

Call.LS_Ret = hist(Train_Calls$LS_Ret, breaks = 20)
Call.Lambda_Dif = hist(Train_Calls$Lambda_Dif, breaks = 20)
Call.Vega_Dif = hist(Train_Calls$Vega_Dif, breaks = 20)
Call.Scaled.LM = hist(Train_Calls$Scaled.LM, breaks = 20)
Call.TTE = hist(Train_Calls$TTE, breaks = 20)
Call.M_Dif = hist(Train_Calls$M_Dif, breaks = 20)
Call.RP_Dif = hist(Train_Calls$RP_Dif, breaks = 20)
Call.Theta_Dif = hist(Train_Calls$Theta_Dif, breaks = 20)
Call.Gamma_Dif = hist(Train_Calls$Gamma_Dif, breaks = 20)
Call.IV_Dif = hist(Train_Calls$IV_Dif, breaks = 20)

Call.LongShort = Train_Calls |>
  group_by(LongShort) |>
  summarise("Number of Trades" = n())


Put.LS_Ret = hist(Train_Puts$LS_Ret, breaks = 20)
Put.Lambda_Dif = hist(Train_Puts$Lambda_Dif, breaks = 20)
Put.Vega_Dif = hist(Train_Puts$Vega_Dif, breaks = 20)
Put.Scaled.LM = hist(Train_Puts$Scaled.LM, breaks = 20)
Put.TTE = hist(Train_Puts$TTE, breaks = 20)
Put.M_Dif = hist(Train_Puts$M_Dif, breaks = 20)
Put.RP_Dif = hist(Train_Puts$RP_Dif, breaks = 20)
Put.Theta_Dif = hist(Train_Puts$Theta_Dif, breaks = 20)
Put.Gamma_Dif = hist(Train_Puts$Gamma_Dif, breaks = 20)
Put.LongShort = plot(Train_Puts$LongShort)
Put.IV_Dif = hist(Train_Puts$IV_Dif, breaks = 20)

Put.LongShort = Train_Puts |>
  group_by(LongShort) |>
  summarise("Number of Trades" = n())


###testing other weightings

Test_Calls = Test_Calls |>
  select(-IW_Ret, -VW_Ret, -GW_Ret, -TW_Ret, -LW_Ret)
Train_Calls = Train_Calls |>
  select(-IW_Ret, -VW_Ret, -GW_Ret, -TW_Ret, -LW_Ret)

#### Interaction FOrest

Call.Model = interactionfor(dependent.variable.name = "LS_Ret", data = Train_Calls, importance = "both", num.trees = 2000 )
Call.Predictions = predict(Call.Model, data = Test_Calls[,-1])

Call.Effects1 = plotEffects(Call.Model, type = "quant", numpairs = 2)
Call.Effects2 = plotEffects(Call.Model, type = "qual", numpairs = 2)


Call.PPlot = ggplot(data.frame(Predicted = Call.Predictions$predictions, Actual = Test_Calls$LS_Ret),
       aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Random Forest Predictions on LS Trade Return",
       x = "Actual Values", y = "Predicted Values")

Call_Stats = data.frame(P = Call.Predictions$predictions, A = Test_Calls$LS_Ret) |>
  mutate(Quadrant = case_when(P > 0 & A >= 0 ~ 1,
                              P > 0 & A < 0 ~ 2,
                              P < 0 & A <= 0 ~ 3,
                              P < 0 & A > 0 ~ 4)) |>
  group_by(Quadrant) |>
  summarise(Count = n(),
            Average.P = mean(P),
            Average.A = mean(A),
            StD.P = sd(P),
            StD.A = sd(A))

Put.Model = interactionfor(dependent.variable.name = "LS_Ret", data = Train_Puts, importance = "both", num.trees = 3000 )
Put.Predictions = predict(Put.Model, data = Test_Puts)

Put.Effects1 = plotEffects(Put.Model, type = "quant", numpairs = 2)
Put.Effects2 = plotEffects(Put.Model, type = "qual", numpairs = 2)

Put.PPlot = ggplot(data.frame(Predicted = Put.Predictions$predictions, Actual = Test_Puts$LS_Ret),
       aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Random Forest Predictions on LS Trade Return",
       x = "Actual Values", y = "Predicted Values")

Put_Stats = data.frame(P = Put.Predictions$predictions, A = Test_Puts$LS_Ret) |>
  mutate(Quadrant = case_when(P > 0 & A >= 0 ~ 1,
                              P > 0 & A < 0 ~ 2,
                              P < 0 & A <= 0 ~ 3,
                              P < 0 & A > 0 ~ 4)) |>
  group_by(Quadrant) |>
  summarise(Count = n(),
            Average.P = mean(P),
            Average.A = mean(A),
            StD.P = sd(P),
            StD.A = sd(A))

#### Post Mortem Results

Train_Calls1 = cleaned$Train_Calls |>
  filter(M_Dif < M_Dif_Threshold, abs(Scaled.LM) < Scaled.LM_Threshold) |>
  select(-IV_RV_Dif, -Moneyness)

Train_Puts1 = cleaned$Train_Puts |>
  filter(M_Dif < M_Dif_Threshold, abs(Scaled.LM) < Scaled.LM_Threshold) |>
  select(-IV_RV_Dif, -Moneyness)

Train_Calls1 = Train_Calls1 |>
  filter(IV_Dif > 2 * Call_IV_SD) |>
  as.data.frame()
Train_Puts1 = Train_Puts1 |>
  filter(IV_Dif > 2 * Put_IV_SD)|>
  as.data.frame()

post.calls = interactionfor(dependent.variable.name = "LS_Ret", data = Train_Calls1, importance = "both", num.trees = 2000 )

p.c.e = plotEffects(post.calls, type = "quant", numpairs = 2)

post.puts = interactionfor(dependent.variable.name = "LS_Ret", data = Train_Puts1, importance = "both", num.trees = 2000 )


p.p.e = plotEffects(post.puts, type = "quant", numpairs = 2)


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




###########################

Train_Calls = cleaned$Train_Calls 

Train_Puts = cleaned$Train_Puts 

Test_Calls = cleaned$Test_Calls 

Test_Puts = cleaned$Test_Puts 

# 
# Call_IV_SD = Train_Calls$IV_Dif |>
#   sd(na.rm = TRUE)
# Put_IV_SD = Train_Puts$IV_Dif |>
#   sd(na.rm = TRUE)

Train_Calls = Train_Calls |>
  # filter(IV_Dif > 2 * Call_IV_SD) |>
  as.data.frame()
Test_Calls = Test_Calls |>
  # filter(IV_Dif > 2 * Call_IV_SD)|>
  as.data.frame()
Train_Puts = Train_Puts |>
  # filter(IV_Dif > 2 * Put_IV_SD)|>
  as.data.frame()
Test_Puts = Test_Puts |>
  # filter(IV_Dif > 2 * Put_IV_SD)|>
  as.data.frame()

mapping = data.frame("value" = as.factor(c("SETF-ETF", "SETF-LETF", "LETF-ETF", "LETF-SETF", "ETF-SETF", "ETF-LETF")), "TD" = c("SQQQ-QQQ", "SQQQ-TQQQ", "TQQQ-QQQ", "TQQQ-SQQQ", "QQQ-SQQQ", "QQQ-TQQQ"))

train = Train_Calls |>
  select(Scaled.LM, TTE, LongShort, Ave_IV) |>
  mutate(Rf = .045,
         LongShort = as.factor(paste(rev(strsplit(as.character(LongShort), "\\.")[[1]]), collapse = "-"))) |>
  rename("Days" = TTE, "Contract_Moneyness" = Scaled.LM, "TD" = LongShort, "AVol" = Ave_IV ) |>
  left_join(mapping, by = "TD") |>
  select(-TD)

Conditional_Moneyness = unname(predict(conditional_model, newdata = train), force = TRUE)

Train_Calls = Train_Calls |>
  cbind(Conditional_Moneyness)

test = Test_Calls |>
  select(Scaled.LM, TTE, LongShort, Ave_IV) |>
  mutate(Rf = .045,
         LongShort = as.factor(paste(rev(strsplit(as.character(LongShort), "\\.")[[1]]), collapse = "-"))) |>
  rename("Days" = TTE, "Contract_Moneyness" = Scaled.LM, "TD" = LongShort, "AVol" = Ave_IV ) |>
  left_join(mapping, by = "TD") |>
  select(-TD)

Conditional_Moneyness = unname(predict(conditional_model, newdata = test), force = TRUE)

Test_Calls = Test_Calls |>
  cbind(Conditional_Moneyness)

set.seed(123)

#### Data graphs

Call.LS_Ret = hist(Train_Calls$LS_Ret, breaks = 20)
Call.Lambda_Dif = hist(Train_Calls$Lambda_Dif, breaks = 20)
Call.Vega_Dif = hist(Train_Calls$Vega_Dif, breaks = 20)
Call.Scaled.LM = hist(Train_Calls$Scaled.LM, breaks = 20)
Call.TTE = hist(Train_Calls$TTE, breaks = 20)
Call.M_Dif = hist(Train_Calls$M_Dif, breaks = 20)
Call.RP_Dif = hist(Train_Calls$RP_Dif, breaks = 20)
Call.Theta_Dif = hist(Train_Calls$Theta_Dif, breaks = 20)
Call.Gamma_Dif = hist(Train_Calls$Gamma_Dif, breaks = 20)
Call.IV_Dif = hist(Train_Calls$IV_Dif, breaks = 20)

Call.LongShort = Train_Calls |>
  group_by(LongShort) |>
  summarise("Number of Trades" = n())


Put.LS_Ret = hist(Train_Puts$LS_Ret, breaks = 20)
Put.Lambda_Dif = hist(Train_Puts$Lambda_Dif, breaks = 20)
Put.Vega_Dif = hist(Train_Puts$Vega_Dif, breaks = 20)
Put.Scaled.LM = hist(Train_Puts$Scaled.LM, breaks = 20)
Put.TTE = hist(Train_Puts$TTE, breaks = 20)
Put.M_Dif = hist(Train_Puts$M_Dif, breaks = 20)
Put.RP_Dif = hist(Train_Puts$RP_Dif, breaks = 20)
Put.Theta_Dif = hist(Train_Puts$Theta_Dif, breaks = 20)
Put.Gamma_Dif = hist(Train_Puts$Gamma_Dif, breaks = 20)
Put.LongShort = plot(Train_Puts$LongShort)
Put.IV_Dif = hist(Train_Puts$IV_Dif, breaks = 20)

Put.LongShort = Train_Puts |>
  group_by(LongShort) |>
  summarise("Number of Trades" = n())

#### Interaction FOrest

Call.Model = interactionfor(dependent.variable.name = "LS_Ret", data = Train_Calls, importance = "both", num.trees = 2000 )
Call.Predictions = predict(Call.Model, data = Test_Calls[,-1])
