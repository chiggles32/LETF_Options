# Quantifying Volatility Decay Numerically for Adjusting Moneyness

library(tidyverse)
library(quantmod)

# Getting Index Data

from_date = "2005-01-01"

getSymbols("QQQ", from = from_date)

QQQ = QQQ$QQQ.Adjusted |>
  Delt(type = "log") |>
  coredata() |>
  as.vector() |>
  na.omit()

mu = mean(QQQ)
sigma = sd(QQQ)

hist(QQQ, breaks = 40, probability = TRUE)
curve(dnorm(x, mean = mu, sd = sigma), add = TRUE)

time_horizons = as.list(seq(from = 2, to = 252, by = 3))

# By bootstrapping we get a better approximation than the normal curve, but still not great as we have no autocorrelation

cumprod_function = function(x){
  
  x = x + 1
  
  apply(x, 2, cumprod)
  
}

sample_function = function(x){
  
  sample_size = 15000
  
  replicate(sample_size, sample(QQQ, x, replace = TRUE))
}

index_path = lapply(time_horizons, sample_function)
LETF_plus = lapply(index_path, "*", 3)
LETF_minus = lapply(index_path, "*", -3)

index_path = lapply(index_path, cumprod_function)
LETF_plus = lapply(LETF_plus, cumprod_function)
LETF_minus = lapply(LETF_minus, cumprod_function)

TV_function = function(x){
  
  tibble(dim(x)[1], as.vector(tail(x, n = 1)))

}

index_stats = lapply(index_path, TV_function)
LETF_plus_stats = lapply(LETF_plus, TV_function)
LETF_minus_stats = lapply(LETF_minus, TV_function)



index_stats = do.call(rbind, index_stats) |>
  mutate("Leverage" = 1)
LETF_plus_stats = do.call(rbind, LETF_plus_stats) |>
  mutate("Leverage" = 3)
LETF_minus_stats = do.call(rbind, LETF_minus_stats) |>
  mutate("Leverage" = -3)

names(index_stats) = c("Days", "Log Wealth", "Leverage")
names(LETF_plus_stats) = c("Days", "Log Wealth", "Leverage")
names(LETF_minus_stats) = c("Days", "Log Wealth", "Leverage")

all_data = rbind(index_stats, LETF_plus_stats, LETF_minus_stats) |>
  mutate("Comp_End" = (`Log Wealth` - 1) / Leverage,
         "Predicted_End" = ((`Log Wealth`-1)/abs(Leverage) + ((.045 * ( abs(Leverage) - 1) + 0) * (Days/365.25))/abs(Leverage) + ((abs(Leverage) -1)/2) * (Days/365.25) * sigma^2))

graph_data = all_data |>
  group_by(Leverage, Days) |>
  summarise("Mean TV" = mean(`Log Wealth`),
            "Std TV" = sd(`Log Wealth`),
            "Adjusted Return" = mean(Comp_End),
            "Mean Prediction" = mean(Predicted_End)) |>
  mutate(Leverage = as.factor(Leverage))

ggplot(graph_data, aes(x = Days, y = `Adjusted Return`, color = Leverage)) + 
  geom_line()
