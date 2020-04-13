library(forecast)
library(ggplot2)
library(dplyr)
library(hash)
library(NNTbiomarker)
library(rlang)
library(fpp)
library(xts)
setwd('C:\\Users\\tangc\\OneDrive\\Desktop\\Files\\HW\\DATA2020\\project')


#### read data ####
# training year is 2015, testing year is 2016
p_train = read.csv(file = 'price_train.csv')
p_test = read.csv(file = 'price_test.csv')
# get all companies
viable_final2_MK4 = read.csv(file = 'viable_final2_MK4.csv')
symbols = unique(viable_final2_MK4$Ticker.Symbol)


#### fit ARIMA and predict for each stock ####
# save models and predictions in dicts
trues = hash()
preds = hash()
orders = hash()
for (s in symbols) {
  print(s)
  # record start time
  start_time = Sys.time()
  # subset training data
  p_train_sub = subset(p_train, symbol == s, select = c(close))
  # log and set frequency to 21
  p_train_sub = ts(log(p_train_sub$close), frequency = 21)
  # fit model using auto.arima, get order, save in orders
  model = auto.arima(p_train_sub)
  od = arimaorder(model)
  orders[[s]] = od
  # subset testing data
  p_test_sub = subset(p_test, symbol == s, select = c(close))
  # log and set frequency to 21
  p_test_sub = ts(log(p_test_sub$close), frequency = 21)
  # predict and save to dict
  p_total = c(p_train_sub, p_test_sub)
  preds_roll = ts(numeric(252), frequency = 21)
  # re-estimate every day
  for (d in 1:252) {
    x = window(p_total, start = d, end = 252 + d - 1)
    refit = Arima(x, order=od[1:3], seasonal=od[4:6], method = 'CSS')
    preds_roll[d] = forecast(refit, h=1)$mean[1]
  }
  preds[[s]] = preds_roll
  # save true values
  trues[[s]] = p_total[252:504]
  # record end time
  end_time = Sys.time()
  print(end_time - start_time)
}


#### calculate pred and true daily rtns for each company ####
pred_rtns = hash()
true_rtns = hash()
for (s in symbols) {
  # record pred and true return for each day
  tmp_pred_rtn = vector()
  tmp_true_rtn = vector()
  for (day in 2:253) {
    # get pred and true price
    pred_value = exp(preds[[s]][day - 1])
    true_value = exp(trues[[s]][day])
    prev_value = exp(trues[[s]][day - 1])
    # calculate pred and true return
    tmp_pred_rtn[day] = (pred_value - prev_value) / prev_value
    tmp_true_rtn[day] = (true_value - prev_value) / prev_value
  }
  pred_rtns[[s]] = tmp_pred_rtn[2:253]
  true_rtns[[s]] = tmp_true_rtn[2:253]
}


#### perform backtest ####
# set capital as $1
capital_base = 1
daily_capital = vector()
short_rtn = vector()
for (day in 1:252) {
  # initialize company as FALSE and max_rtn as 0
  long_company = FALSE
  short_company = FALSE
  max_rtn = 0
  min_rtn = 0
  # find the companies with the largest and smallest predicted return
  for (s in symbols) {
    if (pred_rtns[[s]][day] > max_rtn) {
      long_company = s
      max_rtn = pred_rtns[[s]][day]
    }
    if (pred_rtns[[s]][day] < min_rtn) {
      short_company = s
      min_rtn = pred_rtns[[s]][day]
    }
  }
  # if long_company exists, times capital base with 1 + true return of that company on that day
  if (long_company != FALSE) {
    capital_base = capital_base * (1 + true_rtns[[long_company]][day])
  }
  # if long_company exists, times capital base with 1 - true return of that company on that day
  if (short_company != FALSE) {
    capital_base = capital_base * (1 - true_rtns[[short_company]][day])
  }
  daily_capital[day] = capital_base
  short_rtn[day] = true_rtns[[short_company]][day]
  # cat(1 - true_rtns[[short_company]][day], min_rtn, '\n')
}
print(capital_base - 1)



#### test codes ####

# subset training data
p_train_sub = subset(p_train, symbol == 'AMZN', select = c(close))
# log and set frequency to 21
p_train_sub = ts(log(p_train_sub$close), frequency = 21)

model = auto.arima(p_train_sub)
od = arimaorder(model)

# subset testing data
p_test_sub = subset(p_test, symbol == 'AMZN', select = c(close))
# log and set frequency to 21
p_test_sub = ts(log(p_test_sub$close), frequency = 21)

t_b = Sys.time()
p_total = c(p_train_sub, p_test_sub)
preds_roll = ts(numeric(252), frequency = 21)
# re-estimate every day
for (d in 1:252) {
  x = window(p_total, start = d, end = 252 + d - 1)
  refit = Arima(x, order=od[1:3], seasonal=od[4:6], method = 'CSS')
  preds_roll[d] = forecast(refit, h=1)$mean[1]
}
t_e = Sys.time()
print(t_e - t_b)

plot(preds_roll)
lines(p_test_sub, col='red')
length(preds_roll)



