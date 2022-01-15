library(tidyverse)
library(readr)
library(zoo)
library(forecast)
library(dplyr)
source("./R/utils.R")
source("./R/dataUtils.R")


### Dataset splition
rawData = read.csv("./dat/Projectdata.csv")
calendar = read.csv("./M5 raw data/calendar.csv")
trainData = rawData[c(1:(nrow(rawData)-28)),]
testData = rawData[c((nrow(rawData)-27):nrow(rawData)),]


ts_list = lapply(trainData[,2:ncol(rawData)], msts.beta)
arima_list = lapply(ts_list, auto.arima)
arima_pred_list = lapply(arima_list, forecast.beta)

### calculate the devisors for RMSSE
devisors = unlist(lapply(trainData[,2:ncol(trainData)], cal.devisor))

### calculate the RMSSE for all columns
arima_RMSSE_v = eval.RMSSE(testData, arima_pred_list, devisor = devisors)


###save list and RMSSE
#save(arima_list, file="./arimalist/arima_list.RData")
#load(file="./arimalist/arima_list.RData")
#save(arima_pred_list, file="./arimalist/arima_pred_list.RData")
#save(arima_RMSSE_v, file="./arimalist/arima_RMSSE_v.RData")



###SARIMAX model
ts.beta = function(column){
  output = ts(column,start = 1, frequency = 365.25)
  return(output)
}

auto.arimax.beta = function(column){
  output = auto.arima(column, xreg = temp)
  return(output)
}

arimax_forecast.beta = function(model, h = 28){
  output = forecast(model, level = c(95), h = h, xreg = temp.reg)
  return(output)
}

###ts with frequency = 365
ts365_list = lapply(trainData[,2:ncol(rawData)], ts.beta)

###use event and holiday as external variables
x_var <- calendar
x_var$holiday <- 0
x_var[is.na(x_var$event_type_1)==F,]$holiday <- 1
x_var <- x_var[,c("snap_CA","holiday")]
x_var_train_snap <- x_var[c(1:(nrow(x_var)-28)),1]
x_var_train_snap <- as.matrix(x_var_train_snap)
x_var_train_ho <- x_var[c(1:(nrow(x_var)-28)),2]
x_var_train_ho <- as.matrix(x_var_train_ho)
x_var_test_snap <- x_var[c((nrow(x_var)-27):nrow(x_var)),1]
x_var_test_snap <- as.matrix(x_var_test_snap)
x_var_test_ho <- x_var[c((nrow(x_var)-27):nrow(x_var)),2]
x_var_test_ho <- as.matrix(x_var_test_ho)

temp = x_var_train_snap

arimax_list_ho = lapply(ts365_list, auto.arimax.beta)
#save(arimax_list_snap, file="./arimalist/arimaxlist_snap.RData")
arimax_list_snap = lapply(ts365_list, auto.arimax.beta)
###regressor
temp.reg = x_var_test_snap

arimax_pred_list_snap = lapply(arimax_list_snap, arimax_forecast.beta)

arimax_snap_RMSSE_v = eval.RMSSE(testData, arimax_pred_list_snap, devisor = devisors)
#save(arimax_snap_RMSSE_v,file="./arimalist/arimax_snap_RMSSE_v.RData")
### Aggregation

# by categories
trainAggCat = agg_by_cat(trainData)
testAggCat = agg_by_cat(testData)

###predict sales of items aggregated by categories
pred_list = lapply(testAggCat, forecast.beta)

# by week
trainAggWeek = agg_by_week(trainData)
testAggWeek = agg_by_week(testData)
