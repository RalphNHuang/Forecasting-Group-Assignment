library(tidyverse)
library(readr)
library(zoo)
library(forecast)
library(dplyr)
source("./R/utils.R")
source("./R/dataUtils.R")

load(file = "./RData/dataset.RData")
calendar = read.csv("./M5 raw data/calendar.csv")
###ts with frequency = 365
ts365_list = lapply(trainData[,2:ncol(rawData)], ts.beta)

arima_list = lapply(ts365_list, auto.arima)
arima_pred_list = lapply(arima_list, forecast.beta)

### calculate the devisors for RMSSE
devisors = unlist(lapply(trainData[,2:ncol(trainData)], cal.devisor))

### calculate the RMSSE for all columns
arima_RMSSE_v = eval.RMSSE(testData, arima_pred_list, devisor = devisors)


###save list and RMSSE
#save(arima_list, file="./RData/arima_list.RData")
#save(arima_pred_list, file="./RData/arima_pred_list.RData")
save(arima_RMSSE_v, file="./RData/arima_RMSSE_v.RData")



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
  output = forecast(model, level = c(95), h = h, xreg = temp.reg[1:h])
  return(output)
}


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

arimax_list_snap = lapply(ts365_list, auto.arimax.beta)
#save(arimax_list_snap, file="./RData/arimax_list_snap.RData")

###regressor
temp.reg = x_var_test_snap

arimax_pred_list_snap = lapply(arimax_list_snap, arimax_forecast.beta)
#save(arimax_pred_list_snap, file="./RData/arimax_pred_list_snap.RData")
arimax_snap_RMSSE_v = eval.RMSSE(testData, arimax_pred_list_snap, devisor = devisors)
#save(arimax_snap_RMSSE_v,file="./RData/arimax_snap_RMSSE_v.RData")


### calcualte the MES for both train set and test set

arima_RMSE_train_v = eval.RMSE(trainData, arima_pred_list, mode = "train")
arima_RMSE_test_v = eval.RMSE(testData, arima_pred_list, mode = "test")
arimax_RMSE_train_v = eval.RMSE(trainData, arimax_pred_list_snap, mode = "train")
arimax_RMSE_test_v = eval.RMSE(testData, arimax_pred_list_snap, mode = "test")
#save(arima_RMSE_test_v,file = './Rdata/arima_RMSE_test_v.RData')
#save(arimax_RMSE_train_v,file = './Rdata/arimax_RMSE_train_v.RData')

### Aggregation

# 1.3 evaluate different h

arima_crossH_RMSSE = data.frame()
for (h in 1:28) {
  arima_pred_list = lapply(arima_list, forecast.beta, h = h)
  arima_RMSSE_v = eval.RMSSE(testData, arima_pred_list, h = h, devisor = devisors)
  arima_crossH_RMSSE = rbind(arima_crossH_RMSSE, arima_RMSSE_v)
}
names(arima_crossH_RMSSE) = names(arima_RMSSE_v)
#save(arima_crossH_RMSSE, file="./RData/arima_crossH_RMSSE.RData")
arimax_crossH_RMSSE = data.frame()
for (h in 1:28) {
  arimax_pred_list_snap = lapply(arimax_list_snap, arimax_forecast.beta,h = h)
  arimax_snap_RMSSE_v = eval.RMSSE(testData, arimax_pred_list_snap, h = h, devisor = devisors)
  arimax_crossH_RMSSE = rbind(arimax_crossH_RMSSE, arimax_snap_RMSSE_v)
}
names(arimax_crossH_RMSSE) = names(arimax_snap_RMSSE_v)
#save(arimax_crossH_RMSSE, file="./RData/arimax_crossH_RMSSE.RData")


# 1.4 aggregate by item
###arima
cat_tsa_list = lapply(trainAggCat[,2:ncol(trainAggCat)], ts.beta)
cat_arima_list = lapply(cat_tsa_list, auto.arima)
cat_preda_list = lapply(cat_arima_list, forecast.beta)
cat_devisors = unlist(lapply(trainAggCat[,2:ncol(trainAggCat)], cal.devisor))
cat_arima_RMSSE_v = eval.RMSSE(testAggCat, cat_preda_list, devisor = cat_devisors)
#save(cat_arima_RMSSE_v, file = './RData/cat_arima_RMSSE_v.RData')

###arimax
cat_tsa_list = lapply(trainAggCat[,2:ncol(trainAggCat)], ts.beta)
cat_arimax_list = lapply(cat_tsa_list, auto.arimax.beta)
cat_predax_list = lapply(cat_arimax_list, arimax_forecast.beta)
cat_arimax_RMSSE_v = eval.RMSSE(testAggCat, cat_predax_list, devisor = cat_devisors)
#save(cat_arimax_RMSSE_v, file = './RData/cat_arimax_RMSSE_v.RData')

# 1.4 aggregate by store
###arima
store_tsa_list = lapply(trainAggStore[,2:ncol(trainAggStore)], ts.beta)
store_arima_list = lapply(store_tsa_list, auto.arima)
store_preda_list = lapply(store_arima_list, forecast.beta)
store_devisors = unlist(lapply(trainAggStore[,2:ncol(trainAggStore)], cal.devisor))
store_arima_RMSSE_v = eval.RMSSE(testAggStore, store_preda_list, devisor = store_devisors)
#save(store_arima_RMSSE_v, file = './RData/store_arima_RMSSE_v.RData')

###arimax
store_arimax_list = lapply(store_tsa_list, auto.arimax.beta)
store_predax_list = lapply(store_arimax_list, arimax_forecast.beta)
store_arimax_RMSSE_v = eval.RMSSE(testAggStore, store_predax_list, devisor = store_devisors)
#save(store_arimax_RMSSE_v, file = './RData/store_arimax_RMSSE_v.RData')

# 1.5 aggregate by week

ts52.beta = function(column){
  output = ts(column,start = 1, frequency = 52)
  return(output)
}
###arima
week_tsa_list = lapply(trainAggWeek[,2:ncol(trainAggWeek)], ts52.beta)
week_arima_list = lapply(week_tsa_list, auto.arima)
week_preda_list = lapply(week_arima_list, forecast.beta, h = 4)
week_devisors = unlist(lapply(trainAggWeek[,2:ncol(trainAggWeek)], cal.devisor))
week_arima_RMSSE_v = eval.RMSSE(testAggWeek, week_preda_list, h = 4, devisor = week_devisors)
#save(week_arima_RMSSE_v, file = './RData/week_arima_RMSSE_v.RData')

###arimax
temp = agg_by_week(x_var_train_snap)
temp.reg = agg_by_week(x_var_test_snap)
week_arimax_list = lapply(week_tsa_list, auto.arimax.beta)
week_predax_list = lapply(week_arimax_list, arimax_forecast.beta, h = 4)
week_arimax_RMSSE_v = eval.RMSSE(testAggWeek, week_predax_list, h = 4, devisor = week_devisors)