##Forecasting Group Project
#student: Yue Chen, Kai Kang, Jiaqian Ma, Yinzhe Huang


library(fpp2)
library(smooth)
library(tidyverse)
library(readr)
library(zoo)
library(forecast)
library(dplyr)
library(tseries)
library(forecastHybrid)

source("./R/utils.R")
source("./R/dataUtils.R")
calendar = read.csv("./M5 raw data/calendar.csv")

############# Dataset splition #############
rawData <-  read_csv("C:/Forecasting/Projectdata.csv")
rawData[,2:ncol(rawData)] = apply(rawData[,2:ncol(rawData)], 2, scale)
trainData = rawData[c(1:(nrow(rawData)-28)),]
testData = rawData[c((nrow(rawData)-27):nrow(rawData)),]

############# Aggregation #############
# by categories
trainAggCat = agg_by_cat(trainData)
testAggCat = agg_by_cat(testData)
# by week
trainAggWeek = agg_by_week(trainData)
testAggWeek = agg_by_week(testData)
# by store
trainAggStore = agg_by_store(trainData)
testAggStore = agg_by_store(testData)

# 1
##1.1
###1.1.a ACF& PACF
##### ACF & PACF of Hoobies category
par(mfrow=c(3,2))
acf(rawData$Hobbies_CA_1)
pacf(rawData$Hobbies_CA_1)
acf(rawData$Hobbies_CA_2)
pacf(rawData$Hobbies_CA_2)
acf(rawData$Hobbies_CA_3)
pacf(rawData$Hobbies_CA_3)

##### ACF & PACF of Household_1 category
par(mfrow=c(3,2))
acf(rawData$Household_1_CA_1)
pacf(rawData$Household_1_CA_1)
acf(rawData$Household_1_CA_2)
pacf(rawData$Household_1_CA_2)
acf(rawData$Household_1_CA_3)
pacf(rawData$Household_1_CA_3)

##### ACF & PACF of Household_2 category
par(mfrow=c(3,2))
acf(rawData$Household_2_CA_1)
pacf(rawData$Household_2_CA_1)
acf(rawData$Household_2_CA_2)
pacf(rawData$Household_2_CA_2)
acf(rawData$Household_2_CA_3)
pacf(rawData$Household_2_CA_3)

##### ACF & PACF of Foods_1 category
par(mfrow=c(3,2))
acf(rawData$Foods_1_CA_1)
pacf(rawData$Foods_1_CA_1)
acf(rawData$Foods_1_CA_2)
pacf(rawData$Foods_1_CA_2)
acf(rawData$Foods_1_CA_3)
pacf(rawData$Foods_1_CA_3)

##### ACF & PACF of Food_2 category
par(mfrow=c(3,2))
acf(rawData$Foods_2_CA_1)
pacf(rawData$Foods_2_CA_1)
acf(rawData$Foods_2_CA_2)
pacf(rawData$Foods_2_CA_2)
acf(rawData$Foods_2_CA_3)
pacf(rawData$Foods_2_CA_3)


##### ACF & PACF of Foods_3 category
par(mfrow=c(3,2))
acf(rawData$Foods_3_CA_1)
pacf(rawData$Foods_3_CA_1)
acf(rawData$Foods_3_CA_2)
pacf(rawData$Foods_3_CA_2)
acf(rawData$Foods_3_CA_3)
pacf(rawData$Foods_3_CA_3)

#### 1.1.b performing a test?
##We used ADF test on each time series.
##### ADF Test for Hobbies category
adf.test(rawData$Hobbies_CA_1)
adf.test(rawData$Hobbies_CA_2)
adf.test(rawData$Hobbies_CA_3)

##### ADF Test for Houshold_1 category
adf.test(rawData$Household_1_CA_1)
adf.test(rawData$Household_1_CA_2)
adf.test(rawData$Household_1_CA_3)

##### ADF Test for Houshold_2 category
adf.test(rawData$Household_2_CA_1)
adf.test(rawData$Household_2_CA_2)
adf.test(rawData$Household_2_CA_3)

##### ADF Test for Foods_1 category
adf.test(rawData$Foods_1_CA_1)
adf.test(rawData$Foods_1_CA_2)
adf.test(rawData$Foods_1_CA_3)

##### ADF Test for Foods_2 category
adf.test(rawData$Foods_2_CA_1)
adf.test(rawData$Foods_2_CA_2)
adf.test(rawData$Foods_2_CA_3)

##### ADF Test for Foods_3 category
adf.test(rawData$Foods_3_CA_1)
adf.test(rawData$Foods_3_CA_1)
adf.test(rawData$Foods_3_CA_1)

##1.2 
################ 1) Naive #################
ts_list = lapply(trainData[,2:ncol(rawData)], ts)
naive_list = lapply(ts_list, naive)
naive_pred_list = lapply(naive_list, forecast.beta)
## calculate the RMSSE
devisors = unlist(lapply(trainData[,2:ncol(trainData)], cal.devisor))
naive_RMSSE_v = eval.RMSSE(testData, naive_pred_list, devisor = devisors)
### calculate the MES for both train set and test set
naive_RMSE_train_v = eval.RMSE(trainData, naive_pred_list, mode = "train")
naive_RMSE_test_v = eval.RMSE(testData, naive_pred_list, mode = "test")

################ 2) sNaive #################
msts_list = lapply(trainData[,2:ncol(rawData)], msts.beta)
snaive_list = lapply(ts_list, snaive)
snaive_pred_list = lapply(naive_list, forecast.beta)
devisors = unlist(lapply(trainData[,2:ncol(trainData)], cal.devisor))
snaive_RMSSE_v = eval.RMSSE(testData, snaive_pred_list, devisor = devisors)
snaive_RMSE_train_v = eval.RMSE(trainData, snaive_pred_list, mode = "train")
snaive_RMSE_test_v = eval.RMSE(testData, snaive_pred_list, mode = "test")

################ 3) ES #################
ts_list = lapply(trainData[,2:ncol(rawData)], ts)
devisors = unlist(lapply(trainData[,2:ncol(trainData)], cal.devisor))
ses_crossH_RMSSE = data.frame()
for (h in 1:28) {
  ses_pred_list = lapply(ts_list, ses, h=h)
  ses_RMSSE_v = eval.RMSSE(testData, ses_pred_list, h = h, devisor = devisors)
  ses_crossH_RMSSE = rbind(ses_crossH_RMSSE, ses_RMSSE_v)
}
names(ses_crossH_RMSSE) = names(ses_RMSSE_v)
print(ses_crossH_RMSSE)
ses_RMSE_train_v = eval.RMSE(trainData, ses_pred_list, mode = "train")
ses_RMSE_test_v = eval.RMSE(testData, ses_pred_list, mode = "test")

################ 4) MA #################
ts_list = lapply(trainData[,2:ncol(rawData)], ts)
ma_list = lapply(ts_list, sma)
ma_pred_list = lapply(ma_list, forecast.beta)
devisors = unlist(lapply(trainData[,2:ncol(trainData)], cal.devisor))
ma_RMSSE_v = eval.RMSSE(testData, ma_pred_list, devisor = devisors)
#ma_RMSE_train_v = eval.RMSE(trainData, ma_pred_list, mode = "train")
#ma_RMSE_test_v = eval.RMSE(testData, ma_pred_list, mode = "test")
#ma_RMSE_train_v 
#ma_RMSE_test_v
#ma_pred_list[[1]]$forecast

################ 5) ESX #################
ts_list = lapply(trainData[,2:ncol(rawData)], ts)
for (h in 1:28) {
  esx_pred_list = lapply(ts_list, ses, h=h, xreg = x_var_train$holiday)
}
calendar = read.csv("./dat/calendar.csv")
x_var <- calendar
x_var$holiday <- 0
x_var[is.na(x_var$event_type_1)==F,]$holiday <- 1
x_var <- x_var[,c("snap_CA","holiday")]
x_var <- x_var[c(1:(nrow(x_var)-28)),]
x_var_train <- x_var[c(1:(nrow(x_var)-28)),]
x_var_train <- as.matrix(x_var_train)
x_var_test <- x_var[c((nrow(x_var)-27):nrow(x_var)),]
x_var_test <- as.matrix(x_var_test)

#### use snap_CA to do ESX
ts_list = lapply(trainData[,2:ncol(rawData)], ts)
devisors = unlist(lapply(trainData[,2:ncol(trainData)], cal.devisor))
esx_snap_crossH_RMSSE = data.frame()
for (h in 1:28) {
  esx_snap_pred_list = lapply(ts_list, ses, h=h, xreg =x_var_train$snap_CA)
  esx_snap_RMSSE_v = eval.RMSSE(testData, esx_snap_pred_list, h = h, devisor = devisors)
  esx_snap_crossH_RMSSE = rbind(esx_snap_crossH_RMSSE, esx_snap_RMSSE_v)
}
names(esx_snap_crossH_RMSSE) = names(esx_snap_RMSSE_v)
print(esx_snap_crossH_RMSSE)

##### use holiday to do ESX
ts_list = lapply(trainData[,2:ncol(rawData)], ts)
devisors = unlist(lapply(trainData[,2:ncol(trainData)], cal.devisor))
esx_holiday_crossH_RMSSE = data.frame()
for (h in 1:28) {
  esx_holiday_pred_list = lapply(ts_list, ses, h=h, xreg =x_var_train$holidy)
  esx_holiday_RMSSE_v = eval.RMSSE(testData, esx_holiday_pred_list, h = h, devisor = devisors)
  esx_holiday_crossH_RMSSE = rbind(esx_holiday_crossH_RMSSE, esx_holiday_RMSSE_v)
}
names(esx_holiday_crossH_RMSSE) = names(esx_holiday_RMSSE_v)
print(esx_holiday_crossH_RMSSE)

print(lapply(ts_list, ses, h=h, xreg =x_var_train$snap_CA))
print(lapply(ts_list, ses, h=h, xreg =x_var_train$holidy))

#### f) Compare the in-sample (training) vs. out-of-sample (testing) fit of the models.
esx_snap_RMSE_train_v = eval.RMSE(trainData, esx_snap_pred_list, mode = "train")
esx_snap_RMSE_test_v = eval.RMSE(testData, esx_snap_pred_list, mode = "test")
esx_holiday_RMSE_train_v = eval.RMSE(trainData, esx_holiday_pred_list, mode = "train")
esx_holiday_RMSE_test_v = eval.RMSE(testData, esx_holiday_pred_list, mode = "test")


################ 6) SARIMA #################
###ts with frequency = 365
ts365_list = lapply(trainData[,2:ncol(rawData)], ts.beta)
arima_list = lapply(ts365_list, auto.arima)
arima_pred_list = lapply(arima_list, forecast.beta)
### calculate the devisors for RMSSE
devisors = unlist(lapply(trainData[,2:ncol(trainData)], cal.devisor))
### calculate the RMSSE for all columns
arima_RMSSE_v = eval.RMSSE(testData, arima_pred_list, devisor = devisors)

### calcualte the MES for both train set and test set
arima_RMSE_train_v = eval.RMSE(trainData, arima_pred_list, mode = "train")
arima_RMSE_test_v = eval.RMSE(testData, arima_pred_list, mode = "test")
arimax_RMSE_train_v = eval.RMSE(trainData, arimax_pred_list_snap, mode = "train")
arimax_RMSE_test_v = eval.RMSE(testData, arimax_pred_list_snap, mode = "test")

################ 7) SARIMAX #################
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
###regressor
temp.reg = x_var_test_snap
arimax_pred_list_snap = lapply(arimax_list_snap, arimax_forecast.beta)
arimax_snap_RMSSE_v = eval.RMSSE(testData, arimax_pred_list_snap, devisor = devisors)

################ 8) Holt-Winters #################
ts_list = lapply(trainData[,2:ncol(rawData)], ts)
hw_list = lapply(ts_list, ets)
devisors = unlist(lapply(trainData[,2:ncol(trainData)], cal.devisor))
hw_crossH_RMSSE = data.frame()
for (h in 1:28) {
  hw_pred_list = lapply(hw_list, forecast.beta, h = h)
  hw_RMSSE_v = eval.RMSSE(testData, hw_pred_list, h = h, devisor = devisors)
  hw_crossH_RMSSE = rbind(hw_crossH_RMSSE, hw_RMSSE_v)
}
names(hw_crossH_RMSSE) = names(hw_RMSSE_v)
print(hw_crossH_RMSSE)

####(c) Obtain a sequence of 1-step ahead point forecasts over the testing subsample.
step_1_hw_pred_seq = c()
for (i in 1:28) {
  train_data_step_1 = rawData[c(1:(nrow(rawData)-i)),]
  test_data_step_1 = rawData[c((nrow(rawData)-i+1):nrow(rawData)),]
  train_step_1_ts = ts(train_data_step_1$Hobbies_CA_1)
  hw_step_1 = ets(train_step_1_ts)
  hw_pred_step_1 = forecast.beta(hw_list,h=1)
  step_1_hw_pred_seq[i] = hw_pred_step_1
}

####(f) Compare the in-sample (training) vs. out-of-sample (testing) fit of the models.
hw_RMSE_train_v = eval.RMSE(trainData, hw_pred_list, mode = "train")
hw_RMSE_test_v = eval.RMSE(testData, hw_pred_list, mode = "test")

################ 9)state-space model #################
ts_list = lapply(trainData[,2:ncol(rawData)], msts.beta)
tbats_list = lapply(ts_list, tbats)
tbats_pred_list = lapply(tbats_list, forecast.beta)
devisors = unlist(lapply(trainData[,2:ncol(trainData)], cal.devisor))
tbats_RMSSE_v = eval.RMSSE(testData, tbats_pred_list, devisor = devisors)

### calcualte the MES for both train set and test set
tbats_RMSE_train_v = eval.RMSE(trainData, tbats_pred_list, mode = "train")
tbats_RMSE_test_v = eval.RMSE(testData, tbats_pred_list, mode = "test")


####1.2.e Compare the above with combinations of the forecasting techniques 
####(using simple averages across methods, such as Combination benchmark #21 in the M5 guidelines).
ts_list = lapply(trainData[,2:ncol(rawData)], ts)
hybrid_list = lapply(ts_list, hybridModel,models='ae',weights='equal')
devisors = unlist(lapply(trainData[,2:ncol(trainData)], cal.devisor))

hybrid_crossH_RMSSE = data.frame()
for (h in 1:20) {
  hybrid_pred_list = lapply(hybrid_list, forecast.beta, h = h)
  hybrid_RMSSE_v = eval.RMSSE(testData, hybrid_pred_list, h = h, devisor = devisors)
  hybrid_crossH_RMSSE = rbind(hybrid_crossH_RMSSE, hybrid_RMSSE_v)
}

names(hybrid_crossH_RMSSE) = names(hybrid_RMSSE_v)
print(hybrid_crossH_RMSSE)



##1.3  RMSSE h=1,...,28
################ 1) Naive #################
ts_list = lapply(trainData[,2:ncol(rawData)], ts)
devisors = unlist(lapply(trainData[,2:ncol(trainData)], cal.devisor))
naive_crossH_RMSSE = data.frame()
for (h in 1:28) {
  naive_pred_list = lapply(ts_list, naive, h=h)
  naive_RMSSE_v = eval.RMSSE(testData, naive_pred_list, h = h, devisor = devisors)
  naive_crossH_RMSSE = rbind(naive_crossH_RMSSE, naive_RMSSE_v)
}
names(naive_crossH_RMSSE) = names(naive_RMSSE_v)
naive_crossH_RMSSE

################ 2) sNaive #################
msts_list = lapply(trainData[,2:ncol(rawData)], msts.beta)
devisors = unlist(lapply(trainData[,2:ncol(trainData)], cal.devisor))
snaive_crossH_RMSSE = data.frame()
for (h in 1:28) {
  snaive_pred_list = lapply(msts_list, snaive, h=h)
  snaive_RMSSE_v = eval.RMSSE(testData, snaive_pred_list, h = h, devisor = devisors)
  snaive_crossH_RMSSE = rbind(snaive_crossH_RMSSE, snaive_RMSSE_v)
}
names(snaive_crossH_RMSSE) = names(snaive_RMSSE_v)
snaive_crossH_RMSSE

################ 3) ES #################
ts_list = lapply(trainData[,2:ncol(rawData)], ts)
for (h in 1:28) {
  ses_pred_list = lapply(ts_list, ses, h=h)
}

################ 4) MA #################
ts_list = lapply(trainData[,2:ncol(rawData)], ts)
ma_list = lapply(ts_list, sma)
devisors = unlist(lapply(trainData[,2:ncol(trainData)], cal.devisor))
ma_crossH_RMSSE = data.frame()
for (h in 1:28) {
  ma_pred_list = lapply(ma_list, forecast.beta,h=h)
  ma_RMSSE_v = eval.RMSSE(testData, ma_pred_list, h = h, devisor = devisors)
  ma_crossH_RMSSE = rbind(ma_crossH_RMSSE, ma_RMSSE_v)
}
names(ma_crossH_RMSSE) = names(ma_RMSSE_v)
ma_crossH_RMSSE

################ 5) ESX #################
#### use snap_CA to do ESX
ts_list = lapply(trainData[,2:ncol(rawData)], ts)
devisors = unlist(lapply(trainData[,2:ncol(trainData)], cal.devisor))
esx_snap_crossH_RMSSE = data.frame()
for (h in 1:28) {
  esx_snap_pred_list = lapply(ts_list, ses, h=h, xreg =x_var_train$snap_CA)
  esx_snap_RMSSE_v = eval.RMSSE(testData, esx_snap_pred_list, h = h, devisor = devisors)
  esx_snap_crossH_RMSSE = rbind(esx_snap_crossH_RMSSE, esx_snap_RMSSE_v)
}
names(esx_snap_crossH_RMSSE) = names(esx_snap_RMSSE_v)
print(esx_snap_crossH_RMSSE)

##### use holiday to do ESX
ts_list = lapply(trainData[,2:ncol(rawData)], ts)
devisors = unlist(lapply(trainData[,2:ncol(trainData)], cal.devisor))
esx_holiday_crossH_RMSSE = data.frame()
for (h in 1:28) {
  esx_holiday_pred_list = lapply(ts_list, ses, h=h, xreg =x_var_train$holidy)
  esx_holiday_RMSSE_v = eval.RMSSE(testData, esx_holiday_pred_list, h = h, devisor = devisors)
  esx_holiday_crossH_RMSSE = rbind(esx_holiday_crossH_RMSSE, esx_holiday_RMSSE_v)
}
names(esx_holiday_crossH_RMSSE) = names(esx_holiday_RMSSE_v)
print(esx_holiday_crossH_RMSSE)

################ 6) SARIMA #################
arima_crossH_RMSSE = data.frame()
for (h in 1:28) {
  arima_pred_list = lapply(arima_list, forecast.beta, h = h)
  arima_RMSSE_v = eval.RMSSE(testData, arima_pred_list, h = h, devisor = devisors)
  arima_crossH_RMSSE = rbind(arima_crossH_RMSSE, arima_RMSSE_v)
}
names(arima_crossH_RMSSE) = names(arima_RMSSE_v)


################ 7) SARIMAX #################
arimax_crossH_RMSSE = data.frame()
for (h in 1:28) {
  arimax_pred_list_snap = lapply(arimax_list_snap, arimax_forecast.beta,h = h)
  arimax_snap_RMSSE_v = eval.RMSSE(testData, arimax_pred_list_snap, h = h, devisor = devisors)
  arimax_crossH_RMSSE = rbind(arimax_crossH_RMSSE, arimax_snap_RMSSE_v)
}
names(arimax_crossH_RMSSE) = names(arimax_snap_RMSSE_v)

################ 8) Holt-Winters #################
ts_list = lapply(trainData[,2:ncol(rawData)], ts)
hw_list = lapply(ts_list, ets)
devisors = unlist(lapply(trainData[,2:ncol(trainData)], cal.devisor))

hw_crossH_RMSSE = data.frame()
for (h in 1:28) {
  hw_pred_list = lapply(hw_list, forecast.beta, h = h)
  hw_RMSSE_v = eval.RMSSE(testData, hw_pred_list, h = h, devisor = devisors)
  hw_crossH_RMSSE = rbind(hw_crossH_RMSSE, hw_RMSSE_v)
}

names(hw_crossH_RMSSE) = names(hw_RMSSE_v)
print(hw_crossH_RMSSE)

################ 9)state-space model #################
tbats_crossH_RMSSE = data.frame()
for (h in 1:28) {
  pred_list = lapply(tbats_list, forecast.beta, h = h)
  tbats_RMSSE_v = eval.RMSSE(testData, pred_list, h = h, devisor = devisors)
  tbats_crossH_RMSSE = rbind(tbats_crossH_RMSSE, tbats_RMSSE_v)
}
names(tbats_crossH_RMSSE) = names(tbats_RMSSE_v)


## 1.4 aggregate by item
################ 1) Naive #################
cat_ts_list = lapply(trainAggCat[,2:ncol(trainAggCat)], ts)
naive_cat_pred_list = lapply(cat_ts_list, naive, h=28)
cat_devisors = unlist(lapply(trainAggCat[,2:ncol(trainAggCat)], cal.devisor))
naive_cat_RMSSE_v = eval.RMSSE(testAggCat, naive_cat_pred_list, h=28, devisor = cat_devisors)
naive_cat_RMSSE_v

################ 2) sNaive #################
cat_msts_list = lapply(trainAggCat[,2:ncol(trainAggCat)], msts.beta)
snaive_cat_pred_list = lapply(cat_msts_list, snaive,h=28)
cat_devisors = unlist(lapply(trainAggCat[,2:ncol(trainAggCat)], cal.devisor))
snaive_cat_RMSSE_v = eval.RMSSE(testAggCat, snaive_cat_pred_list, devisor = cat_devisors)
snaive_cat_RMSSE_v

################ 3) ES #################
cat_ts_list = lapply(trainAggCat[,2:ncol(trainAggCat)], ts)
cat_ses_list = lapply(cat_ts_list, ses, h=4)
cat_devisors = unlist(lapply(trainAggCat[,2:ncol(trainAggCat)], cal.devisor))
cat_ses_RMSSE_v = eval.RMSSE(testAggCat, cat_ses_list, h=4, devisor = cat_devisors)
print(cat_ses_RMSSE_v)

################ 4) MA #################
cat_ts_list = lapply(trainAggCat[,2:ncol(trainAggCat)], ts)
ma_cat_list = lapply(cat_ts_list, sma)
ma_cat_pred_list = lapply(ma_cat_list, forecast.beta)
cat_devisors = unlist(lapply(trainAggCat[,2:ncol(trainAggCat)], cal.devisor))
ma_cat_RMSSE_v = eval.RMSSE(testAggCat, ma_cat_pred_list, devisor = cat_devisors)
ma_cat_RMSSE_v

################ 5) ESX #################
cat_ts_list = lapply(trainAggCat[,2:ncol(trainAggCat)], ts)
cat_esx_snap_list = lapply(cat_ts_list, ses, h=4)
cat_devisors = unlist(lapply(trainAggCat[,2:ncol(trainAggCat)], cal.devisor))
cat_esx_snap_RMSSE_v = eval.RMSSE(testAggCat, cat_esx_snap_list, h=4, devisor = cat_devisors)
print(cat_esx_snap_RMSSE_v)

cat_ts_list = lapply(trainAggCat[,2:ncol(trainAggCat)], ts)
cat_esx_holiday_list = lapply(cat_ts_list, ses, h=4)
cat_devisors = unlist(lapply(trainAggCat[,2:ncol(trainAggCat)], cal.devisor))
cat_esx_holiday_RMSSE_v = eval.RMSSE(testAggCat, cat_esx_holiday_list, h=4, devisor = cat_devisors)
print(cat_esx_holiday_RMSSE_v)

################ 6) SARIMA #################
cat_tsa_list = lapply(trainAggCat[,2:ncol(trainAggCat)], ts.beta)
cat_arima_list = lapply(cat_tsa_list, auto.arima)
cat_preda_list = lapply(cat_arima_list, forecast.beta)
cat_devisors = unlist(lapply(trainAggCat[,2:ncol(trainAggCat)], cal.devisor))
cat_arima_RMSSE_v = eval.RMSSE(testAggCat, cat_preda_list, devisor = cat_devisors)

################ 7) SARIMAX #################
cat_tsa_list = lapply(trainAggCat[,2:ncol(trainAggCat)], ts.beta)
cat_arimax_list = lapply(cat_tsa_list, auto.arimax.beta)
cat_predax_list = lapply(cat_arimax_list, arimax_forecast.beta)
cat_arimax_RMSSE_v = eval.RMSSE(testAggCat, cat_predax_list, devisor = cat_devisors)

################ 8) Holt-Winters #################
cat_ts_list = lapply(trainAggCat[,2:ncol(trainAggCat)], msts.beta)
cat_hw_list = lapply(cat_ts_list, ets)
cat_hw_pred_list = lapply(cat_ts_list, forecast.beta)
cat_devisors = unlist(lapply(trainAggCat[,2:ncol(trainAggCat)], cal.devisor))
cat_hw_RMSSE_v = eval.RMSSE(testAggCat, cat_hw_pred_list, devisor = cat_devisors)
print(cat_hw_RMSSE_v)

################ 9) stace model #################
cat_ts_list = lapply(trainAggCat[,2:ncol(trainAggCat)], msts.beta)
cat_tbats_list = lapply(cat_ts_list, tbats)
cat_tbats_pred_list = lapply(cat_tbats_list, forecast.beta)
cat_devisors = unlist(lapply(trainAggCat[,2:ncol(trainAggCat)], cal.devisor))
cat_tbats_RMSSE_v = eval.RMSSE(testAggCat, cat_tbats_pred_list, devisor = cat_devisors)


## 1.4 aggregate by store
################ 1) Naive #################
store_ts_list = lapply(trainAggStore[,2:ncol(trainAggStore)], ts)
naive_store_pred_list = lapply(store_ts_list, naive, h=28)
store_devisors = unlist(lapply(trainAggStore[,2:ncol(trainAggStore)], cal.devisor))
naive_store_RMSSE_v = eval.RMSSE(testAggStore, naive_store_pred_list, devisor = store_devisors)
naive_store_RMSSE_v

################ 2) sNaive #################
store_msts_list = lapply(trainAggStore[,2:ncol(trainAggStore)], msts.beta)
snaive_store_pred_list = lapply(store_msts_list, snaive,h=28)
store_devisors = unlist(lapply(trainAggStore[,2:ncol(trainAggStore)], cal.devisor))
snaive_store_RMSSE_v = eval.RMSSE(testAggStore, snaive_store_pred_list, devisor = store_devisors)
snaive_store_RMSSE_v

################ 3) ES #################
store_ts_list = lapply(trainAggStore[,2:ncol(trainAggStore)], ts)
store_ses_list = lapply(store_ts_list, ses, h=4)
store_devisors = unlist(lapply(trainAggStore[,2:ncol(trainAggStore)], cal.devisor))
store_ses_RMSSE_v = eval.RMSSE(testAggStore, store_ses_list, h=4, devisor = store_devisors)
print(store_ses_RMSSE_v)

################ 4) MA #################
store_ts_list = lapply(trainAggStore[,2:ncol(trainAggStore)], ts)
ma_store_list = lapply(store_ts_list, sma)
ma_store_pred_list = lapply(ma_store_list, forecast.beta,h=28)
store_devisors = unlist(lapply(trainAggStore[,2:ncol(trainAggStore)], cal.devisor))
ma_store_RMSSE_v = eval.RMSSE(testAggStore, ma_store_pred_list, devisor = store_devisors)
ma_store_RMSSE_v

################ 5) ESX #################
store_ts_list = lapply(trainAggStore[,2:ncol(trainAggStore)], ts)
store_esx_snap_list = lapply(store_ts_list, ses, h=4)
store_devisors = unlist(lapply(trainAggStore[,2:ncol(trainAggStore)], cal.devisor))
store_esx_snap_RMSSE_v = eval.RMSSE(testAggStore, store_esx_snap_list, h=4, devisor = store_devisors)
print(store_esx_snap_RMSSE_v)

store_ts_list = lapply(trainAggStore[,2:ncol(trainAggStore)], ts)
store_esx_holiday_list = lapply(store_ts_list, ses, h=4)
store_devisors = unlist(lapply(trainAggStore[,2:ncol(trainAggStore)], cal.devisor))
store_esx_holiday_RMSSE_v = eval.RMSSE(testAggStore, store_esx_holiday_list, h=4, devisor = store_devisors)
print(store_esx_holiday_RMSSE_v)

################ 6) SARIMA #################
store_tsa_list = lapply(trainAggStore[,2:ncol(trainAggStore)], ts.beta)
store_arima_list = lapply(store_tsa_list, auto.arima)
store_preda_list = lapply(store_arima_list, forecast.beta)
store_devisors = unlist(lapply(trainAggStore[,2:ncol(trainAggStore)], cal.devisor))
store_arima_RMSSE_v = eval.RMSSE(testAggStore, store_preda_list, devisor = store_devisors)

################ 7) SARIMAX #################
store_arimax_list = lapply(store_tsa_list, auto.arimax.beta)
store_predax_list = lapply(store_arimax_list, arimax_forecast.beta)
store_arimax_RMSSE_v = eval.RMSSE(testAggStore, store_predax_list, devisor = store_devisors)

################ 8) Holt-Winters #################
store_ts_list = lapply(trainAggStore[,2:ncol(trainAggStore)], msts.beta)
store_hw_list = lapply(store_ts_list, ets)
store_hw_pred_list = lapply(store_ts_list, forecast.beta)
store_devisors = unlist(lapply(trainAggStore[,2:ncol(trainAggStore)], cal.devisor))
store_hw_RMSSE_v = eval.RMSSE(testAggStore, store_hw_pred_list, devisor = store_devisors)
print(store_hw_RMSSE_v)

################ 9) state model #################
store_ts_list = lapply(trainAggStore[,2:ncol(trainAggStore)], msts.beta)
store_tbats_list = lapply(store_ts_list, tbats)
store_tbats_pred_list = lapply(store_tbats_list, forecast.beta)
store_devisors = unlist(lapply(trainAggStore[,2:ncol(trainAggStore)], cal.devisor))
store_tbats_RMSSE_v = eval.RMSSE(testAggStore, store_tbats_pred_list, devisor = store_devisors)

## 1.5 aggregate by week
################ 1) Naive #################
week_ts_list = lapply(trainAggWeek[,2:ncol(trainAggWeek)], ts)
naive_week_pred_list = lapply(week_ts_list, naive, h = 4)
week_devisors = unlist(lapply(trainAggWeek[,2:ncol(trainAggWeek)], cal.devisor))
naive_week_RMSSE_v = eval.RMSSE(testAggWeek, naive_week_pred_list, h = 4, devisor = week_devisors)
naive_week_RMSSE_v

################ 2) sNaive ################# 
week_ts_list = lapply(trainAggWeek[,2:ncol(trainAggWeek)], ts)
snaive_week_pred_list = lapply(week_ts_list, snaive, h = 4)
week_devisors = unlist(lapply(trainAggWeek[,2:ncol(trainAggWeek)], cal.devisor))
snaive_week_RMSSE_v = eval.RMSSE(testAggWeek, snaive_week_pred_list, h = 4, devisor = week_devisors)
snaive_week_RMSSE_v

################ 3) ES ################# 
week_ts_list = lapply(trainAggWeek[,2:ncol(trainAggWeek)], ts)
week_ses_list = lapply(week_ts_list, ses, h=4)
week_devisors = unlist(lapply(trainAggWeek[,2:ncol(trainAggWeek)], cal.devisor))
week_tbats_RMSSE_v = eval.RMSSE(testAggWeek, week_ses_list, h = 4, devisor = week_devisors)
print(week_tbats_RMSSE_v)

################ 4) MA #################
week_ts_list = lapply(trainAggWeek[,2:ncol(trainAggWeek)], ts)
ma_week_list = lapply(week_ts_list, sma)
ma_week_pred_list = lapply(ma_week_list, forecast.beta, h = 4)
week_devisors = unlist(lapply(trainAggWeek[,2:ncol(trainAggWeek)], cal.devisor))
ma_week_RMSSE_v = eval.RMSSE(testAggWeek, ma_week_pred_list, h = 4, devisor = week_devisors)
ma_week_RMSSE_v

################ 5) ESX #################
week_ts_list = lapply(trainAggWeek[,2:ncol(trainAggWeek)], ts)
week_esx_snap_list = lapply(week_ts_list, ses, h=4)
week_devisors = unlist(lapply(trainAggWeek[,2:ncol(trainAggWeek)], cal.devisor))
week_tbats_RMSSE_v = eval.RMSSE(testAggWeek, week_esx_snap_list, h = 4, devisor = week_devisors)
print(week_tbats_RMSSE_v)

week_ts_list = lapply(trainAggWeek[,2:ncol(trainAggWeek)], ts)
week_esx_holiday_list = lapply(week_ts_list, ses, h=4)
week_devisors = unlist(lapply(trainAggWeek[,2:ncol(trainAggWeek)], cal.devisor))
week_tbats_RMSSE_v = eval.RMSSE(testAggWeek, week_esx_holiday_list, h = 4, devisor = week_devisors)
print(week_tbats_RMSSE_v)

################ 6) SARIMA #################
ts52.beta = function(column){
  output = ts(column,start = 1, frequency = 52)
  return(output)
}
week_tsa_list = lapply(trainAggWeek[,2:ncol(trainAggWeek)], ts52.beta)
week_arima_list = lapply(week_tsa_list, auto.arima)
week_preda_list = lapply(week_arima_list, forecast.beta, h = 4)
week_devisors = unlist(lapply(trainAggWeek[,2:ncol(trainAggWeek)], cal.devisor))
week_arima_RMSSE_v = eval.RMSSE(testAggWeek, week_preda_list, h = 4, devisor = week_devisors)

################ 7) SARIMAX #################
temp = agg_by_week(x_var_train_snap)
temp.reg = agg_by_week(x_var_test_snap)
week_arimax_list = lapply(week_tsa_list, auto.arimax.beta)
week_predax_list = lapply(week_arimax_list, arimax_forecast.beta, h = 4)
week_arimax_RMSSE_v = eval.RMSSE(testAggWeek, week_predax_list, h = 4, devisor = week_devisors)

################ 8) Holt-Winters #################
week_ts_list = lapply(trainAggWeek[,2:ncol(trainAggWeek)], msts.beta)
week_hw_list = lapply(week_ts_list, ets)
week_hw_pred_list = lapply(week_ts_list, forecast.beta, h = 4)
week_devisors = unlist(lapply(trainAggWeek[,2:ncol(trainAggWeek)], cal.devisor))
week_hw_RMSSE_v = eval.RMSSE(testAggWeek, week_hw_pred_list, h = 4, devisor = week_devisors)
print(week_hw_RMSSE_v)
  
################ 9) state-space model #################
week_ts_list = lapply(trainAggWeek[,2:ncol(trainAggWeek)], msts.beta)
week_tbats_list = lapply(week_ts_list, tbats)
week_tbats_pred_list = lapply(week_tbats_list, forecast.beta, h = 4)
week_devisors = unlist(lapply(trainAggWeek[,2:ncol(trainAggWeek)], cal.devisor))
week_tbats_RMSSE_v = eval.RMSSE(testAggWeek, week_tbats_pred_list, 
                                h = 4, devisor = week_devisors)





###save RData
save(arima_RMSSE_v, file="./RData/arima_RMSSE_v.RData")
save(arimax_list_snap, file="./RData/arimax_list_snap.RData")
save(arimax_pred_list_snap, file="./RData/arimax_pred_list_snap.RData")
save(arimax_snap_RMSSE_v,file="./RData/arimax_snap_RMSSE_v.RData")
save(arima_RMSE_test_v,file = './Rdata/arima_RMSE_test_v.RData')
save(arimax_RMSE_train_v,file = './Rdata/arimax_RMSE_train_v.RData')
save(arima_crossH_RMSSE, file="./RData/arima_crossH_RMSSE.RData")
save(arimax_crossH_RMSSE, file="./RData/arimax_crossH_RMSSE.RData")
save(naive_pred_list,naive_RMSSE_v,naive_RMSE_train_v,naive_RMSE_test_v,
     snaive_pred_list,snaive_RMSSE_v,snaive_RMSE_train_v,
     snaive_RMSE_test_v,ma_pred_list,ma_RMSSE_v,ma_RMSE_train_v,ma_RMSE_test_v,
     naive_pred_list,naive_pred_list,snaive_pred_list,ma_pred_list,naive_cat_pred_list,
     snaive_cat_pred_list, ma_cat_list, ma_cat_pred_list, naive_store_pred_list,
     snaive_store_pred_list, ma_store_pred_list,naive_week_pred_list,
     snaive_week_pred_list,ma_week_pred_list, naive_crossH_RMSSE,
     snaive_crossH_RMSSE,ma_crossH_RMSSE,naive_cat_RMSSE_v,snaive_cat_RMSSE_v,
     ma_cat_RMSSE_v,naive_store_RMSSE_v,snaive_store_RMSSE_v,ma_store_RMSSE_v,
     naive_week_RMSSE_v,snaive_week_RMSSE_v, ma_week_RMSSE_v, 
     file='./RData/scaled_naive.Rdata')
save(naive_list,snaive_list,ma_list,
     ma_cat_list,ma_store_list,ma_week_list,
     file='./RData/scaled_ma_model_list.Rdata')
save(cat_arima_RMSSE_v, file = './RData/cat_arima_RMSSE_v.RData')
save(cat_arimax_RMSSE_v, file = './RData/cat_arimax_RMSSE_v.RData')
save(store_arima_RMSSE_v, file = './RData/store_arima_RMSSE_v.RData')
save(store_arimax_RMSSE_v, file = './RData/store_arimax_RMSSE_v.RData')
save(week_arima_RMSSE_v, file = './RData/week_arima_RMSSE_v.RData')
save(ts_list, tbats_list, tbats_pred_list, 
     tbats_RMSSE_v, tbats_RMSE_train_v, tbats_RMSE_test_v,
     tbats_crossH_RMSSE, 
     cat_ts_list, cat_tbats_list, cat_tbats_pred_list, cat_tbats_RMSSE_v,
     store_ts_list, store_tbats_list, store_tbats_pred_list, store_tbats_RMSSE_v, 
     week_ts_list, week_tbats_list, week_tbats_pred_list, week_tbats_RMSSE_v,
     file = "./RData/tbats.RData")