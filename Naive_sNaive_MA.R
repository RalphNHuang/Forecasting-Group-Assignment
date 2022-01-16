
source("C:/Users/69560/Desktop/forecasting_R/utils.R")
source("C:/Users/69560/Desktop/forecasting_R/dataUtils.R")
library(fpp2)
library(smooth)
#source("./R/utils.R")
#source("./R/dataUtils.R")
load("C:/Users/69560/Desktop/forecasting_R/scaled_naive.Rdata")


#load("C:/Users/69560/Desktop/forecasting_R/naive.Rdata")
#load("C:/Users/69560/Desktop/forecasting_R/scaled_naive.Rdata")

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

##1.2 
###1.2.a,d,f
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


##1.3  benchmarks
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

################ 4) MA #################
cat_ts_list = lapply(trainAggCat[,2:ncol(trainAggCat)], ts)
ma_cat_list = lapply(cat_ts_list, sma)
ma_cat_pred_list = lapply(ma_cat_list, forecast.beta)
cat_devisors = unlist(lapply(trainAggCat[,2:ncol(trainAggCat)], cal.devisor))
ma_cat_RMSSE_v = eval.RMSSE(testAggCat, ma_cat_pred_list, devisor = cat_devisors)
ma_cat_RMSSE_v


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

################ 4) MA #################
store_ts_list = lapply(trainAggStore[,2:ncol(trainAggStore)], ts)
ma_store_list = lapply(store_ts_list, sma)
ma_store_pred_list = lapply(ma_store_list, forecast.beta,h=28)
store_devisors = unlist(lapply(trainAggStore[,2:ncol(trainAggStore)], cal.devisor))
ma_store_RMSSE_v = eval.RMSSE(testAggStore, ma_store_pred_list, devisor = store_devisors)
ma_store_RMSSE_v

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

################ 4) MA #################
week_ts_list = lapply(trainAggWeek[,2:ncol(trainAggWeek)], ts)
ma_week_list = lapply(week_ts_list, sma)
ma_week_pred_list = lapply(ma_week_list, forecast.beta, h = 4)
week_devisors = unlist(lapply(trainAggWeek[,2:ncol(trainAggWeek)], cal.devisor))
ma_week_RMSSE_v = eval.RMSSE(testAggWeek, ma_week_pred_list, h = 4, devisor = week_devisors)
ma_week_RMSSE_v


####
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
     file='C:/Users/69560/Desktop/forecasting_R/scaled_naive.Rdata')

save(naive_list,snaive_list,ma_list,
     ma_cat_list,ma_store_list,ma_week_list,
     file='C:/Users/69560/Desktop/forecasting_R/scaled_ma_model_list.Rdata')
