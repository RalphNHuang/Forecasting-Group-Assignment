#重要！导入所有函数，需要tidyverse包
source("./R/utils.R")
source("./R/dataUtils.R")
load("./RData/dataset.RData")


# ### Dataset splition
# rawData = read_csv("./dat/Projectdata.csv")
# rawData[,2:ncol(rawData)] = apply(rawData[,2:ncol(rawData)], 2, scale)
# trainData = rawData[c(1:(nrow(rawData)-28)),]
# testData = rawData[c((nrow(rawData)-27):nrow(rawData)),]
# 
# ### Aggregation
# 
# # by categories
# trainAggCat = agg_by_cat(trainData)
# testAggCat = agg_by_cat(testData)
# 
# 
# # by week
# trainAggWeek = agg_by_week(trainData)
# testAggWeek = agg_by_week(testData)
# 
# 
# # by store
# trainAggStore = agg_by_store(trainData)
# testAggStore = agg_by_store(testData)
# 
# save(rawData, testAggCat, testAggStore, testAggWeek, testData,
#      trainAggCat, trainAggStore, trainAggWeek, trainData,
#      file = "./RData/dataset.RData")


### experiment with one column

#hobby_ts_1 = msts(trainData$Hobbies_CA_1, start = 1, seasonal.periods = c(7, 30.4, 365.25))

#plot(hobby_ts_1)

#tbats_hobby_1 = tbats(hobby_ts_1)

#tbats_hobby_1_pred = forecast(tbats_hobby_1, level = c(95), h = 28)

#plot(tbats_hobby_1_pred)

#tbats_hobby_1_pred$mean

###############################################################################
### Do it with 18 columns

# ————————————————————————————————————————————————————————————————————————————
# lapply(input_list, function)的功能是对list里的每一个元素执行function的操作
# 其输出为output_list，其中的每一个元素为input_list中对应元素操作后的结果
# e.g. 构建time series类型的变量
# 原操作：time_series = ts(column, ...)
# lapply操作: time_series_list = lapply(dataframe, ts)
# 在这个语句中，dataframe的每一列都会被传到ts()的第一个参数
# 而如果我们想要自定义别的参数，梗稳妥的办法是自定义一个函数，规定好其他参数
# 这个自定义的函数如下被命名为msts.beta()
# 大家可以根据自己需要的seasonal.period的个数来选择用msts()还是ts()
# 而后，我们就可以得到ts_list，每一个元素都是一个时间序列类型的变量
# ___________________________________________________________________________


ts_list = lapply(trainData[,2:ncol(rawData)], msts.beta)

# ---------------------------------------------------------------------------
# 对于我的tbats模型，没有什么其他参数需要自定义
# 如果大家的模型有别的参数要传，也可以按照我function.beta()来自定义
# ---------------------------------------------------------------------------

tbats_list = lapply(ts_list, tbats)


### same as the predicting process
tbats_pred_list = lapply(tbats_list, forecast.beta)

### calculate the devisors for RMSSE
devisors = unlist(lapply(trainData[,2:ncol(trainData)], cal.devisor))

### calculate the RMSSE for all columns (h =28)
tbats_RMSSE_v = eval.RMSSE(testData, tbats_pred_list, devisor = devisors)

### calcualte the MES for both train set and test set

tbats_RMSE_train_v = eval.RMSE(trainData, tbats_pred_list, mode = "train")
tbats_RMSE_test_v = eval.RMSE(testData, tbats_pred_list, mode = "test")




# 1.3 evaluate different h

tbats_crossH_RMSSE = data.frame()
for (h in 1:28) {
  pred_list = lapply(tbats_list, forecast.beta, h = h)
  tbats_RMSSE_v = eval.RMSSE(testData, pred_list, h = h, devisor = devisors)
  tbats_crossH_RMSSE = rbind(tbats_crossH_RMSSE, tbats_RMSSE_v)
}
names(tbats_crossH_RMSSE) = names(tbats_RMSSE_v)

# 1.4 aggregate by item

cat_ts_list = lapply(trainAggCat[,2:ncol(trainAggCat)], msts.beta)
cat_tbats_list = lapply(cat_ts_list, tbats)
cat_tbats_pred_list = lapply(cat_tbats_list, forecast.beta)
cat_devisors = unlist(lapply(trainAggCat[,2:ncol(trainAggCat)], cal.devisor))
cat_tbats_RMSSE_v = eval.RMSSE(testAggCat, cat_tbats_pred_list, devisor = cat_devisors)

# 1.4 aggregate by store

store_ts_list = lapply(trainAggStore[,2:ncol(trainAggStore)], msts.beta)
store_tbats_list = lapply(store_ts_list, tbats)
store_tbats_pred_list = lapply(store_tbats_list, forecast.beta)
store_devisors = unlist(lapply(trainAggStore[,2:ncol(trainAggStore)], cal.devisor))
store_tbats_RMSSE_v = eval.RMSSE(testAggStore, store_tbats_pred_list, devisor = store_devisors)

# 1.5 aggregate by week

week_ts_list = lapply(trainAggWeek[,2:ncol(trainAggWeek)], msts.beta)
week_tbats_list = lapply(week_ts_list, tbats)
week_tbats_pred_list = lapply(week_tbats_list, forecast.beta, h = 4)
week_devisors = unlist(lapply(trainAggWeek[,2:ncol(trainAggWeek)], cal.devisor))
week_tbats_RMSSE_v = eval.RMSSE(testAggWeek, week_tbats_pred_list, 
                                h = 4, devisor = week_devisors)

save(ts_list, tbats_list, tbats_pred_list, 
     tbats_RMSSE_v, tbats_RMSE_train_v, tbats_RMSE_test_v,
     tbats_crossH_RMSSE, 
     cat_ts_list, cat_tbats_list, cat_tbats_pred_list, cat_tbats_RMSSE_v,
     store_ts_list, store_tbats_list, store_tbats_pred_list, store_tbats_RMSSE_v, 
     week_ts_list, week_tbats_list, week_tbats_pred_list, week_tbats_RMSSE_v,
     file = "./RData/tbats.RData")
