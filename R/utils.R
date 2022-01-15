library(forecast)
library(tidyverse)


# function for time series customization
# customize `seasonal.periods`
msts.beta = function(column){
  output = msts(column, start = 1, seasonal.periods = c(7, 30.4, 365.25))
  return(output)
}

# function for forecast customization
# customize `h`, which is rhe length of prediction
forecast.beta = function(model, h = 28){
  pred = forecast(model, level = c(95), h = h)
  return(pred)
}

# function for calculating the devisor of RMSSE
## 输入:
## ts_vector: 训练集数据的一列
cal.devisor = function(ts_vector){
  tmp = ts_vector[2:length(ts_vector)]-ts_vector[1:(length(ts_vector)-1)]
  output = sum(tmp^2)/(length(ts_vector)-1)
  return(output)
}

# function for calculating RMSSE
## 输入：
## pred: 测试集预测值
## origin: 测试集真实值
## devisor: RMSSE的分母，由cal.devisor()计算得到
cal.RMSSE = function(pred, origin, devisor){
  if(length(pred) != length(origin)){
    cat("lengthError: please keep the length of pred and the length of origin consistent\n")
  }
  else{
    output = sqrt(sum((pred - origin)^2)/(devisor*length(pred)))
    return(output)
  }
}

# functions for calculating RMSSE for all the columns
## 输入：
## testData：之前划分的数据集（包含日期列，共19列）
## pred_list：lapply方法生成的包含所有列对应模型的预测结果
### 如果为每一列自定义模型，则需要手动构建pred_list
### 注意，list内元素的名称和顺序需要与原数据的列名对应
### pred_list = list(
###   Hobbies_CA_1 = forecast(model1, h = h),
###   Household_1_CA_1 = forecast(model2, h = h),
###   ...
### )
## devisiors：训练集每一列算出来的分母，同样可以使用lapply函数得到

eval.RMSSE = function(testData, pred_list, h = 28, devisors){
  testData = testData[1:h,2:ncol(testData)]
  if(names(testData)!=names(pred_list))
    cat("Please rearrange the pred_list\n")
  else{
    output = numeric()
    for (i in 1:length(pred_list)) {
      pred = pred_list[[i]][["mean"]]
      origin = pull(testData, i)
      output = c(output, cal.RMSSE(pred, origin, devisors[i]))
    }
    return(output)
  }
}


# function to calculate RMSE
## 输入：
## pred：预测值
## origin：实际值
cal.RMSE = function(pred, origin){
  rmse = sqrt(mean((pred-origin)^2, rm.na = T))
  return(rmse)
}


# function to calculate MSE for all columns

eval.RMSE = function(testData, pred_list, mode = "train"){
  testData = testData[,2:ncol(testData)]
  if(names(testData)!=names(pred_list))
    cat("Please rearrange the pred_list\n")
  else{
    if(mode=="train"){
      output = numeric()
      for (i in 1:length(pred_list)) {
        pred = pred_list[[i]][["fitted"]]
        origin = pull(testData, i)
        output = c(output, cal.RMSE(pred, origin))
      }
      return(output)
    }
    else if(mode=="test"){
      output = numeric()
      for (i in 1:length(pred_list)) {
        pred = pred_list[[i]][["mean"]]
        origin = pull(testData, i)
        output = c(output, cal.RMSE(pred, origin))
      }
      return(output)
    }
    else
      cat("please check the mode\n")
  }
}
