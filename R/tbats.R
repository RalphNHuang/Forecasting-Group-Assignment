library(forecast)
library(tidyverse)

### Dataset splition
trainData = rawData[c(1:(nrow(rawData)-28)),]
testData = rawData[c((nrow(rawData)-27):nrow(rawData)),]

### experiment with one column

hobby_ts_1 = msts(trainData$Hobbies_CA_1, start = 1, seasonal.periods = c(7,30.4, 365.25))

plot(hobby_ts_1)

tbats_hobby_1 = tbats(hobby_ts_1)

tbats_hobby_1_pred = forecast(tbats_hobby_1, level = c(95), h = 28)

plot(tbats_hobby_1_pred)

tbats_hobby_1_pred$mean

## 输入:
## ts_vector: 训练集数据
cal.devisor = function(ts_vector){
  tmp = ts_vector[2:length(ts_vector)]-ts_vector[1:(length(ts_vector)-1)]
  output = sum(tmp^2)/(length(ts_vector)-1)
  return(output)
}

## 输入：
## pred: 测试集预测值
## origin: 测试集真实值
## devisor: RMSSE的分母，由cal.devisor()计算得到
cal.RMSSE = function(pred, origin, devisor){
  if(length(pred) != length(origin)){
    cat("lengthError: please keep the length of pred and the length of origin consistent")
  }
  else{
    output = sqrt(sum((pred - origin)^2)/(devisor*length(pred)))
    return(output)
  }
}

cal.RMSSE(tbats_hobby_1_pred$mean, testData$Hobbies_CA_1, cal.devisor(trainData$Hobbies_CA_1))

# 0.6441513