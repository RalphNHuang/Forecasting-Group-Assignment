library(tidyverse)
library(vars)

# How many lag we should use
VARselect(trainAggStore[,2:ncol(trainAggStore)], lag.max = 50, type = "none")$selection
VARselect(trainAggCat[,2:ncol(trainAggCat)], lag.max = 50, type = "none")$selection
VARselect(trainData[,2:ncol(trainData)], lag.max = 50, type = "none")$selection

# ----------------------------------------------------------------------------
# In conclusion, 14 would be a good lag to use
# which is both short and able to cover weekly seasonality
# ----------------------------------------------------------------------------


# Modeling for each stores

store_1 = c(2:7)
store_2 = c(8:13)
store_3 = c(14:19)

VARselect(trainData[,store_1], lag.max = 50, type = "none")$selection
VARselect(trainData[,store_2], lag.max = 50, type = "none")$selection
VARselect(trainData[,store_3], lag.max = 50, type = "none")$selection

var_store_1 = VAR(trainData[,store_1], type = "none", p = 7)
plot(stability(var_store_1, type = "RE"))
summary(var_store_1)

var_store_2 = VAR(trainData[,store_2], type = "none", p = 7)
plot(stability(var_store_2, type = "OLS-CUSUM"))
summary(var_store_2)

var_store_3 = VAR(trainData[,store_3], type = "none", p = 7)
plot(stability(var_store_3, type = "Rec-MOSUM"))
summary(var_store_3)

var_store_1_pred = predict(var_store_1, n.ahead = 1, ci = 0.95)
var_store_2_pred = predict(var_store_2, n.ahead = 1, ci = 0.95)
var_store_3_pred = predict(var_store_3, n.ahead = 1, ci = 0.95)

# Modeling for each category

cat_1 = c(2, 8, 14)
cat_2 = c(3, 4, 9,10, 15, 16)
cat_3 = c(5:7, 11:13, 17:19)
VARselect(trainData[,cat_1], lag.max = 50, type = "none")$selection
VARselect(trainData[,cat_2], lag.max = 50, type = "none")$selection
VARselect(trainData[,cat_3], lag.max = 50, type = "none")$selection

var_cat_1 = VAR(trainData[,cat_1], type = "none", p = 7)
plot(stability(var_cat_1, type = "Rec-MOSUM"))
summary(var_cat_1)

var_cat_2 = VAR(trainData[,cat_2], type = "none", p = 7)
plot(stability(var_cat_2, type = "RE"))
summary(var_cat_2)

var_cat_3 = VAR(trainData[,cat_3], type = "none", p = 7)
plot(stability(var_cat_3, type = "OLS-CUSUM"))
summary(var_cat_3)

var_cat_1_pred = predict(var_cat_1, n.ahead = 1, ci = 0.95)
var_cat_2_pred = predict(var_cat_2, n.ahead = 1, ci = 0.95)
var_cat_3_pred = predict(var_cat_3, n.ahead = 1, ci = 0.95)


# Model for all

var_all = VAR(trainData[,2:ncol(trainData)],
              type = "none", p = 7)
plot(stability(var_all, type = "OLS-CUSUM"))
summary(var_all)

var_all_pred = predict(var_all, n.ahead = 1, ci = 0.95)

