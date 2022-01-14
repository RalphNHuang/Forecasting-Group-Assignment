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

# Modeling

var_store = VAR(trainAggStore[,2:ncol(trainAggStore)],
                type = "none", p = 14)
summary(var_store)

var_cat = VAR(trainAggCat[,2:ncol(trainAggCat)],
                type = "none", p = 14)
summary(var_cat)

var_all = VAR(trainData[,2:ncol(trainData)],
              type = "none", p = 7)
summary(var_all)
