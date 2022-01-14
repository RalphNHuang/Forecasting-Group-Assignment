library(tidyverse)

# function for data aggregation by categories
## 输入：rawData, trainData, testData
agg_by_cat = function(data){
  data = data %>% 
    pivot_longer(cols = colnames(data)[2:ncol(data)],
                 names_to = "category", values_to = "sales")
  data$category = str_sub(data$category, 1, 3)
  mapping = data.frame(category = c("Hob", "Foo", "Hou"),
                       mapCat = c("Hobbies", "Foods", "Household"))
  data = data %>% 
    left_join(mapping) %>% 
    select(date, mapCat, sales) %>% 
    group_by(date, mapCat) %>% 
    summarise(sales = sum(sales)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = mapCat, values_from = sales) %>% 
    arrange(date) %>% 
    select(date, Hobbies, Foods, Household)
  return(data)
}

# function for data aggregation by week
## 输入：rawData, trainData, testData
### 注：由于1969天不能整除7，为了保持数据的完整性，从周一开始（即剔除第一天和第二天）
agg_by_week = function(data){
  s = nrow(data)%%7
  d = (nrow(data)-s)/7
  col = colnames(data)[2:ncol(data)]
  data = data[(1+s):nrow(data),] %>% 
    mutate(week = rep(paste0("w_",c(1:d)), each = 7)) %>% 
    select(!"date") %>% 
    group_by(week) %>% 
    summarise(across(where(is.numeric), sum, na.rm = T)) %>% 
    ungroup() 
  data$week = data$week %>% 
    str_remove_all("w_") %>% 
    as.numeric()
  data = data %>% 
    arrange(week)
  return(data)
}

agg_by_store = function(data){
  data = data %>% 
    pivot_longer(cols = colnames(data)[2:ncol(data)],
                 names_to = "category", values_to = "sales")
  data$category = str_sub(data$category, -4, -1)
  data = data %>% 
    #left_join(mapping) %>% 
    select(date, category, sales) %>% 
    group_by(date, category) %>% 
    summarise(sales = sum(sales)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = category, values_from = sales) %>% 
    arrange(date)# %>% 
    #select(date, Hobbies, Foods, Household)
}