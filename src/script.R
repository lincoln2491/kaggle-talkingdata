library(data.table)
library(bit64)
library(caret)
source("src/preprocessing_functions.R")
source("src/classification.R")
library(rworldmap)

gender_age_train = load_data()
gender_age_test = load_data(is_train = FALSE)





# train = create_set(gender_age_train)
# data_partition = createDataPartition(gender_age_train$group, p = 0.8, list = FALSE, times = 1)
# train_set = train[data_partition] 
# test_set = train[-data_partition]
# 
# mll = grid_search(train_set, test_set)
