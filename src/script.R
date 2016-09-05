library(data.table)
library(bit64)
library(caret)
source("src/preprocessing_functions.R")
source("src/classification.R")
source("src/params.R")
source("src/plots.R")
# library(rworldmap)

gender_age_train = load_data()
gender_age_train = get_app_statistics(gender_age_train)
gender_age_test = load_data(is_train = FALSE)
gender_age_test = get_app_statistics(gender_age_test)

test1 = get_app_statistics(test1)
write.csv(test1, "data/test1.csv")
rm(test1)
gc()
test2 = get_app_statistics(test2)
write.csv(test2, "data/test2.csv")
rm(test2)
gc()

gender_age_train = rename_column_names(gender_age_train)
gender_age_test = rename_column_names(gender_age_test)



# gender_age_train[is.na(gender_age_train)] = 0
train = create_set(gender_age_train)
test = create_set(gender_age_test)
# 
previous_na_action = options('na.action')
options(na.action='na.pass')
set.seed(6)
gc()
model = create_model(train, model_3_params())
predictions = predict_model(model, test)
colnames(predictions) = sort(unique(gender_age_train$group))
predictions$device_id = gender_age_test$device_id
predictions = predictions[, c(13, 1:12), with = FALSE]
write.csv(predictions, file = "data/out.csv", row.names=FALSE, quote = FALSE)
gc()
# data_partition = createDataPartition(gender_age_train$group, p = 0.8, list = FALSE, times = 1)
# train_set = train[data_partition]
# test_set = train[-data_partition]
# rm(gender_age_train, train)
# gc()
# mll = grid_search(train_set, test_set)
