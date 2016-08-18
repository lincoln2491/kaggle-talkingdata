library(data.table)
library(bit64)
library(caret)
source("src/preprocessing_functions.R")
source("src/classification.R")
source("src/params.R")
library(rworldmap)

gender_age_train = load_data()
gender_age_test = load_data(is_train = FALSE)


gender_age_train = rename_column_names(gender_age_train)
gender_age_test = rename_column_names(gender_age_test)



# gender_age_train[is.na(gender_age_train)] = 0
train = create_set(gender_age_train)
test = create_set(gender_age_test)
# 
previous_na_action = options('na.action')
options(na.action='na.pass')
set.seed(6)
model = create_model(train, model_2_params())
predictions = predict_model(model, test)
colnames(predictions) = sort(unique(gender_age_train$group))
predictions$device_id = gender_age_test$device_id
predictions = predictions[, c(13, 1:12), with = FALSE]
write.csv(predictions, file = "data/out.csv", row.names=FALSE, quote = FALSE)
# data_partition = createDataPartition(gender_age_train$group, p = 0.8, list = FALSE, times = 1)
# train_set = train[data_partition]
# test_set = train[-data_partition]
# mll = grid_search(train_set, test_set)
