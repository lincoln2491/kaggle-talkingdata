library(data.table)
library(bit64)
library(caret)
source("src/preprocessing_functions.R")
source("src/classification.R")


phone_brand_device_model = fread("data/English_phone_brand_device_model.csv")
phone_brand_device_model = phone_brand_device_model[duplicated(phone_brand_device_model, by = "device_id") == FALSE]
phone_brand_device_model$phone_brand = as.factor(phone_brand_device_model$phone_brand)
phone_brand_device_model$device_model = as.factor(phone_brand_device_model$device_model)


gender_age_train = fread("data/gender_age_train.csv")
gender_age_devices_train = merge(x = gender_age_train, y = phone_brand_device_model,
								 by = "device_id", all.x = TRUE)
rownames(gender_age_devices_train) = gender_age_devices_train$device_id

gender_age_test =  fread("data/gender_age_test.csv")
gender_age_devices_test = merge(x = gender_age_test, y = phone_brand_device_model, 
								by = "device_id", all.x = TRUE)
rownames(gender_age_devices_test) = gender_age_devices_test$device_id

train = create_set(gender_age_devices_train)
data_partition = createDataPartition(gender_age_devices_train$group, p = 0.8, list = FALSE, times = 1)
train_set = train[data_partition] 
test_set = train[-data_partition]

mll = grid_search(train_set, test_set)

# test = create_set(gender_age_devices_test)
# 
# model = create_model(train)
# 
# predictions = predict_model(model, test)

# app_events = fread("data/app_events.csv")
# app_labels = fread("data/app_labels.csv")
# events = fread("data/events.csv")
# gender_age_test = fread("data/gender_age_test.csv")
# gender_age_train = fread("data/gender_age_train.csv")
# label_categories = fread("data/label_categories.csv")
# phone_brand_device_model = fread("data/phone_brand_device_model.csv")
# sample_submission = fread("data/sample_submission.csv")

# ids_train = unique(gender_age_train$device_id)
# ids_test = unique(gender_age_test$device_id)


# events_train = events[device_id %in% ids_train]
# events_test = events[device_id %in% ids_test]
# rm(events)
# gc()
# 
# phone_brand_device_model = phone_brand_device_model[duplicated(phone_brand_device_model, by = NULL) == FALSE]
# app_labels = app_labels[duplicated(app_labels, by = NULL) == FALSE]
# gc()
# 
# train = merge_data(gender_age_train, phone_brand_device_model, app_labels, label_categories, app_events, events_train)
