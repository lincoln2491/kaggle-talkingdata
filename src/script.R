library(data.table)
library(bit64)
library(caret)
source("src/preprocessing_functions.R")
source("src/classification.R")
library(rworldmap)

phone_brand_device_model = fread("data/English_phone_brand_device_model.csv")
phone_brand_device_model = phone_brand_device_model[duplicated(phone_brand_device_model, by = "device_id") == FALSE]
phone_brand_device_model$phone_brand = as.factor(phone_brand_device_model$phone_brand)
phone_brand_device_model$device_model = as.factor(phone_brand_device_model$device_model)
phone_brand_device_model$device_id = as.character(phone_brand_device_model$device_id)

events =  fread("data/events.csv")
events$device_id = as.character(events$device_id)
events = split_timestamp(events)
events[(latitude < 10) & (latitude > -10) & (longitude > -10) & (longitude < 10), longitude := NA]
events[(latitude < 10) & (latitude > -10), latitude := NA]

events_count_table = table(events$device_id)
events_count_table = as.data.table(events_count_table)
colnames(events_count_table) = c("device_id", "count")

gender_age_train = fread("data/gender_age_train.csv")
gender_age_train$device_id = as.character(gender_age_train$device_id)
gender_age_train = merge(x = gender_age_train, y = phone_brand_device_model,
						 by = "device_id", all.x = TRUE)
rownames(gender_age_train) = gender_age_train$device_id

gender_age_train = merge(gender_age_train, events_count_table, 
						 by = "device_id", all.x = TRUE)
geo_statistics = get_geo_statistics(events)
gender_age_train = merge(gender_age_train, geo_statistics, by = "device_id", 
						 all.x = TRUE)
# gender_age_train = 


gender_age_test =  fread("data/gender_age_test.csv")
gender_age_devices_test = merge(x = gender_age_test, y = phone_brand_device_model, 
								by = "device_id", all.x = TRUE)
rownames(gender_age_devices_test) = gender_age_devices_test$device_id

train = create_set(gender_age_devices_train)
data_partition = createDataPartition(gender_age_devices_train$group, p = 0.8, list = FALSE, times = 1)
train_set = train[data_partition] 
test_set = train[-data_partition]

mll = grid_search(train_set, test_set)
